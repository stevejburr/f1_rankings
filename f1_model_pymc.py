"""
F1 Driver Rankings – Bayesian Hierarchical Model (PyMC)
========================================================

PURPOSE
-------
Replicate and improve upon the brms/Stan model in analysis_and_modelling_2026.R
using PyMC.  The goal is identical: estimate latent driver ability across
different eras while controlling for car quality.  The R model is kept as the
reference; every modelling choice below is compared against it.

METHODOLOGICAL COMPARISON
--------------------------

1. RESPONSE VARIABLE
   R model   : total_points ~ Poisson(mu)
   Here      : total_points ~ NegativeBinomial(mu, alpha_nb)

   Motivation: the marginal variance/mean of total_points is 4.35, far above
   the Poisson constraint of 1.0.  The distribution is bimodal – 38 % zeros
   (DNFs and finishes outside the top-10) plus a roughly flat mass over 1–11
   for points-scorers.  NegativeBinomial adds a free dispersion parameter
   alpha_nb (large alpha_nb → Poisson).  This is strictly more expressive and
   should fit residual patterns the Poisson model attributes to random effects.

   Alternative (commented at end): hurdle-NegativeBinomial, which separately
   models the probability of scoring at all and the conditional distribution
   given scoring.  This would capture the bimodality more explicitly.

2. CAREER TRAJECTORY
   R model   : poly(career_year, 2) – brms uses orthogonal polynomials,
               scaling and centring internally
   Here      : explicit z-scored career_year with quadratic terms

   The R poly() call centres/scales internally but makes prior calibration
   opaque.  Explicit z-scoring here makes the prior scale interpretable:
   1 unit in career_year_z corresponds to ~3.5 calendar years.  Priors on
   beta_1, beta_2 can therefore be set with direct reference to the outcome
   scale.

   Alternative (commented at end): replace the polynomial with a Gaussian
   Process over career year (GaussianRandomWalk or pm.gp.Latent).  GPs give
   a non-parametric, flexible career shape that does not extrapolate with
   polynomial behaviour and does not force a symmetric quadratic peak.

3. DRIVER RANDOM EFFECTS: NON-CENTERED PARAMETERISATION
   R model   : (1 + poly(career_year, 2) | Driver) – centered, brms defaults
   Here      : z_driver ~ Normal(0,1); effects = z_driver @ chol.T

   In the centered form (brms default), the driver-level parameters are drawn
   directly from the population distribution:
       driver_intercept[d] ~ Normal(0, sigma_intercept)
   When sigma_intercept is small (drivers are similar), NUTS sees a narrow
   funnel: the constrained likelihood forces driver parameters to cluster near
   zero, but the prior allows wide exploration.  This geometry causes
   divergences and poor mixing.

   The non-centered (Matt's trick) form separates shape from scale:
       z_driver[d] ~ Normal(0, 1)         [always well-conditioned]
       driver_effects[d] = z_driver[d] @ chol.T
   NUTS explores z_driver freely; the scale is absorbed into chol.  This
   eliminates the funnel and typically gives far lower divergence counts and
   higher effective sample sizes.

   LKJCholeskyCov with eta=2 provides a weakly-informative prior on the
   3×3 correlation matrix (intercept, linear slope, quadratic), mildly
   preferring correlations near zero.

4. CAR / TEAM EFFECTS: HIERARCHICAL POOLING ACROSS SEASONS
   R model   : (1 | Car:year_chr) – fully independent per (car, season)
   Here      : car_year_effect[cy] = team_baseline[team] + team_year_delta[cy]

   The R model treats each (constructor, year) pair as an independent random
   effect.  This ignores the strong prior knowledge that a team's pace in year
   t is informative about year t+1.  With independent effects, a team with
   only 3 races in a year gets near-zero shrinkage toward any sensible default.

   The two-level hierarchy here pools within-team across years:
     team_baseline[t] : team t's average competitiveness across all seasons
     team_year_delta[cy]: how much that specific car-year deviated

   A team with a short season borrows from team_baseline; a full-season team
   pins team_year_delta precisely.

   Alternative (commented at end): replace team_year_delta with a
   GaussianRandomWalk over calendar year per team.  This captures gradual
   performance arcs (e.g. Red Bull's rise 2019–2022) rather than treating
   each year as an independent fluctuation.

5. PRIORS
   R model   : brms defaults – HalfStudent-t(3, 0, 2.5) on random-effect SDs,
               flat on population betas
   Here      : domain-calibrated weakly informative priors (see model section)

   Flat priors on regression coefficients are weakly regularising on the
   log scale but can cause pathological behaviour when multiplied by the
   exponential link.  Domain knowledge is used to set sensible scales:
   – Global intercept: e^1 ≈ 2.7 pts/race is a plausible all-driver average
   – Driver SDs: HalfNormal(0.5) allows a ~2× difference at ±2 sd, plausible
     between top and back-of-grid drivers

DEPENDENCIES
------------
    pip install pymc arviz pandas numpy matplotlib

Optional (5–10× faster sampling):
    pip install numpyro jax
"""

# =============================================================================
# IMPORTS
# =============================================================================

import warnings
import numpy as np
import pandas as pd
import pymc as pm
import pytensor.tensor as pt
import arviz as az
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from pathlib import Path

warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)


# =============================================================================
# DATA LOADING AND PREPROCESSING
# =============================================================================

DATA_FILE = "all_f1_race_results_14_06_2026.csv"

df = pd.read_csv(DATA_FILE)

# --- Harmonise columns that changed format in the 2023+ scraped data ---------
# Pre-2023: Car, Time/Retired, PTS columns are populated
# 2023+:    Team, "Time / Retired", Pts columns are populated instead
# combine_first() fills NaN values in the left series from the right series.
df["Car"] = df["Car"].combine_first(df["Team"])
df["Time/Retired"] = df["Time/Retired"].combine_first(df["Time / Retired"])
df["PTS"] = (
    pd.to_numeric(df["PTS"], errors="coerce")
    .combine_first(pd.to_numeric(df["Pts"], errors="coerce"))
)

# Drop rows still missing Car or Driver (data quality guard)
df = df.dropna(subset=["Car", "Driver", "year"])

# --- Finished flag -----------------------------------------------------------
# A driver counts as "finished" unless they have a retirement code or
# a non-classified / disqualified position.
# Note: 2023+ data uses "DQ" in Pos rather than "DSQ" in Time/Retired.
_DNF_CODES = {"DNF", "DSQ", "DNC", "DNS"}
_NC_CODES   = {"NC", "DQ"}
df["finished"] = np.where(
    df["Time/Retired"].isin(_DNF_CODES) | df["Pos"].isin(_NC_CODES),
    0, 1
)

# --- Position points ---------------------------------------------------------
# Scoring system (matches R script):
#   1st → 10 pts, 2nd → 9, …, 10th → 1, outside top-10 → 0
#   +1 for finishing the race
# This gives total_points ∈ {0, 1, 2, …, 11}
pos_num = pd.to_numeric(df["Pos"], errors="coerce")
df["pos_points"] = np.where(
    (pos_num >= 1) & (pos_num <= 10), 11.0 - pos_num, 0.0
)
df["pos_points"] = df["pos_points"].fillna(0.0)
df["total_points"] = (df["finished"] + df["pos_points"]).astype(int)

# --- Career year -------------------------------------------------------------
first_year = df.groupby("Driver")["year"].min().rename("first_year")
df = df.merge(first_year, on="Driver")
df["career_year"] = 1 + (df["year"] - df["first_year"])

# Z-score career year for prior calibration.
# On this scale, 1 unit ≈ cy_sd calendar years – priors on betas are
# interpretable without knowing the raw range.
cy_mean = df["career_year"].mean()
cy_sd   = df["career_year"].std()
df["career_year_z"] = (df["career_year"] - cy_mean) / cy_sd

print(f"career_year z-scored: mean={cy_mean:.1f} yrs, sd={cy_sd:.1f} yrs")
print(f"(1 unit of career_year_z ≈ {cy_sd:.1f} calendar years)")

# --- Team name: strip engine supplier from Car string -------------------------
# Constructors are listed as e.g. "McLaren Mercedes", "Red Bull Racing RBPT".
# Strip the engine supplier so "McLaren" is recognised as the same team across
# different engine eras.  This lets us pool information across seasons for the
# same constructor when estimating team_baseline.
_ENGINE_KW = {
    "Mercedes", "Ferrari", "Honda", "RBPT", "Renault", "TAG", "Ford",
    "Cosworth", "Porsche", "BMW", "Judd", "Mugen", "Ilmor", "Supertec",
    "Peugeot", "Climax", "BRM", "Weslake", "Repco", "Maserati", "Aramco",
    "Chevrolet", "Hart", "Lamborghini", "Yamaha", "Zytek",
}

def _strip_engine(car: str) -> str:
    parts = car.split()
    team_parts = [p for p in parts if p not in _ENGINE_KW]
    return " ".join(team_parts) if team_parts else car

df["team"] = df["Car"].apply(_strip_engine)

# --- Integer indices for PyMC ------------------------------------------------
drivers_cat  = pd.Categorical(df["Driver"])
car_year_cat = pd.Categorical(df["Car"] + "_" + df["year"].astype(str))
teams_cat    = pd.Categorical(df["team"])

df["driver_idx"]  = drivers_cat.codes
df["car_year_idx"] = car_year_cat.codes
df["team_idx"]    = teams_cat.codes

n_obs      = len(df)
n_drivers  = len(drivers_cat.categories)
n_car_years = len(car_year_cat.categories)
n_teams    = len(teams_cat.categories)

# car_year_to_team[cy] = team index for car-year cy.
# Used so each car-year inherits its team's baseline.
cy_team_df = (
    df[["car_year_idx", "team_idx"]]
    .drop_duplicates()
    .sort_values("car_year_idx")
)
assert len(cy_team_df) == n_car_years, "Each car-year must map to exactly one team"
car_year_to_team = cy_team_df["team_idx"].values  # shape (n_car_years,)

print(f"\nDataset: {n_obs:,} observations | "
      f"{n_drivers} drivers | {n_car_years} car-years | {n_teams} teams")

# Marginal overdispersion (justifies NegativeBinomial over Poisson)
m = df["total_points"].mean()
v = df["total_points"].var()
print(f"total_points: mean={m:.3f}, variance={v:.3f}, "
      f"dispersion (var/mean)={v/m:.2f}  [Poisson requires 1.0]")

# --- Arrays passed into PyMC -------------------------------------------------
y_obs_arr    = df["total_points"].values
driver_idx_arr   = df["driver_idx"].values
car_year_idx_arr = df["car_year_idx"].values
cy_z_arr     = df["career_year_z"].values

# Coordinate labels for arviz
driver_names  = list(drivers_cat.categories)
car_year_labels = list(car_year_cat.categories)
team_names    = list(teams_cat.categories)

# Filter list for driver ability plots (same ≥25 race threshold as R script)
driver_race_counts = df.groupby("Driver").size()
drivers_25 = driver_race_counts[driver_race_counts >= 25].index.tolist()


# =============================================================================
# PyMC MODEL
# =============================================================================

with pm.Model(coords={
    "driver":        driver_names,
    "car_year":      car_year_labels,
    "team":          team_names,
    "driver_effect": ["intercept", "linear", "quadratic"],
}) as f1_model:

    # =========================================================================
    # POPULATION-LEVEL (FIXED) EFFECTS
    # =========================================================================

    # Global log-mean points per race across all drivers and cars.
    # Prior reasoning: typical race average ≈ 3 pts → log(3) ≈ 1.1.
    # Normal(1, 0.5) spans roughly [0.1, 5] pts/race at ±2 sd – wide enough
    # to cover the range from back-of-grid in 1950 to dominant 2023 Red Bull.
    alpha = pm.Normal("alpha", mu=1.0, sigma=0.5)

    # Population-level career trajectory (quadratic in z-scored career year).
    #
    # beta_1 (linear): no strong prior direction – weakly centred at 0.
    #   On the z-scale, 1 unit ≈ 3.5 yrs.  Normal(0, 0.3) allows ±~30 %
    #   change in expected points per 3.5 years – plausible but not forcing.
    #
    # beta_2 (quadratic): mild negative prior.  We expect the average driver
    #   to peak somewhere in mid-career rather than at the start or end.
    #   Normal(-0.2, 0.2) allows the peak to shift substantially while gently
    #   penalising careers that improve indefinitely.
    beta_1 = pm.Normal("beta_1", mu=0.0, sigma=0.3)
    beta_2 = pm.Normal("beta_2", mu=-0.2, sigma=0.2)

    # =========================================================================
    # DRIVER RANDOM EFFECTS – non-centered, correlated
    # =========================================================================
    #
    # Each driver d has three deviation parameters from the population:
    #   driver_effects[d, 0]: ability intercept  (overall talent level)
    #   driver_effects[d, 1]: career slope deviation
    #   driver_effects[d, 2]: career curvature deviation
    #
    # These three dimensions are allowed to covary via a Cholesky-decomposed
    # covariance matrix.  The LKJ prior with eta=2 weakly prefers small
    # off-diagonal correlations but allows substantial correlation, for example
    # between raw talent (intercept) and how quickly a driver reaches peak form.
    #
    # NON-CENTERED PARAMETERISATION (crucial for HMC efficiency):
    #   z_driver[d, k] ~ Normal(0, 1)              [independent raw effects]
    #   driver_effects   = z_driver @ chol.T       [rotated into correlated space]
    #
    # In the centered form (brms default), driver_intercept[d] ~ Normal(0, σ).
    # When σ is small (drivers similar), NUTS encounters a narrow funnel in the
    # joint (σ, driver_intercept) space and is forced to take tiny step sizes,
    # producing divergences and poor mixing.  The non-centered form decouples
    # shape (z_driver) from scale (chol), making the geometry uniform and easy
    # for NUTS to navigate.

    chol, corr, stds = pm.LKJCholeskyCov(
        "driver_chol",
        n=3,
        eta=2.0,
        sd_dist=pm.HalfNormal.dist(sigma=0.5, shape=3),
        compute_corr=True,
    )

    z_driver = pm.Normal("z_driver", 0.0, 1.0, shape=(n_drivers, 3))

    driver_effects = pm.Deterministic(
        "driver_effects",
        z_driver @ chol.T,          # (n_drivers, 3)
        dims=("driver", "driver_effect"),
    )

    driver_alpha = driver_effects[:, 0]   # ability intercept per driver
    driver_b1    = driver_effects[:, 1]   # linear career slope deviation
    driver_b2    = driver_effects[:, 2]   # quadratic career deviation

    # =========================================================================
    # CAR / TEAM EFFECTS – two-level hierarchy
    # =========================================================================
    #
    # R model: (1 | Car:year_chr) – fully independent per (car, season).
    #
    # Problem: a constructor's pace in year t is strongly correlated with year
    # t+1.  Treating each car-year as independent ignores this and requires
    # much more data per car-year before estimates tighten.
    #
    # Solution: decompose the car-year effect into two layers:
    #
    #   team_baseline[t]  : constructor t's average quality across all seasons
    #   team_year_delta[cy]: how much better/worse car-year cy was vs baseline
    #
    #   car_year_effect[cy] = team_baseline[team(cy)] + team_year_delta[cy]
    #
    # Partial pooling: a constructor with only a handful of races in a season
    # (e.g. a new entrant) has little data to pin team_year_delta[cy], so it
    # falls back on team_baseline – which itself is estimated from all seasons.
    # A full-season team's year delta is estimated largely from its own data.
    #
    # Prior reasoning:
    #   sigma_team      ~ HalfNormal(0.5) : between-team spread on log scale.
    #     ±1 sd ≈ ±50 % in expected points, plausible range across the grid.
    #   sigma_team_year ~ HalfNormal(0.3) : year-to-year fluctuations should be
    #     smaller than the inherent spread between teams.

    sigma_team = pm.HalfNormal("sigma_team", sigma=0.5)
    team_baseline_raw = pm.Normal("team_baseline_raw", 0.0, 1.0, shape=n_teams)
    team_baseline = pm.Deterministic(
        "team_baseline",
        team_baseline_raw * sigma_team,
        dims="team",
    )

    sigma_team_year = pm.HalfNormal("sigma_team_year", sigma=0.3)
    team_year_delta_raw = pm.Normal(
        "team_year_delta_raw", 0.0, 1.0, shape=n_car_years
    )
    team_year_delta = pm.Deterministic(
        "team_year_delta",
        team_year_delta_raw * sigma_team_year,
        dims="car_year",
    )

    # Compose: broadcast team baseline into car-year space, then add deviation.
    # car_year_to_team maps each car-year index to its constructor's team index.
    car_year_effect = team_baseline[car_year_to_team] + team_year_delta

    # =========================================================================
    # LINEAR PREDICTOR
    # =========================================================================
    #
    # log(mu_i) = global intercept
    #           + global career curve (beta_1 * cy_z + beta_2 * cy_z²)
    #           + driver intercept deviation (driver_alpha[d])
    #           + driver-specific career slope (driver_b1[d] * cy_z)
    #           + driver-specific career curvature (driver_b2[d] * cy_z²)
    #           + car-year quality (team_baseline[t] + team_year_delta[cy])

    log_mu = (
        alpha
        + beta_1 * cy_z_arr
        + beta_2 * cy_z_arr ** 2
        + driver_alpha[driver_idx_arr]
        + driver_b1[driver_idx_arr] * cy_z_arr
        + driver_b2[driver_idx_arr] * cy_z_arr ** 2
        + car_year_effect[car_year_idx_arr]
    )

    mu = pm.math.exp(log_mu)

    # =========================================================================
    # LIKELIHOOD – Negative Binomial
    # =========================================================================
    #
    # Variance/mean of the raw data is 4.35, far above the Poisson constraint
    # of 1.0.  After conditioning on driver and car-year effects the residual
    # overdispersion will be smaller but is unlikely to vanish completely:
    # safety-car-aided results, mechanical failures, and weather are genuine
    # sources of extra variability not captured by the latent structure.
    #
    # NegativeBinomial(mu, alpha_nb) parameterisation:
    #   E[y] = mu,   Var[y] = mu + mu² / alpha_nb
    # Large alpha_nb → Poisson (low residual overdispersion).
    # Small alpha_nb → high overdispersion.
    #
    # HalfNormal(2) is wide enough to accommodate the observed marginal
    # overdispersion (implied alpha_nb ≈ 1) while regularising toward lower
    # overdispersion as the model absorbs variance through the random effects.

    alpha_nb = pm.HalfNormal("alpha_nb", sigma=2.0)

    pm.NegativeBinomial("y_obs", mu=mu, alpha=alpha_nb, observed=y_obs_arr)


# =============================================================================
# SAMPLING
# =============================================================================

if __name__ == '__main__':
    print("\nFitting model – this may take 30–90 minutes on large CPU.")
    print("Tip: uncomment nuts_sampler='numpyro' below for ~5–10× speedup.\n")
    
    TRACE_FILE = "f1_model_trace.nc"
    
    with f1_model:
        # target_accept=0.9 uses smaller step sizes than the default 0.8.
        # More conservative but reduces divergences; the non-centered
        # parameterisation should make divergences rare anyway.
        trace = pm.sample(
            draws=1000,
            tune=1000,
            chains=4,
            target_accept=0.9,
            # nuts_sampler="numpyro",  # install: pip install numpyro jax
            random_seed=42,
            idata_kwargs={"log_likelihood": True},  # enables LOO-CV comparison
        )
    
    trace.to_netcdf(TRACE_FILE)
    print(f"\nTrace saved to {TRACE_FILE}")
    # To reload without re-sampling: trace = az.from_netcdf(TRACE_FILE)
    
    
    # =============================================================================
    # DIAGNOSTICS
    # =============================================================================
    
    print("\n--- Population-level parameter summary ---")
    pop_vars = ["alpha", "beta_1", "beta_2", "sigma_team", "sigma_team_year", "alpha_nb"]
    print(az.summary(trace, var_names=pop_vars, round_to=3))
    
    # Divergences: should be 0 with the non-centered parameterisation.
    # Any divergences indicate the posterior geometry is still problematic –
    # try increasing target_accept toward 0.95 or check prior scales.
    n_div = int(trace.sample_stats.diverging.sum())
    print(f"\nDivergences: {n_div}  (target: 0)")
    
    # R-hat: should be < 1.01 for convergence.  ESS > 400 per chain is adequate.
    print("\nMax R-hat across driver effects (should be <1.01):")
    driver_rhat = az.rhat(trace, var_names=["z_driver"])
    print(f"  {float(driver_rhat['z_driver'].max()):.4f}")
    
    print("\nMin bulk ESS across driver effects (want >400 per chain):")
    driver_ess = az.ess(trace, var_names=["z_driver"])
    print(f"  {float(driver_ess['z_driver'].min()):.0f}")
    
    
    # =============================================================================
    # POSTERIOR ANALYSIS
    # =============================================================================
    
    # ---- Posterior samples (flatten chains and draws) ---------------------------
    # driver_effects shape: (chain, draw, n_drivers, 3)
    de_samples = trace.posterior["driver_effects"].values
    n_chains, n_draws = de_samples.shape[:2]
    n_samples = n_chains * n_draws
    de_flat = de_samples.reshape(n_samples, n_drivers, 3)   # (n_samples, n_drivers, 3)
    
    alpha_samples = trace.posterior["alpha"].values.ravel()
    b1_samples    = trace.posterior["beta_1"].values.ravel()
    b2_samples    = trace.posterior["beta_2"].values.ravel()
    
    # ---- Driver ability summary -------------------------------------------------
    # driver_effects[:, :, 0] is the intercept (ability level), marginalised over
    # career year.  Higher = better driver in an average car at a typical
    # career-year point.  Exponentiate to get a multiplier on expected points.
    
    driver_alpha_flat = de_flat[:, :, 0]    # (n_samples, n_drivers)
    
    driver_summary = pd.DataFrame({
        "driver":      driver_names,
        "mean":        driver_alpha_flat.mean(axis=0),
        "sd":          driver_alpha_flat.std(axis=0),
        "q2_5":        np.percentile(driver_alpha_flat, 2.5, axis=0),
        "q97_5":       np.percentile(driver_alpha_flat, 97.5, axis=0),
        "races":       [int(driver_race_counts.get(d, 0)) for d in driver_names],
    }).sort_values("mean", ascending=False)
    
    # Restrict to drivers with ≥25 races (same cutoff as R script)
    driver_summary_qual = driver_summary[driver_summary["races"] >= 25].copy()
    
    print(f"\n--- Top 20 drivers by estimated ability (≥25 races) ---")
    print(driver_summary_qual.head(20).to_string(index=False))
    
    # ---- Career trajectory predictions ------------------------------------------
    # Predict expected points on a grid of career years for a set of focus drivers,
    # holding the car-year effect at the population mean (= 0, i.e. average car).
    #
    # log(mu_d(cy)) = alpha + (beta_1 + driver_b1[d]) * cy_z
    #               + (beta_2 + driver_b2[d]) * cy_z²
    #               + driver_alpha[d]
    #
    # This isolates pure driver quality from car quality.
    
    FOCUS_DRIVERS = [
        "Lewis Hamilton HAM",
        "Michael Schumacher MSC",
        "Max Verstappen VER",
        "Ayrton Senna SEN",
        "Alain Prost PRO",
        "Sebastian Vettel VET",
        "Fernando Alonso ALO",
        "Lando Norris NOR",
    ]
    
    CAREER_GRID = np.arange(1, 16)
    cy_z_grid   = (CAREER_GRID - cy_mean) / cy_sd
    
    traj_rows = []
    for drv in FOCUS_DRIVERS:
        if drv not in driver_names:
            print(f"  Warning: '{drv}' not in data – skipping")
            continue
        d = driver_names.index(drv)
        for cy_val, cy_z_val in zip(CAREER_GRID, cy_z_grid):
            log_mu_s = (
                alpha_samples
                + (b1_samples + de_flat[:, d, 1]) * cy_z_val
                + (b2_samples + de_flat[:, d, 2]) * cy_z_val ** 2
                + de_flat[:, d, 0]
            )
            mu_s = np.exp(log_mu_s)
            traj_rows.append({
                "driver":      drv,
                "career_year": cy_val,
                "mean":        mu_s.mean(),
                "q05":         np.percentile(mu_s, 5),
                "q95":         np.percentile(mu_s, 95),
            })
    
    traj_df = pd.DataFrame(traj_rows)
    
    # ---- Team performance summary (all-time) ------------------------------------
    tb_samples = trace.posterior["team_baseline"].values.reshape(n_samples, n_teams)
    team_summary = pd.DataFrame({
        "team": team_names,
        "mean": tb_samples.mean(axis=0),
        "q2_5": np.percentile(tb_samples, 2.5, axis=0),
        "q97_5": np.percentile(tb_samples, 97.5, axis=0),
    }).sort_values("mean", ascending=False)
    
    print(f"\n--- Top 15 constructors by all-time baseline ---")
    print(team_summary.head(15).to_string(index=False))
    
    
    # =============================================================================
    # VISUALISATIONS
    # =============================================================================
    
    COLOURS = plt.cm.tab10(np.linspace(0, 0.9, len(FOCUS_DRIVERS)))
    
    fig = plt.figure(figsize=(18, 12))
    gs  = gridspec.GridSpec(2, 2, figure=fig, hspace=0.4, wspace=0.35)
    
    # ---- Panel A: driver ability ranking (top 30) -------------------------------
    ax_rank = fig.add_subplot(gs[:, 0])   # spans both rows
    
    top30 = driver_summary_qual.head(30).reset_index(drop=True)
    y_pos = np.arange(len(top30))
    
    ax_rank.barh(y_pos, top30["mean"], color="steelblue", alpha=0.6, height=0.6)
    ax_rank.errorbar(
        top30["mean"], y_pos,
        xerr=[top30["mean"] - top30["q2_5"], top30["q97_5"] - top30["mean"]],
        fmt="none", color="#1a1a2e", linewidth=1.0, capsize=2,
    )
    ax_rank.set_yticks(y_pos)
    ax_rank.set_yticklabels(
        [f"{r.driver}  ({r.races})" for _, r in top30.iterrows()], fontsize=7.5
    )
    ax_rank.axvline(0, color="grey", linewidth=0.8, linestyle="--", alpha=0.7)
    ax_rank.set_xlabel("Driver ability (log-scale deviation from population mean)")
    ax_rank.set_title(
        "Driver ability ranking\n(≥25 races; bar = posterior mean, line = 95% credible interval)",
        fontsize=9
    )
    ax_rank.invert_yaxis()
    
    # ---- Panel B: career trajectory curves --------------------------------------
    ax_traj = fig.add_subplot(gs[0, 1])
    
    for drv, col in zip(FOCUS_DRIVERS, COLOURS):
        sub = traj_df[traj_df["driver"] == drv]
        if sub.empty:
            continue
        label = drv.rsplit(" ", 1)[-1]   # use abbreviation: "HAM", "MSC", etc.
        ax_traj.plot(sub["career_year"], sub["mean"], color=col, label=label, linewidth=1.8)
        ax_traj.fill_between(sub["career_year"], sub["q05"], sub["q95"],
                             alpha=0.10, color=col)
    
    ax_traj.set_xlabel("Career year")
    ax_traj.set_ylabel("Expected points per race\n(population-average car)")
    ax_traj.set_title(
        "Predicted career trajectories\n(90 % credible band; average car quality)",
        fontsize=9
    )
    ax_traj.legend(fontsize=8, ncol=2)
    ax_traj.set_xlim(1, 15)
    
    # ---- Panel C: team baseline distribution ------------------------------------
    ax_team = fig.add_subplot(gs[1, 1])
    
    top15_teams = team_summary.head(15).reset_index(drop=True)
    y_team = np.arange(len(top15_teams))
    
    ax_team.barh(y_team, top15_teams["mean"], color="coral", alpha=0.6, height=0.6)
    ax_team.errorbar(
        top15_teams["mean"], y_team,
        xerr=[top15_teams["mean"] - top15_teams["q2_5"],
              top15_teams["q97_5"] - top15_teams["mean"]],
        fmt="none", color="#1a1a2e", linewidth=1.0, capsize=2,
    )
    ax_team.set_yticks(y_team)
    ax_team.set_yticklabels(top15_teams["team"], fontsize=8)
    ax_team.axvline(0, color="grey", linewidth=0.8, linestyle="--", alpha=0.7)
    ax_team.set_xlabel("Team baseline quality (log-scale, vs average)")
    ax_team.set_title("Top 15 constructors: all-time baseline\n(95 % credible interval)", fontsize=9)
    ax_team.invert_yaxis()
    
    fig.suptitle("F1 Driver Rankings – Bayesian Hierarchical Model (PyMC)\n"
                 "NegativeBinomial likelihood, non-centered driver effects, "
                 "hierarchical team effects",
                 fontsize=10, y=1.01)
    
    plt.savefig("f1_rankings_pymc.png", dpi=150, bbox_inches="tight")
    plt.show()
    print("\nPlot saved to f1_rankings_pymc.png")
    
    
    # =============================================================================
    # EXPORT RESULTS FOR MODEL COMPARISON
    # =============================================================================
    # Writes the key fitted quantities of the PyMC model to model_outputs/ so they
    # can be compared against the brms model (see compare_models.R).  File names and
    # column conventions mirror the R export block so the comparison script can join
    # the two sets of results on the (verbatim) Driver string.
    
    OUT_DIR = Path("model_outputs")
    OUT_DIR.mkdir(exist_ok=True)
    
    # 1. Population-level parameters ----------------------------------------------
    (
        az.summary(trace, var_names=pop_vars, round_to=6)
        .reset_index()
        .rename(columns={"index": "term"})
        .to_csv(OUT_DIR / "py_population_params.csv", index=False)
    )
    
    # 2. Driver ability summary (intercept deviation, log scale) ------------------
    driver_summary.to_csv(OUT_DIR / "py_driver_coefs.csv", index=False)
    
    # 3. Team baseline summary ----------------------------------------------------
    team_summary.to_csv(OUT_DIR / "py_team_summary.csv", index=False)
    
    # Helper: expected points/race for driver d at a z-scored career year, average
    # car (car_year_effect = 0).  Returns the posterior mean of mu (= E[y] for NB).
    def _expected_points(d_idx: int, cy_z_val: float) -> np.ndarray:
        log_mu_s = (
            alpha_samples
            + (b1_samples + de_flat[:, d_idx, 1]) * cy_z_val
            + (b2_samples + de_flat[:, d_idx, 2]) * cy_z_val ** 2
            + de_flat[:, d_idx, 0]
        )
        return np.exp(log_mu_s)
    
    # 4. Driver career trajectories in an AVERAGE car -----------------------------
    career_grid_full = np.arange(1, 21)
    cy_z_grid_full = (career_grid_full - cy_mean) / cy_sd
    
    traj_export = []
    for drv in drivers_25:
        if drv not in driver_names:
            continue
        d = driver_names.index(drv)
        for cy_val, cy_z_val in zip(career_grid_full, cy_z_grid_full):
            mu_s = _expected_points(d, cy_z_val)
            traj_export.append({
                "Driver":      drv,
                "career_year": int(cy_val),
                "pred_mean":   mu_s.mean(),
                "pred_lwr":    np.percentile(mu_s, 5),
                "pred_upr":    np.percentile(mu_s, 95),
            })
    pd.DataFrame(traj_export).to_csv(
        OUT_DIR / "py_driver_trajectories.csv", index=False
    )
    
    # 5. Single-number ability metric ---------------------------------------------
    # Expected points/race at career_year = 7, average car – the common currency
    # matching the R export.
    cy7_z = (7 - cy_mean) / cy_sd
    ability_export = []
    for drv in drivers_25:
        if drv not in driver_names:
            continue
        d = driver_names.index(drv)
        mu_s = _expected_points(d, cy7_z)
        ability_export.append({
            "Driver":      drv,
            "career_year": 7,
            "pred_mean":   mu_s.mean(),
            "pred_lwr":    np.percentile(mu_s, 5),
            "pred_upr":    np.percentile(mu_s, 95),
            "races":       int(driver_race_counts.get(drv, 0)),
        })
    pd.DataFrame(ability_export).to_csv(
        OUT_DIR / "py_driver_ability.csv", index=False
    )
    
    print(f"PyMC model outputs written to {OUT_DIR.resolve()}")


# =============================================================================
# ALTERNATIVE MODEL IDEAS (commented out – extensions for future work)
# =============================================================================

# ---------------------------------------------------------------------------
# ALTERNATIVE A: Gaussian Process career trajectory
# ---------------------------------------------------------------------------
# Replace the fixed quadratic polynomial with a per-driver GP over career year.
# The GP allows arbitrary smooth shapes – drivers who peak early, plateau, or
# have unusual comebacks are not penalised.  The quadratic is a special case
# of the GP with a specific kernel.
#
# with pm.Model() as gp_model:
#     # Shared length-scale and amplitude for career trajectory GP
#     ls_career  = pm.HalfNormal("ls_career", sigma=3.0)   # in years
#     amp_career = pm.HalfNormal("amp_career", sigma=0.5)  # on log scale
#
#     # One GP per driver (shared hyper-parameters, independent realisations)
#     cov = amp_career**2 * pm.gp.cov.Matern52(1, ls=ls_career)
#     gp  = pm.gp.Latent(cov_func=cov)
#
#     # Evaluate GP at observed career years for each driver
#     # (would require reshaping by driver and using separate inducing points)
#     f_career = gp.prior("f_career", X=career_year_obs[:, None])
#     ...
#
# Practical note: a latent GP over 30,000 observations is expensive.
# Use sparse GP (pm.gp.MarginalSparse) with inducing points at
# career_year ∈ {1, 2, ..., 20}.

# ---------------------------------------------------------------------------
# ALTERNATIVE B: Random-walk team performance arc
# ---------------------------------------------------------------------------
# Replace the year-specific deviations (independent Normal) with a Gaussian
# Random Walk within each team over calendar years.  This captures gradual
# performance arcs (e.g. Red Bull's 2019-2022 rise) rather than treating
# each year as an independent fluctuation.
#
# The implementation requires padding teams to the same number of years and
# masking out years when the team did not compete.
#
# with pm.Model() as rw_model:
#     sigma_rw = pm.HalfNormal("sigma_rw", sigma=0.2)
#
#     # For each team t, a random walk over n_years calendar years.
#     # GaussianRandomWalk(sigma, shape) models:
#     #   rw[0] ~ Normal(0, sigma)
#     #   rw[t] ~ Normal(rw[t-1], sigma)  for t > 0
#     team_rw = pm.GaussianRandomWalk(
#         "team_rw", sigma=sigma_rw, shape=(n_teams, n_years)
#     )
#     # Index into team_rw using (team_idx, year_idx) for each car-year
#     car_year_effect_rw = team_rw[team_idx_for_car_year, year_idx_for_car_year]
#     ...

# ---------------------------------------------------------------------------
# ALTERNATIVE C: Hurdle-NegativeBinomial (two-part model)
# ---------------------------------------------------------------------------
# The distribution of total_points is bimodal: 38 % zeros (DNFs, outside
# top-10) plus a roughly uniform mass over 1-11 for points-scorers.
# A hurdle model separates:
#   Part 1: logistic regression for P(points > 0) – "did the driver score?"
#   Part 2: NegativeBinomial for the conditional distribution given scoring
#
# with pm.Model() as hurdle_model:
#     # Logit probability of scoring at all
#     logit_p = alpha_hurdle + driver_ability_hurdle[driver_idx] + ...
#     scored   = pm.Bernoulli("scored", logit_p=logit_p, observed=(y > 0).astype(int))
#
#     # Conditional NB on points > 0 (observed subset)
#     mask = y > 0
#     mu_cond = pm.math.exp(log_mu[mask])
#     pm.NegativeBinomial("y_cond", mu=mu_cond, alpha=alpha_nb, observed=y[mask])
#
# This gives a cleaner separation between finishing the race and finishing
# well, which may improve out-of-sample predictions for DNF-prone drivers.
