# =============================================================================
# Compare brms (R) vs PyMC F1 driver-ranking models
# =============================================================================
#
# PRE-REQUISITES
#   Run both model scripts first so their export blocks populate model_outputs/:
#     1. analysis_and_modelling_2026.R  -> r_*.csv
#     2. f1_model_pymc.py               -> py_*.csv
#
# WHAT THIS SCRIPT DOES
#   The two models use different likelihoods (Poisson vs NegativeBinomial),
#   different car parameterisations (independent car-year vs hierarchical team),
#   and different career-trajectory bases (orthogonal poly vs z-scored quadratic).
#   Their raw coefficients are therefore NOT on a common scale.  To compare them
#   fairly we use a COMMON CURRENCY both models can produce:
#
#       expected total_points per race, in an AVERAGE car, by career year.
#
#   This is a directly interpretable "sporting" quantity (points/race) that
#   marginalises away each model's internal parameterisation.
#
#   We assess two kinds of difference:
#     STATISTICAL - agreement of point estimates, ranks, and uncertainty.
#     SPORTING    - who each model rates highest, biggest disagreements, and
#                   how career trajectories differ for marquee drivers.
# =============================================================================

library(tidyverse)
library(ggrepel)

out_dir  <- "model_outputs"
fig_dir  <- "comparison_outputs"
dir.create(fig_dir, showWarnings = FALSE)

rd <- function(f) read_csv(file.path(out_dir, f), show_col_types = FALSE)

r_ability  <- rd("r_driver_ability.csv")
py_ability <- rd("py_driver_ability.csv")
r_traj     <- rd("r_driver_trajectories.csv")
py_traj    <- rd("py_driver_trajectories.csv")
r_pop      <- rd("r_population_params.csv")
py_pop     <- rd("py_population_params.csv")


# =============================================================================
# 1. ABILITY: common-currency comparison (points/race, average car, career yr 7)
# =============================================================================

ability <- r_ability %>%
  select(Driver, races, r_pred = pred_mean, r_lwr = pred_lwr, r_upr = pred_upr) %>%
  inner_join(
    py_ability %>%
      select(Driver, py_pred = pred_mean, py_lwr = pred_lwr, py_upr = pred_upr),
    by = "Driver"
  ) %>%
  mutate(
    r_rank      = min_rank(desc(r_pred)),
    py_rank     = min_rank(desc(py_pred)),
    rank_change = r_rank - py_rank,          # +ve => Python ranks higher
    pred_diff   = py_pred - r_pred,          # +ve => Python more generous
    r_ci_width  = r_upr - r_lwr,
    py_ci_width = py_upr - py_lwr
  )

cat("\n==================================================================\n")
cat("STATISTICAL AGREEMENT\n")
cat("==================================================================\n")

n_drivers <- nrow(ability)
pearson   <- cor(ability$r_pred, ability$py_pred)
spearman  <- cor(ability$r_pred, ability$py_pred, method = "spearman")
mae       <- mean(abs(ability$pred_diff))
bias      <- mean(ability$pred_diff)

cat(sprintf("Drivers compared (>=25 races, in both models): %d\n", n_drivers))
cat(sprintf("Pearson  r (points/race) : %.3f\n", pearson))
cat(sprintf("Spearman r (rank agree)  : %.3f\n", spearman))
cat(sprintf("Mean abs difference      : %.3f pts/race\n", mae))
cat(sprintf("Mean signed difference   : %+.3f pts/race  (PyMC - brms)\n", bias))
cat(sprintf("Median 90%% CI width      : brms %.2f vs PyMC %.2f pts/race\n",
            median(ability$r_ci_width), median(ability$py_ci_width)))

# Population parameters side by side (note: NOT directly comparable in scale;
# printed for reference / sanity only).
cat("\n--- Population parameters (reference only, different parameterisations) ---\n")
cat("\nbrms fixef:\n"); print(r_pop)
cat("\nPyMC population:\n"); print(py_pop)


# =============================================================================
# 2. SPORTING: rankings and disagreements
# =============================================================================

cat("\n==================================================================\n")
cat("SPORTING COMPARISON\n")
cat("==================================================================\n")

cat("\n--- Top 15 by brms ---\n")
ability %>% arrange(r_rank) %>%
  transmute(Driver, races,
            brms_pts = round(r_pred, 2), brms_rank = r_rank,
            pymc_pts = round(py_pred, 2), pymc_rank = py_rank,
            rank_change) %>%
  head(15) %>% print(n = 15)

cat("\n--- Top 15 by PyMC ---\n")
ability %>% arrange(py_rank) %>%
  transmute(Driver, races,
            pymc_pts = round(py_pred, 2), pymc_rank = py_rank,
            brms_pts = round(r_pred, 2), brms_rank = r_rank,
            rank_change) %>%
  head(15) %>% print(n = 15)

cat("\n--- Biggest rank disagreements ---\n")
ability %>%
  mutate(abs_rank_change = abs(rank_change)) %>%
  arrange(desc(abs_rank_change)) %>%
  transmute(Driver, races, brms_rank = r_rank, pymc_rank = py_rank,
            rank_change,
            brms_pts = round(r_pred, 2), pymc_pts = round(py_pred, 2)) %>%
  head(15) %>% print(n = 15)

# Persist the merged ability table for downstream use
write_csv(ability, file.path(fig_dir, "ability_comparison.csv"))


# =============================================================================
# 3. PLOTS
# =============================================================================

# ---- 3a. Agreement scatter: brms vs PyMC points/race ------------------------
lim <- range(c(ability$r_pred, ability$py_pred))
p_scatter <- ability %>%
  ggplot(aes(x = r_pred, y = py_pred)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = races), alpha = 0.5, colour = "steelblue") +
  geom_text_repel(
    data = ~ slice_max(.x, abs(pred_diff), n = 12),
    aes(label = Driver), size = 2.6, max.overlaps = 20
  ) +
  coord_equal(xlim = lim, ylim = lim) +
  scale_size_continuous(range = c(1, 6)) +
  labs(
    title = "Driver ability: brms vs PyMC",
    subtitle = sprintf("Expected points/race, average car, career year 7  |  Pearson r = %.3f",
                       pearson),
    x = "brms (Poisson) predicted points/race",
    y = "PyMC (NegBin, hierarchical) predicted points/race",
    size = "Races"
  ) +
  theme_minimal()
ggsave(file.path(fig_dir, "ability_scatter.png"), p_scatter,
       width = 8, height = 8, dpi = 150)

# ---- 3b. Rank slope chart for the union of each model's top 20 ---------------
top_union <- ability %>%
  filter(r_rank <= 20 | py_rank <= 20)

p_slope <- top_union %>%
  select(Driver, brms = r_rank, PyMC = py_rank) %>%
  pivot_longer(c(brms, PyMC), names_to = "model", values_to = "rank") %>%
  mutate(model = factor(model, levels = c("brms", "PyMC"))) %>%
  ggplot(aes(x = model, y = rank, group = Driver)) +
  geom_line(alpha = 0.4) +
  geom_point() +
  geom_text_repel(
    data = ~ filter(.x, model == "brms"),
    aes(label = Driver), hjust = 1, nudge_x = -0.05, size = 2.5,
    direction = "y", max.overlaps = 30
  ) +
  scale_y_reverse(breaks = scales::pretty_breaks()) +
  labs(title = "Driver ranking shifts: brms -> PyMC",
       subtitle = "Union of each model's top 20 (rank 1 = best)",
       x = NULL, y = "Rank") +
  theme_minimal()
ggsave(file.path(fig_dir, "rank_slope.png"), p_slope,
       width = 8, height = 10, dpi = 150)

# ---- 3c. Career trajectory overlays for marquee drivers ----------------------
focus <- c("Lewis Hamilton HAM", "Michael Schumacher MSC", "Max Verstappen VER",
           "Ayrton Senna SEN", "Alain Prost PRO", "Sebastian Vettel VET",
           "Fernando Alonso ALO", "Lando Norris NOR")

traj_both <- bind_rows(
  r_traj  %>% mutate(model = "brms"),
  py_traj %>% mutate(model = "PyMC")
) %>%
  filter(Driver %in% focus)

p_traj <- traj_both %>%
  ggplot(aes(x = career_year, y = pred_mean, colour = model, fill = model)) +
  geom_ribbon(aes(ymin = pred_lwr, ymax = pred_upr), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ Driver, scales = "free_y") +
  labs(title = "Career trajectories: brms vs PyMC",
       subtitle = "Expected points/race in an average car (band = 90% interval)",
       x = "Career year", y = "Expected points/race", colour = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
ggsave(file.path(fig_dir, "trajectories.png"), p_traj,
       width = 11, height = 7, dpi = 150)

cat(sprintf("\nComparison tables and figures written to %s\n",
            normalizePath(fig_dir)))
