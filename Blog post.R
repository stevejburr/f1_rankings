# Libraries ---------------------------------------------------------------
library(tidyverse)
library(brms)
library(emmeans)
library(ggrepel)
library(marginaleffects)

# Data Read ---------------------------------------------------------------

# Read in CSV of all results data
all_data <- read_csv("all_f1_race_results_21_12_22.csv")

# Read in preferred model -
model_4 <- read_rds("model_4.rds")