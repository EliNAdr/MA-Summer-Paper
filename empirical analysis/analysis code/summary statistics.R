# =============================================================================
# Script 04: Summary statistics
#
# INPUT:
#   final_panel.rds
#
# OUTPUT:
#   summary statistics
#   number of members with career gaps
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Load required package
# -----------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)


# -----------------------------------------------------------------------------
# 1. Load input data
# -----------------------------------------------------------------------------

INPUT_FILE <- "empirical analysis/data files/final_panel.rds"

if (!file.exists(INPUT_FILE)) {
  stop("Data not found. Run 'merge and collapse.R' first.")
}

data <- readRDS(INPUT_FILE)


INPUT_FILE2 <- "empirical analysis/data files/vote_data.rds"
if (!file.exists(INPUT_FILE2)) {
  message("The vote count summary requires vote_data.rds. Run 'data from Rvoteview.R' first.")
}

# -----------------------------------------------------------------------------
# 2. Summary statistics
# -----------------------------------------------------------------------------

# --- Vote-level count ---
cat("\n--- By Congress ---")
if (file.exists(INPUT_FILE2)) {
  vote_data <- readRDS(INPUT_FILE2)
  vote_data <- vote_data |>
  group_by(congress) |>
  summarise(
    vote_count = n_distinct(rollcall_id)
  ) |>
  ungroup() |>
  summarise(
    votes_mean = mean(vote_count),
    votes_min = min(vote_count),
    votes_max = max(vote_count),
    votes_sd = sd(vote_count)
  )
  
  cat(sprintf("\nNumber of votes        mean=%.0f  min=%.0f  max=%.0f  SD=%.0f\n",
            vote_data$votes_mean, vote_data$votes_min, vote_data$votes_max, vote_data$votes_sd))
}


# --- By-observation statistics ---
ove_mean <- mean(data$overall_loyalty,     na.rm = TRUE)
ove_min  <- min(data$overall_loyalty,      na.rm = TRUE)
ove_max  <- max(data$overall_loyalty,      na.rm = TRUE)
ove_sd   <- sd(data$overall_loyalty,       na.rm = TRUE)

par_mean <- mean(data$partisan_loyalty,     na.rm = TRUE)
par_min  <- min(data$partisan_loyalty,      na.rm = TRUE)
par_max  <- max(data$partisan_loyalty,      na.rm = TRUE)
par_sd   <- sd(data$partisan_loyalty,       na.rm = TRUE)

unc_mean <- mean(data$unconditional_loyalty,     na.rm = TRUE)
unc_min  <- min(data$unconditional_loyalty,      na.rm = TRUE)
unc_max  <- max(data$unconditional_loyalty,      na.rm = TRUE)
unc_sd   <- sd(data$unconditional_loyalty,       na.rm = TRUE)

cat("\n--- By observation ---\n")
cat(sprintf("Overall loyalty        mean=%.1f  min=%.1f  max=%.1f  SD=%.1f\n",
            ove_mean, ove_min, ove_max, ove_sd))
cat(sprintf("Partisan loyalty       mean=%.1f  min=%.1f  max=%.1f  SD=%.1f\n",
            par_mean, par_min, par_max, par_sd))
cat(sprintf("Unconditional loyalty  mean=%.1f  min=%.1f  max=%.1f  SD=%.1f\n",
            unc_mean, unc_min, unc_max, unc_sd))

# --- By-representative statistics ---
rep_level <- data |>
  group_by(bioguide_id) |>
  summarise(
    n_terms           = n(),
    first_house_year  = first(first_house_year),
    birth_year        = first(birth_year),
    sd_overall        = sd(overall_loyalty),
    sd_partisan       = sd(partisan_loyalty),
    sd_uncon          = sd(unconditional_loyalty),
    .groups = "drop"
  ) |>
  mutate(entry_age = first_house_year - birth_year)

terms_mean <- mean(rep_level$n_terms)
terms_min  <- min(rep_level$n_terms)
terms_max  <- max(rep_level$n_terms)
terms_sd   <- sd(rep_level$n_terms)

first_year_mean <- mean(rep_level$first_house_year)
first_year_min  <- min(rep_level$first_house_year)
first_year_max  <- max(rep_level$first_house_year)
first_year_sd   <- sd(rep_level$first_house_year)

entry_age_mean <- mean(rep_level$entry_age)
entry_age_min  <- min(rep_level$entry_age)
entry_age_max  <- max(rep_level$entry_age)
entry_age_sd   <- sd(rep_level$entry_age)

sd_overall_mean <- mean(rep_level$sd_overall, na.rm = TRUE)
sd_overall_min  <- min(rep_level$sd_overall, na.rm = TRUE)
sd_overall_max  <- max(rep_level$sd_overall, na.rm = TRUE)
sd_overall_sd   <- sd(rep_level$sd_overall, na.rm = TRUE)

sd_partisan_mean <- mean(rep_level$sd_partisan, na.rm = TRUE)
sd_partisan_min  <- min(rep_level$sd_partisan, na.rm = TRUE)
sd_partisan_max  <- max(rep_level$sd_partisan, na.rm = TRUE)
sd_partisan_sd   <- sd(rep_level$sd_partisan, na.rm = TRUE)

sd_uncon_mean <- mean(rep_level$sd_uncon, na.rm = TRUE)
sd_uncon_min  <- min(rep_level$sd_uncon, na.rm = TRUE)
sd_uncon_max  <- max(rep_level$sd_uncon, na.rm = TRUE)
sd_uncon_sd   <- sd(rep_level$sd_uncon, na.rm = TRUE)

cat("\n--- By unique Representative ---\n")
cat(sprintf("Number of terms in sample               mean=%.1f   min=%d     max=%d    SD=%.1f\n",
            terms_mean, terms_min, terms_max, terms_sd))
cat(sprintf("First year in House                     mean=%d  min=%d  max=%d  SD=%.0f\n",
            as.integer(first_year_mean), first_year_min, first_year_max, first_year_sd))
cat(sprintf("Age when first seated in House          mean=%d    min=%d    max=%d    SD=%.0f\n",
            as.integer(entry_age_mean), entry_age_min, entry_age_max, entry_age_sd))
cat(sprintf("Within-Representative overall SD        mean=%.1f   min=%.1f   max=%.1f  SD=%.1f\n",
            sd_overall_mean, sd_overall_min, sd_overall_max, sd_overall_sd))
cat(sprintf("Within-Representative partisan SD       mean=%.1f   min=%.1f   max=%.1f  SD=%.1f\n",
            sd_partisan_mean, sd_partisan_min, sd_partisan_max, sd_partisan_sd))
cat(sprintf("Within-Representative unconditional SD  mean=%.1f   min=%.1f   max=%.1f  SD=%.1f\n",
            sd_uncon_mean, sd_uncon_min, sd_uncon_max, sd_uncon_sd))

# --- Dataset-level counts ---
cat("\n--- Dataset counts ---\n")
n_obs        <- nrow(data)
n_reps       <- n_distinct(data$bioguide_id)
n_congresses <- n_distinct(data$congress)

cat("Number of observations:          ", n_obs, "\n")
cat("Number of unique Representatives:", n_reps, "\n")
cat("Number of Congresses:            ", n_congresses, "\n\n")


# -----------------------------------------------------------------------------
# 3. Career gaps
# -----------------------------------------------------------------------------

gap_members <- data |>
  arrange(bioguide_id, congress) |>
  group_by(bioguide_id) |>
  mutate(
    congress_gap = congress - lag(congress),
    term_gap = house_term_number - lag(house_term_number)) |>
  filter(congress_gap > 1) |>
  filter(congress_gap != term_gap) |>
  ungroup() |>
  distinct(bioguide_id) |>
  pull(bioguide_id)

message("Members with career gaps: ", length(gap_members))