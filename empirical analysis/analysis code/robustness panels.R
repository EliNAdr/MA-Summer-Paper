# =============================================================================
# Script 06: Paths for samples conditioned on terms
#
# INPUT:
#   final_panel.rds
#
# OUTPUTS:
#   panels of robustness checks and channel checks for the main results
#
# NOTE: Normalization follows the same procedure as 'regressions normalized.R'.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Load required packages & choose plot parameters
# -----------------------------------------------------------------------------

library(tidyverse)
library(fixest)

PLOT_MAX_TERM <- 10 # number of terms to plot; 10 by default
TREND_START <- 4 # start of terms used to fit trend line

sample_condition_thresholds <- c(4, 7, 10) # sample conditioning thresholds

term_cap_thresholds <- c(7, 13, 16) # term caps

# -----------------------------------------------------------------------------
# 1. Load input data, select observations, create variables
# -----------------------------------------------------------------------------

INPUT_FILE <- "empirical analysis/data files/final_panel.rds"

if (!file.exists(INPUT_FILE)) {
  stop("Data not found. Run 'merge and collapse.R' first.")
}

data <- readRDS(INPUT_FILE)

data <- data |>
  group_by(bioguide_id) |>
  mutate(career_max_term = max(house_term_number)) |>
  ungroup()

# Untruncated copy, kept for the term-cap panel in section 4
data_full <- data

data <- data |>
  filter(house_term_number <= PLOT_MAX_TERM) |>
  mutate(
    term = as.factor(house_term_number)
  )


# -----------------------------------------------------------------------------
# 2. Helpers for robustness panels
# -----------------------------------------------------------------------------

normalized_coefs <- function(regression, max_term = PLOT_MAX_TERM, trend = TREND_START:PLOT_MAX_TERM) {
  J <- max_term
  nms <- paste0("term", 1:J)
  keep <- nms[-1] # term 1 is the omitted reference

  cf <- coef(regression)
  Vf <- vcov(regression) # clustered at Representative level by feols regressions

  # Coefficient vector with the reference term added back in at zero
  b <- setNames(numeric(J), nms)
  b[keep] <- cf[keep]

  # Covariance with a zero row/column for the reference term
  V <- matrix(0, J, J, dimnames = list(nms, nms))
  V[keep, keep] <- Vf[keep, keep]

  # Contrast matrix: g_j = beta_j - (OLS line through trend terms, evaluated at j)
  a <- trend
  tbar <- mean(a)
  Sxx <- sum((a - tbar)^2)
  C <- matrix(0, J, J, dimnames = list(nms, nms))
  for (j in 1:J) {
    C[j, j] <- C[j, j] + 1
    C[j, a] <- C[j, a] - 1 / length(a) - (a - tbar) * (j - tbar) / Sxx
  }

  g <- as.vector(C %*% b)
  se <- sqrt(pmax(diag(C %*% V %*% t(C)), 0)) # pmax() in case of numerical precision issues

  tibble(
    term_num = 1:J,
    estimate = g,
    std.error = se,
    conf.low = g - 1.96 * se, # 95% CIs
    conf.high = g + 1.96 * se,
    term = nms
  )
}

# -----------------------------------------------------------------------------
# 3. Panel of overall loyalty regressions with different term minimum conditions
# -----------------------------------------------------------------------------

message("\nConditional samples\n")

# For a given minimum-term threshold (NA = no conditioning), filter to
# representatives who reach at least that term, run the overall loyalty
# regression, and return the normalized point estimates.
conditional_path <- function(min_term) {
  sample_data <- data
  if (!is.na(min_term)) {
    sample_data <- sample_data |>
      filter(career_max_term >= min_term)
  }

  reg <- feols(
    overall_loyalty ~ term + is_leader
    | bioguide_id + congress, data = sample_data, cluster = ~bioguide_id
    )

  n_obs <- nobs(reg)
  path_label <- if (is.na(min_term)) {
    paste0("All observations (N = ", n_obs, ")")
  } else {
    paste0("Conditional on reaching term ", min_term, " (N = ", n_obs, ")")
  }

  coefs <- normalized_coefs(reg)

  list(
    points = mutate(coefs, path = path_label),
    label = path_label
  )
}

paths <- map(sample_condition_thresholds, conditional_path)
path_labels <- map_chr(paths, "label")

points_df <- map_dfr(paths, "points") |>
  mutate(path = factor(path, levels = path_labels))

p_conditional <- ggplot(points_df, aes(x = term_num, y = estimate, color = path)) +
  geom_vline(xintercept = 3, colour = "navy", linewidth = 0.3, linetype = 5) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  facet_wrap(~path, ncol = 1) +
  scale_x_continuous(breaks = 1:PLOT_MAX_TERM, limits = c(1, PLOT_MAX_TERM)) +
  scale_color_brewer(palette = "Set1", guide = "none") +
  labs(x = "Term", y = "Loyalty Relative to Later-Career Trend\n(percentage points)\n") +
  theme_bw() +
  theme(strip.text = element_text(size = 8))


# -----------------------------------------------------------------------------
# 4. Panel of overall loyalty regressions with different term caps
# -----------------------------------------------------------------------------

message("\nTerm Caps\n")

# For a given term cap, drop observations beyond the cap, run the overall
# loyalty regression, and return the normalized point estimates.
term_cap_path <- function(max_term) {
  sample_data <- data_full |>
    filter(house_term_number <= max_term) |>
    mutate(term = as.factor(house_term_number))

  reg <- feols(
    overall_loyalty ~ term + is_leader
    | bioguide_id + congress, data = sample_data, cluster = ~bioguide_id
    )

  n_obs <- nobs(reg)
  path_label <- paste0("Term cap = ", max_term, " (N = ", n_obs, ")")

  coefs <- normalized_coefs(reg, max_term = max_term, trend = TREND_START:max_term)

  list(
    points = mutate(coefs, path = path_label),
    label = path_label
  )
}

term_cap_paths <- map(term_cap_thresholds, term_cap_path)
term_cap_labels <- map_chr(term_cap_paths, "label")

term_cap_points_df <- map_dfr(term_cap_paths, "points") |>
  mutate(path = factor(path, levels = term_cap_labels))

p_term_cap <- ggplot(term_cap_points_df, aes(x = term_num, y = estimate, color = path)) +
  geom_vline(xintercept = 3, colour = "navy", linewidth = 0.3, linetype = 5) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  facet_wrap(~path, ncol = 1) +
  scale_x_continuous(breaks = 1:max(term_cap_thresholds), limits = c(1, max(term_cap_thresholds))) +
  scale_color_brewer(palette = "Set1", guide = "none") +
  labs(x = "Term", y = "Loyalty Relative to Later-Career Trend\n(percentage points)\n") +
  theme_bw() +
  theme(strip.text = element_text(size = 8))


# -----------------------------------------------------------------------------
# 5. Pre- and post- 2012 accrual change (full sample & close sample)
# -----------------------------------------------------------------------------

# For a given first-house-year range, filter to representatives whose first
# House year falls in that range, run the overall loyalty regression, and return
# the normalized point estimates.
accrual_change_path <- function(range, label) {
  sample_data <- data |>
    filter(first_house_year >= range[1], first_house_year <= range[2])

  # Highest term actually observed in this sample: members first seated
  # near the end of the panel can't be observed reaching term PLOT_MAX_TERM.
  max_term <- min(PLOT_MAX_TERM, max(sample_data$house_term_number))

  reg <- feols(
    overall_loyalty ~ term + is_leader
    | bioguide_id + congress, data = sample_data, cluster = ~bioguide_id
    )

  n_obs <- nobs(reg)
  path_label <- paste0(label, " (N = ", n_obs, ")")

  coefs <- normalized_coefs(reg, max_term = max_term, trend = TREND_START:max_term)

  list(
    points = mutate(coefs, path = path_label),
    label = path_label
  )
}

# Build the accrual-change panel plot for a given set of first-house-year
# range thresholds (each a named list of c(min, max) vectors).
accrual_change_panel <- function(thresholds) {
  paths <- imap(thresholds, accrual_change_path)
  labels <- map_chr(paths, "label")

  points_df <- map_dfr(paths, "points") |>
    mutate(path = factor(path, levels = labels))

  ggplot(points_df, aes(x = term_num, y = estimate, color = path)) +
    geom_vline(xintercept = 3, colour = "navy", linewidth = 0.3, linetype = 5) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high), na.rm = TRUE) +
    geom_point(size = 2, na.rm = TRUE) +
    facet_wrap(~path, ncol = 1) +
    scale_x_continuous(breaks = 1:PLOT_MAX_TERM, limits = c(1, PLOT_MAX_TERM)) +
    scale_color_brewer(palette = "Set1", guide = "none") +
    labs(x = "Term", y = "Loyalty Relative to Later-Career Trend\n(percentage points)\n") +
    theme_bw() +
    theme(strip.text = element_text(size = 8))
}

message("\nPre- and post-2012 accrual rate change\n")

# Members first seated after 2007 could not vest before the rate decrease
p_accrual_change <- accrual_change_panel(list(
  "First seated before 2008" = c(-Inf, 2007),
  "First seated 2008 or later" = c(2008, Inf)
))

message("\nPre- and post-2012 accrual rate change (close sample)\n")

# Use narrow window in order to avoid confounding time trends
p_accrual_change_narrow <- accrual_change_panel(list(
  "First seated 2001-2007" = c(2001, 2007),
  "First seated 2008-2015" = c(2008, 2015)
))

# -----------------------------------------------------------------------------
# 7. Save plots
# -----------------------------------------------------------------------------

ggsave("empirical analysis/plots/conditional_samples_panel.pdf", plot = p_conditional, width = 9, height = 4.5)
ggsave("empirical analysis/plots/term_cap_panel.pdf", plot = p_term_cap, width = 9, height = 4.5)
ggsave("empirical analysis/plots/accrual_change.pdf", plot = p_accrual_change, width = 9, height = 4.5)
ggsave("empirical analysis/plots/accrual_change_narrow.pdf", plot = p_accrual_change_narrow, width = 9, height = 4.5)