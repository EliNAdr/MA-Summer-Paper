# =============================================================================
# Script 05: Regressions & Plots
#
# INPUT:
#   final_panel.rds
#
# OUTPUTS:
#   normalized coefficient plots
#
# NOTE: See Section 5 and Appendix D for details on normalization.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Load required packages & choose plot parameters
# -----------------------------------------------------------------------------

library(tidyverse)
library(fixest)

PLOT_MAX_TERM <- 10 # number of terms to plot; 10 by default
TREND_START <- 4 # start of terms used to fit trend line

TREND_TERMS <- TREND_START:PLOT_MAX_TERM

# -----------------------------------------------------------------------------
# 1. Load input data, select observations, create variables
# -----------------------------------------------------------------------------

INPUT_FILE <- "empirical analysis/data files/final_panel.rds"

if (!file.exists(INPUT_FILE)) {
  stop("Data not found. Run 'merge and collapse.R' first.")
}

data <- readRDS(INPUT_FILE)

data <- data |>
  filter(house_term_number <= PLOT_MAX_TERM) |>
  mutate(
    term = as.factor(house_term_number)
  )


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

message("Members in regression sample with career gaps: ", length(gap_members))


# -----------------------------------------------------------------------------
# 2. Helpers for normalized coefficient plots
# -----------------------------------------------------------------------------

normalized_coefs <- function(regression, max_term = PLOT_MAX_TERM, trend = TREND_TERMS) {
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

# Normalized coefficient plot
norm_coef_plot <- function(regression, ylim, ybreaks, title, trend = TREND_TERMS) {
  coefs <- normalized_coefs(regression, trend = trend)

  ggplot(coefs, aes(x = term, y = estimate)) +
    geom_vline(xintercept = 3, colour = "navy", linewidth = 0.3, linetype = 5) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
    geom_point(size = 2.5) +
    scale_x_discrete(limits = paste0("term", 1:PLOT_MAX_TERM), labels = as.character(1:PLOT_MAX_TERM)) +
    scale_y_continuous(breaks = ybreaks) +
    labs(
      x = "Term",
      y = "Loyalty Relative to Later-Career Trend\n(percentage points)\n",
      title = title) +
    coord_cartesian(ylim = ylim) +
    theme_bw()
}


# -----------------------------------------------------------------------------
# 3. Overall loyalty
# -----------------------------------------------------------------------------

message("\nOverall loyalty\n")

regression1 <- feols(
  overall_loyalty ~ term + is_leader
  | bioguide_id + congress, data = data, cluster = ~bioguide_id
)

p1 <- norm_coef_plot(regression1, ylim = c(-0.5, 2), ybreaks = seq(0, 2, by = 1), title = paste0("N = ", nobs(regression1)))


# -----------------------------------------------------------------------------
# 4. Partisan loyalty
# -----------------------------------------------------------------------------

message("\nPartisan loyalty\n")

regression2 <- feols(
  partisan_loyalty ~ term + is_leader
  | bioguide_id + congress, data = data, cluster = ~bioguide_id
)

p2 <- norm_coef_plot(regression2, ylim = c(-0.5, 2), ybreaks = seq(0, 2, by = 1), title = paste0("N = ", nobs(regression2)))


# -----------------------------------------------------------------------------
# 5. Unconditional loyalty
# -----------------------------------------------------------------------------

message("\nUnconditional loyalty\n")

regression3 <- feols(
  unconditional_loyalty ~ term + is_leader
  | bioguide_id + congress, data = data, cluster = ~bioguide_id
)

p3 <- norm_coef_plot(regression3, ylim = c(-1, 3.5), ybreaks = seq(-1, 3, by = 1), title = paste0("N = ", nobs(regression3)))


# -----------------------------------------------------------------------------
# 6. Differential trends based on party (overall loyalty)
# -----------------------------------------------------------------------------

message("\nParty Congress trends, overall loyalty\n")

regression4 <- feols(
  overall_loyalty ~ term + is_leader
  | bioguide_id + congress + congress^party, data = data, cluster = ~bioguide_id
)

p4 <- norm_coef_plot(regression4, ylim = c(-0.5, 2), ybreaks = seq(0, 2, by = 1), title = paste0("N = ", nobs(regression4)))


# -----------------------------------------------------------------------------
# 7. Differential trends based on party (partisan loyalty)
# -----------------------------------------------------------------------------

message("\nParty Congress trends, partisan loyalty\n")

regression5 <- feols(
  partisan_loyalty ~ term + is_leader
  | bioguide_id + congress + congress^party, data = data, cluster = ~bioguide_id
)

p5 <- norm_coef_plot(regression5, ylim = c(-0.5, 2), ybreaks = seq(0, 2, by = 1), title = paste0("N = ", nobs(regression5)))


# -----------------------------------------------------------------------------
# 8. Differential trends based on party (unconditional loyalty)
# -----------------------------------------------------------------------------

message("\nParty Congress trends, unconditional loyalty\n")

regression6 <- feols(
  unconditional_loyalty ~ term + is_leader
  | bioguide_id + congress + congress^party, data = data, cluster = ~bioguide_id
)

p6 <- norm_coef_plot(regression6, ylim = c(-1, 3.5), ybreaks = seq(-1, 5, by = 1), title = paste0("N = ", nobs(regression6)))


# -----------------------------------------------------------------------------
# 9. Drop early congresses with no later-career members
# -----------------------------------------------------------------------------

message("\nDrop early congresses with no later-career members\n")

later_congresses <- data |>
  filter(congress >= 104) # drop early Congresses with no later-career members

regression7 <- feols(
  overall_loyalty ~ term + is_leader
  | bioguide_id + congress, data = later_congresses, cluster = ~bioguide_id
)

p7 <- norm_coef_plot(regression7, ylim = c(-0.5, 2), ybreaks = seq(0, 2, by = 1), title = paste0("N = ", nobs(regression7)))


# -----------------------------------------------------------------------------
# 10. Example of bad linear fit
# -----------------------------------------------------------------------------

message("\nBad fit\n")

regression8 <- feols(
  overall_loyalty ~ term + is_leader
  | bioguide_id + congress, data = data, cluster = ~bioguide_id
)

p8 <- norm_coef_plot(regression8, ylim = c(-1, 1), ybreaks = seq(-1, 1, by = 1), title = paste0("N = ", nobs(regression8)), trend = 1:PLOT_MAX_TERM)


# -----------------------------------------------------------------------------
# 11. Save plots
# -----------------------------------------------------------------------------

ggsave("empirical analysis/plots/overall_loyalty.pdf", plot = p1, width = 9, height = 4.5)
ggsave("empirical analysis/plots/partisan_loyalty.pdf", plot = p2, width = 9, height = 4.5)
ggsave("empirical analysis/plots/uncon_loyalty.pdf", plot = p3, width = 9, height = 4.5)
ggsave("empirical analysis/plots/party_trends_overall.pdf", plot = p4, width = 9, height = 4.5)
ggsave("empirical analysis/plots/party_trends_partisan.pdf", plot = p4, width = 9, height = 4.5)
ggsave("empirical analysis/plots/party_trends_uncon.pdf", plot = p6, width = 9, height = 4.5)
ggsave("empirical analysis/plots/later_congresses.pdf", plot = p7, width = 9, height = 4.5)
ggsave("empirical analysis/plots/bad_fit.pdf", plot = p8, width = 9, height = 4.5)