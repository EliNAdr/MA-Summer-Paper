# =============================================================================
# Script 07: FERS annuities plot
#
# OUTPUT:
#   Plot of FERS annuity vs completed terms in office
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Load required packages
# -----------------------------------------------------------------------------

library(ggplot2)
library(scales)


# -----------------------------------------------------------------------------
# 1. Plot annuities and save
# -----------------------------------------------------------------------------

annuity <- matrix(0, nrow = 20, ncol = 2)

for (t in 1:20){
    annuity[t,1] <- t
}

for (t in 3:10){
    annuity[t,2] <- 174000*0.017*2*t
}

for (t in 11:20){
    annuity[t,2] <- annuity[10,2] + 174000*0.01*2*(t-10)
}

annuity <- as.data.frame(annuity)
colnames(annuity) <- c("term", "annuity")

ggplot(annuity, aes(x = term, y = annuity)) +
    geom_vline(xintercept = 3, linetype = 5, colour = "navy") +
    geom_line() +
    geom_point(size = 2.5) +
    coord_cartesian(ylim = c(0, 100000)) +
    scale_y_continuous(label = dollar, breaks = seq(0,100000, by = 25000)) +
    scale_x_continuous(breaks = seq(1,20, by = 1)) +
    labs(
        x = "Term", y = "Pension Annuity\n") +
    theme_bw()

ggsave("annuities plot/annuity_plot.pdf", width = 9, height = 4.5)