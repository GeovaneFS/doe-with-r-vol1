# Chapter 3. Model diagnostics and remedies
# Planning and Analysis of Experiments with R, Volume 1.
# Run this script from the repository root, for example with
#   source("scripts/ch03_diagnostics.R")
# The setup file loads the packages and the plot theme used in the book.

# ---- setup -------------------------------------------------------
source("R/setup.R")
library(car)       # Levene test, Anova()
library(emmeans)
library(nlme)      # gls() for unequal variances
library(MASS)      # boxcox(); we call it with MASS:: to avoid masking dplyr::select
set.seed(303)

# ---- simulate ----------------------------------------------------

doses <- c(D0 = 10, D1 = 14, D2 = 20, D3 = 28)     # true mean dry mass (g)

pots <- tibble(dose = factor(rep(names(doses), each = 8)))

# The standard deviation is 15% of the mean, so larger means come with larger spread.
mu <- unname(doses[as.character(pots$dose)])
pots$mass <- rnorm(nrow(pots), mean = mu, sd = 0.15 * mu)

fit <- lm(mass ~ dose, data = pots)
car::Anova(fit, type = 2)

# ---- base-diagnostics --------------------------------------------

par(mfrow = c(2, 2))    # arrange the four plots in a 2 x 2 grid
plot(fit)
par(mfrow = c(1, 1))    # restore the default layout

# ---- gg-diagnostics ----------------------------------------------

diag_df <- tibble(
  fitted    = fitted(fit),
  resid     = resid(fit),
  std_resid = rstandard(fit)         # residuals divided by their estimated standard deviation
)

p1 <- ggplot(diag_df, aes(fitted, resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point() +
  labs(x = "Fitted values (g)", y = "Residuals (g)")

p2 <- ggplot(diag_df, aes(sample = std_resid)) +
  stat_qq() + stat_qq_line() +
  labs(x = "Theoretical quantiles", y = "Standardized residuals")

# patchwork is optional. If installed, it places the plots side by side.
if (requireNamespace("patchwork", quietly = TRUE)) {
  patchwork::wrap_plots(p1, p2, ncol = 2)
} else {
  print(p1); print(p2)
}

# ---- levene ------------------------------------------------------

car::leveneTest(mass ~ dose, data = pots, center = median)

# ---- shapiro -----------------------------------------------------

shapiro.test(resid(fit))

# ---- cook --------------------------------------------------------

cd <- cooks.distance(fit)

tibble(pot = seq_along(cd), cooks = cd) |>
  ggplot(aes(pot, cooks)) +
  geom_col() +
  geom_hline(yintercept = 4 / length(cd), linetype = 2) +
  labs(x = "Pot", y = "Cook's distance")

# ---- boxcox ------------------------------------------------------

bc <- MASS::boxcox(fit, lambda = seq(-1, 2, 0.05), plotit = TRUE)
lambda_hat <- bc$x[which.max(bc$y)]     # lambda with the highest likelihood
round(lambda_hat, 2)

# ---- log-fit -----------------------------------------------------

fit_log <- lm(log(mass) ~ dose, data = pots)

ggplot(tibble(f = fitted(fit_log), r = resid(fit_log)), aes(f, r)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point() +
  labs(x = "Fitted log(mass)", y = "Residuals")

car::leveneTest(log(mass) ~ dose, data = pots, center = median)

# ---- gls ---------------------------------------------------------

# Each dose gets its own residual standard deviation
fit_gls <- gls(mass ~ dose, data = pots,
               weights = varIdent(form = ~ 1 | dose))

fit_equal <- gls(mass ~ dose, data = pots)       # same model with constant variance

anova(fit_equal, fit_gls)     # likelihood-ratio test: is a separate variance per dose needed?

# ---- gls-emmeans -------------------------------------------------

emmeans(fit_gls, ~ dose)      # means keep the original scale, and their standard errors differ by dose

# ---- save-data -----------------------------------------------
# Saves the simulated datasets of this chapter to data/.
dir.create("data", showWarnings = FALSE)
if (exists("pots")) write.csv(pots, "data/ch03_pots.csv", row.names = FALSE)
