# Chapter 5. The randomized complete block design
# Planning and Analysis of Experiments with R, Volume 1.
# Run this script from the repository root, for example with
#   source("scripts/ch05_rcbd.R")
# The setup file loads the packages and the plot theme used in the book.

# ---- setup -------------------------------------------------------
source("R/setup.R")
library(car)
library(emmeans)
library(lme4)
library(lmerTest)   # adds p-values and Satterthwaite degrees of freedom to lmer()
set.seed(505)

# ---- simulate ----------------------------------------------------

temps  <- c(T60 = 12.0, T70 = 10.5, T80 = 9.0, T90 = 8.4, T100 = 8.0)   # true mean moisture (%)
blocks <- paste0("Day", 1:6)

rcbd <- expand_grid(
  day  = factor(blocks),
  temp = factor(names(temps), levels = names(temps))
)

# Each day shifts everything up or down by a random amount (the block effect).
day_effect <- rnorm(length(blocks), mean = 0, sd = 1.5)
names(day_effect) <- blocks

rcbd$moisture <- unname(temps[as.character(rcbd$temp)]) +
                 unname(day_effect[as.character(rcbd$day)]) +
                 rnorm(nrow(rcbd), mean = 0, sd = 0.5)      # unit-level error

head(rcbd, 8)

# ---- block-plot --------------------------------------------------

ggplot(rcbd, aes(x = temp, y = moisture, group = day, colour = day)) +
  geom_line() + geom_point() +
  labs(x = "Drying temperature", y = "Moisture (%)", colour = "Block")

# ---- fit ---------------------------------------------------------

fit <- lm(moisture ~ day + temp, data = rcbd)
car::Anova(fit, type = 2)

# ---- ignore-blocks -----------------------------------------------

fit_crd <- lm(moisture ~ temp, data = rcbd)     # the same data analyzed as if there were no blocks
car::Anova(fit_crd, type = 2)

# ---- efficiency --------------------------------------------------

mse_rcbd <- sigma(fit)^2
mse_crd  <- sigma(fit_crd)^2
round(c(RCBD = mse_rcbd, ignoring_blocks = mse_crd, ratio = mse_crd / mse_rcbd), 2)

# ---- emmeans -----------------------------------------------------

emm <- emmeans(fit, ~ temp)
emm
pairs(emm, adjust = "tukey")

# ---- lmer --------------------------------------------------------

fit_mixed <- lmer(moisture ~ temp + (1 | day), data = rcbd)    # (1 | day) = random intercept for each day
anova(fit_mixed)                                               # F-test for temp with Satterthwaite df

# ---- lmer-emmeans ------------------------------------------------

emmeans(fit_mixed, ~ temp)
VarCorr(fit_mixed)       # variance among days and residual variance

# ---- missing-setup -----------------------------------------------

# Delete one observation: Day3, T80
rcbd_na <- rcbd
miss <- which(rcbd_na$day == "Day3" & rcbd_na$temp == "T80")
true_value <- rcbd_na$moisture[miss]      # keep the true value for comparison
rcbd_na$moisture[miss] <- NA

# Modern route: fit on the observed data only
fit_obs <- lm(moisture ~ day + temp, data = rcbd_na)    # lm() drops the NA row
emm_obs <- emmeans(fit_obs, ~ temp)

# ---- missing-yates -----------------------------------------------

# Classical route: Yates' formula
t_n <- nlevels(rcbd_na$temp)     # number of treatments
r_n <- nlevels(rcbd_na$day)      # number of blocks

Btot <- sum(rcbd_na$moisture[rcbd_na$day  == "Day3"], na.rm = TRUE)   # observed total of the block
Ttot <- sum(rcbd_na$moisture[rcbd_na$temp == "T80"],  na.rm = TRUE)   # observed total of the treatment
Gtot <- sum(rcbd_na$moisture, na.rm = TRUE)                           # observed grand total

x_hat <- (r_n * Btot + t_n * Ttot - Gtot) / ((r_n - 1) * (t_n - 1))

c(true_value = true_value, yates_estimate = x_hat)

# ---- missing-compare ---------------------------------------------

# Fill in the estimated value and analyze the filled data
rcbd_fill <- rcbd_na
rcbd_fill$moisture[miss] <- x_hat
fit_fill <- lm(moisture ~ day + temp, data = rcbd_fill)

# Treatment means from the filled data (plain averages) versus emmeans on the observed data
means_filled <- tapply(rcbd_fill$moisture, rcbd_fill$temp, mean)
comparison <- tibble(
  temp               = names(means_filled),
  filled_data_mean   = as.numeric(means_filled),
  emmeans_observed   = summary(emm_obs)$emmean
)
comparison

# The error mean squares agree after correcting the degrees of freedom
c(mse_observed = sigma(fit_obs)^2,
  mse_filled_corrected = sum(resid(fit_fill)^2) / df.residual(fit_obs))

# ---- missing-lmer ------------------------------------------------

fit_mixed_na <- lmer(moisture ~ temp + (1 | day), data = rcbd_na)
anova(fit_mixed_na)
emmeans(fit_mixed_na, ~ temp)

# ---- save-data -----------------------------------------------
# Saves the simulated datasets of this chapter to data/.
dir.create("data", showWarnings = FALSE)
if (exists("rcbd")) write.csv(rcbd, "data/ch05_rcbd.csv", row.names = FALSE)
