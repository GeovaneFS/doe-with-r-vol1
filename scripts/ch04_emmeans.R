# Chapter 4. Comparing treatments with emmeans
# Planning and Analysis of Experiments with R, Volume 1.
# Run this script from the repository root, for example with
#   source("scripts/ch04_emmeans.R")
# The setup file loads the packages and the plot theme used in the book.

# ---- setup -------------------------------------------------------
source("R/setup.R")
library(car)
library(emmeans)
set.seed(404)

# ---- simulate ----------------------------------------------------

true_means <- c(Control = 50, A = 53, B = 58, C = 59, D = 50.5)

resin <- tibble(
  formulation = factor(rep(names(true_means), each = 8),
                       levels = names(true_means))      # Control first, so it is the reference
)
resin$strength <- rnorm(nrow(resin),
                        mean = unname(true_means[as.character(resin$formulation)]),
                        sd = 3)

fit <- lm(strength ~ formulation, data = resin)
car::Anova(fit, type = 2)

emm <- emmeans(fit, ~ formulation)
emm

# ---- tukey -------------------------------------------------------

pairs(emm, adjust = "tukey")

# ---- tukey-ci ----------------------------------------------------

tukey_ci <- confint(pairs(emm, adjust = "tukey"))
tukey_ci

# ---- tukey-plot --------------------------------------------------

ggplot(as.data.frame(tukey_ci), aes(x = estimate, y = reorder(contrast, estimate))) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange(aes(xmin = lower.CL, xmax = upper.CL)) +
  labs(x = "Difference in strength (MPa)", y = NULL)

# ---- dunnett -----------------------------------------------------

contrast(emm, method = "trt.vs.ctrl", ref = "Control")     # Dunnett adjustment is the default here

# ---- dunnett-ci --------------------------------------------------

confint(contrast(emm, method = "trt.vs.ctrl", ref = "Control"))

# ---- fdr ---------------------------------------------------------

pairs(emm, adjust = "fdr")

# ---- contrasts ---------------------------------------------------

# Weights follow the order of the levels: Control, A, B, C, D
planned <- list(
  "New (A,B,C) vs Control" = c(-1, 1/3, 1/3, 1/3, 0),
  "D vs Control"           = c(-1, 0, 0, 0, 1),
  "B vs C"                 = c(0, 0, 1, -1, 0)
)

contrast(emm, method = planned, adjust = "holm")     # Holm adjustment for these three planned questions

# ---- regrid ------------------------------------------------------

fit_log <- lm(log(strength) ~ formulation, data = resin)
emm_log <- emmeans(fit_log, ~ formulation)

summary(emm_log, type = "response")                     # geometric means on the original scale
pairs(emm_log, type = "response", adjust = "tukey")     # ratios between formulations

# ---- emm-plot ----------------------------------------------------

ggplot(as.data.frame(emm), aes(x = formulation, y = emmean)) +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL)) +
  labs(x = "Formulation", y = "Estimated mean strength (MPa)")

# ---- save-data -----------------------------------------------
# Saves the simulated datasets of this chapter to data/.
dir.create("data", showWarnings = FALSE)
if (exists("resin")) write.csv(resin, "data/ch04_resin.csv", row.names = FALSE)
