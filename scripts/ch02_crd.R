# Chapter 2. The completely randomized design
# Planning and Analysis of Experiments with R, Volume 1.
# Run this script from the repository root, for example with
#   source("scripts/ch02_crd.R")
# The setup file loads the packages and the plot theme used in the book.

# ---- setup -------------------------------------------------------
source("R/setup.R")
library(car)       # Anova() with Type II and Type III sums of squares
library(emmeans)   # estimated marginal means
set.seed(202)      # reproducible random numbers for this chapter

# ---- simulate ----------------------------------------------------

# True mean tensile strength of each formulation (MPa). In a real study we would not know these.
true_means <- c(F1 = 52, F2 = 55, F3 = 59, F4 = 60)

resin <- tibble(
  formulation = factor(rep(names(true_means), each = 6))   # 6 bars per formulation, stored as a factor
)

# Strength = true mean of the formulation + random error (standard deviation 3 MPa)
resin$strength <- rnorm(nrow(resin),
                        mean = unname(true_means[as.character(resin$formulation)]),
                        sd = 3)

# Randomized testing order, as in a real CRD
resin$test_order <- sample(nrow(resin))

head(resin)

# ---- explore -----------------------------------------------------

ggplot(resin, aes(x = formulation, y = strength)) +
  geom_boxplot(outlier.shape = NA, fill = "grey92") +          # summary of each group
  geom_jitter(width = 0.08, height = 0, size = 2) +            # every observation
  labs(x = "Formulation", y = "Tensile strength (MPa)")

# ---- fit ---------------------------------------------------------

fit <- lm(strength ~ formulation, data = resin)
summary(fit)

# ---- design-matrix-treatment -------------------------------------

# One row per formulation (rows 1, 7, 13 and 19 are the first bar of F1, F2, F3 and F4)
mm_trt <- model.matrix(~ formulation, data = resin,
                       contrasts.arg = list(formulation = "contr.treatment"))
mm_trt[c(1, 7, 13, 19), ]

# ---- design-matrix-sum -------------------------------------------

mm_sum <- model.matrix(~ formulation, data = resin,
                       contrasts.arg = list(formulation = "contr.sum"))
mm_sum[c(1, 7, 13, 19), ]

# ---- anova -------------------------------------------------------

anova_tab <- car::Anova(fit, type = 2)    # Type II sums of squares
anova_tab

# ---- eta-squared -------------------------------------------------

ss <- anova_tab$"Sum Sq"                    # first row = formulation, second row = residuals
eta_sq <- ss[1] / sum(ss)                   # proportion of total variation due to formulation
round(eta_sq, 3)

# ---- report-numbers ----------------------------------------------

F_val <- round(anova_tab$"F value"[1], 2)
df1   <- anova_tab$Df[1]
df2   <- anova_tab$Df[2]
p_txt <- format.pval(anova_tab$"Pr(>F)"[1], digits = 2, eps = 0.001)

# ---- emmeans-first -----------------------------------------------

emm <- emmeans(fit, ~ formulation)    # one estimated mean per formulation
emm

# ---- emmeans-plot ------------------------------------------------

emm_df <- as.data.frame(emm)          # convert to a data frame for ggplot2

ggplot(emm_df, aes(x = formulation, y = emmean)) +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL)) +
  labs(x = "Formulation", y = "Estimated mean strength (MPa)")

# ---- missing -----------------------------------------------------

resin_na <- resin
resin_na$strength[c(3, 11, 20)] <- NA     # three bars were lost

# lm() drops rows with a missing response automatically.
fit_na <- lm(strength ~ formulation, data = resin_na)

nobs(fit_na)                               # number of observations actually used
df.residual(fit_na)                        # residual degrees of freedom (one lost per missing value)
table(resin_na$formulation[!is.na(resin_na$strength)])   # the layout is now unbalanced

# ---- missing-anova -----------------------------------------------

car::Anova(fit_na, type = 2)
emmeans(fit_na, ~ formulation)

# ---- save-data -----------------------------------------------
# Saves the simulated datasets of this chapter to data/.
dir.create("data", showWarnings = FALSE)
if (exists("resin")) write.csv(resin, "data/ch02_resin.csv", row.names = FALSE)
