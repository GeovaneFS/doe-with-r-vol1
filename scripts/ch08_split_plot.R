# Chapter 8. Split-plot designs as mixed models
# Planning and Analysis of Experiments with R, Volume 1.
# Run this script from the repository root, for example with
#   source("scripts/ch08_split_plot.R")
# The setup file loads the packages and the plot theme used in the book.

# ---- setup -------------------------------------------------------
source("R/setup.R")
library(car)
library(emmeans)
library(lme4)
library(lmerTest)    # p-values and Satterthwaite degrees of freedom
set.seed(808)
options(contrasts = c("contr.sum", "contr.poly"))

# ---- simulate ----------------------------------------------------

irrigation <- factor(c("Low", "Medium", "High"), levels = c("Low", "Medium", "High"))
variety    <- factor(c("V1", "V2", "V3", "V4"))
blocks     <- factor(paste0("B", 1:4))

sp <- expand_grid(block = blocks, irrigation = irrigation, variety = variety) |>
  mutate(whole_plot = factor(paste(block, irrigation, sep = "_")))    # each block x irrigation = one whole plot

# Random effects: block effect and whole-plot error (larger than subplot error)
block_eff <- rnorm(nlevels(blocks), sd = 0.8)
names(block_eff) <- levels(blocks)

wp_eff <- rnorm(nlevels(sp$whole_plot), sd = 1.2)         # whole-plot error
names(wp_eff) <- levels(sp$whole_plot)

# Fixed effects: irrigation, variety and a small interaction
irr_eff <- c(Low = 0, Medium = 2.0, High = 3.0)
var_eff <- c(V1 = 0, V2 = 1.0, V3 = 2.5, V4 = 1.5)
int_eff <- ifelse(sp$irrigation == "High" & sp$variety == "V3", 1.5, 0)    # V3 gains more under high irrigation

sp <- sp |>
  mutate(
    yield = 20 + irr_eff[as.character(irrigation)] + var_eff[as.character(variety)] + int_eff +
            block_eff[as.character(block)] + wp_eff[as.character(whole_plot)] +
            rnorm(n(), sd = 0.7)                           # subplot error
  )

head(sp)

# ---- fit ---------------------------------------------------------

fit <- lmer(yield ~ irrigation * variety + (1 | block) + (1 | block:irrigation), data = sp)

# ---- anova -------------------------------------------------------

anova(fit)     # Type III tests with Satterthwaite degrees of freedom (lmerTest)

# ---- anova-kr ----------------------------------------------------

anova(fit, ddf = "Kenward-Roger")      # Kenward-Roger is a good alternative for small designs

# ---- varcorr -----------------------------------------------------

VarCorr(fit)       # variance among blocks, among whole plots, and among subplots (residual)

# ---- wrong -------------------------------------------------------

fit_wrong <- lm(yield ~ block + irrigation * variety, data = sp)     # treats all 48 observations as independent
car::Anova(fit_wrong, type = 3)

# ---- interaction -------------------------------------------------

emm_int <- emmeans(fit, ~ variety | irrigation)
emm_int

# ---- emmip -------------------------------------------------------

emmip(fit, variety ~ irrigation, CIs = TRUE) +
  labs(x = "Irrigation", y = "Estimated mean yield", colour = "Variety")

# ---- simple-effects ----------------------------------------------

pairs(emm_int, adjust = "tukey")      # Tukey within each irrigation level

# ---- irrigation-means --------------------------------------------

emm_irr <- emmeans(fit, ~ irrigation)       # averaged over varieties
emm_irr
pairs(emm_irr, adjust = "tukey")

# ---- missing -----------------------------------------------------

sp_na <- sp
sp_na$yield[c(5, 17, 30)] <- NA          # three subplots lost

fit_na <- lmer(yield ~ irrigation * variety + (1 | block) + (1 | block:irrigation), data = sp_na)
anova(fit_na)
emmeans(fit_na, ~ irrigation)

# ---- diagnostics -------------------------------------------------

diag_df <- tibble(fitted = fitted(fit), resid = resid(fit))

p1 <- ggplot(diag_df, aes(fitted, resid)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_point() +
  labs(x = "Fitted values", y = "Residuals")

p2 <- ggplot(diag_df, aes(sample = resid)) +
  stat_qq() + stat_qq_line() +
  labs(x = "Theoretical quantiles", y = "Residuals")

if (requireNamespace("patchwork", quietly = TRUE)) {
  patchwork::wrap_plots(p1, p2, ncol = 2)
} else {
  print(p1); print(p2)
}

# ---- ranef-check -------------------------------------------------

wp_re <- ranef(fit)$"block:irrigation"
qqnorm(wp_re[, 1], main = "")                 # random effects at the whole-plot level
qqline(wp_re[, 1])

# ---- save-data -----------------------------------------------
# Saves the simulated datasets of this chapter to data/.
dir.create("data", showWarnings = FALSE)
if (exists("sp")) write.csv(sp, "data/ch08_sp.csv", row.names = FALSE)
