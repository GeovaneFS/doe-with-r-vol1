# Solutions for the exercises of Chapter 3
# Run from the repository root: source("solutions/ch03_solutions.R")
invisible(capture.output(source("scripts/ch03_diagnostics.R")))

# ---- Exercise 1: constant standard deviation of 3 g ----
set.seed(301)
pots_c <- tibble(dose = factor(rep(names(doses), each = 8)))
pots_c$mass <- rnorm(nrow(pots_c), mean = unname(doses[as.character(pots_c$dose)]), sd = 3)
fit_c <- lm(mass ~ dose, data = pots_c)
ggplot(tibble(fitted = fitted(fit_c), resid = resid(fit_c)), aes(fitted, resid)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_point() +
  labs(x = "Fitted values (g)", y = "Residuals (g)")
car::leveneTest(mass ~ dose, data = pots_c, center = median)

# ---- Exercise 2: one extreme value ----
pots_out <- pots
pots_out$mass[1] <- 60                      # a pot of dose D0, whose mean is about 10 g
fit_out <- lm(mass ~ dose, data = pots_out)
cd_out <- cooks.distance(fit_out)
c(max_cooks = max(cd_out), threshold = 4 / nrow(pots_out))
which.max(cd_out)                            # the pot we changed
rbind(before = car::Anova(fit, type = 2)[1, c("F value", "Pr(>F)")],
      after  = car::Anova(fit_out, type = 2)[1, c("F value", "Pr(>F)")])

# ---- Exercise 3: intervals of the dose means under three approaches ----
ci_lm  <- as.data.frame(confint(emmeans(fit, ~ dose)))
ci_log <- as.data.frame(confint(emmeans(fit_log, ~ dose), type = "response"))
ci_gls <- as.data.frame(confint(emmeans(fit_gls, ~ dose)))
tibble(dose = ci_lm$dose,
       width_lm  = ci_lm$upper.CL  - ci_lm$lower.CL,
       width_log = ci_log$upper.CL - ci_log$lower.CL,
       width_gls = ci_gls$upper.CL - ci_gls$lower.CL)

# ---- Exercise 4: detection of the funnel with 3 pots per dose, 200 times ----
set.seed(304)
detect <- t(replicate(200, {
  d <- tibble(dose = factor(rep(names(doses), each = 3)))
  m <- unname(doses[as.character(d$dose)])
  d$mass <- rnorm(nrow(d), m, 0.15 * m)
  f <- lm(mass ~ dose, data = d)
  c(levene  = car::leveneTest(mass ~ dose, data = d, center = median)$`Pr(>F)`[1],
    shapiro = shapiro.test(resid(f))$p.value)
}))
colMeans(detect < 0.05)
