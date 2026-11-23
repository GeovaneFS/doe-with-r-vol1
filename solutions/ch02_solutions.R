# Solutions for the exercises of Chapter 2
# Run from the repository root: source("solutions/ch02_solutions.R")
invisible(capture.output(source("scripts/ch02_crd.R")))

# ---- Exercise 1: larger noise (sd = 6) ----
# Same seed, so the new errors are exactly twice the old ones. The treatment means
# do not change, only the noise grows.
simulate_resin <- function(sd, seed = 202) {
  set.seed(seed)
  d <- tibble(formulation = factor(rep(names(true_means), each = 6)))
  d$strength <- rnorm(nrow(d), mean = unname(true_means[as.character(d$formulation)]), sd = sd)
  d
}
res <- lapply(c(3, 6), function(s) {
  d <- simulate_resin(s)
  m <- lm(strength ~ formulation, data = d)
  list(F = car::Anova(m, type = 2)$`F value`[1],
       ci_width = mean(diff(t(confint(emmeans(m, ~ formulation))[, c("lower.CL", "upper.CL")]))))
})
tibble(sd = c(3, 6), F_value = sapply(res, `[[`, "F"), mean_ci_width = sapply(res, `[[`, "ci_width"))

# ---- Exercise 2: keep only one bar of one formulation ----
keep <- !(resin$formulation == "F4" & resin$test_order != min(resin$test_order[resin$formulation == "F4"]))
resin_one <- resin[keep, ]
table(resin_one$formulation)
fit_one <- lm(strength ~ formulation, data = resin_one)
confint(emmeans(fit_one, ~ formulation))

# ---- Exercise 3: coefficients versus estimated marginal means ----
coef(fit)                                  # intercept = mean of F1; others = difference to F1
as.data.frame(emm)[, c("formulation", "emmean")]
cf <- coef(fit); V <- vcov(fit)
L <- c(0, -1, 1, 0)                        # F3 - F2 = b3 - b2
c(estimate = sum(L * cf), se = sqrt(drop(t(L) %*% V %*% L)))
subset(as.data.frame(pairs(emm)), contrast == "F2 - F3")   # emmeans, one line (sign reversed: F2 - F3)

# ---- Exercise 4: three random missing bars, 500 times ----
set.seed(204)
f3_estimate <- replicate(500, {
  d <- resin
  d$strength[sample(nrow(d), 3)] <- NA
  m <- lm(strength ~ formulation, data = d)
  predict(m, newdata = data.frame(formulation = "F3"))
})
c(full_data = unname(coef(fit)[1] + coef(fit)[3]),
  mean_with_missing = mean(f3_estimate),
  sd_with_missing = sd(f3_estimate))
