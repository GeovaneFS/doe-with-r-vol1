# Solutions for the exercises of Chapter 1
# Run from the repository root: source("solutions/ch01_solutions.R")
# The chapter script is sourced silently first, so every object of the chapter exists.
invisible(capture.output(source("scripts/ch01_foundations.R")))

# ---- Exercise 1: strength of the link between soil quality and fertilizer ----
obs_estimate <- function(k, n = 200, true_effect = 2) {
  soil <- rnorm(n)
  fert <- rbinom(n, 1, plogis(k * soil))
  y <- 50 + true_effect * fert + 4 * soil + rnorm(n, 0, 3)
  mean(y[fert == 1]) - mean(y[fert == 0])
}
set.seed(101)
ks <- c(0, 1.5, 3)
bias_table <- tibble(k = ks,
                     mean_estimate = sapply(ks, function(k) mean(replicate(2000, obs_estimate(k)))),
                     true_effect = 2)
bias_table   # k = 0 is close to 2; larger k inflates the estimate

# ---- Exercise 2: smaller variation between pots ----
false_positive <- function(sd_pot, B = 2000) {
  p <- replicate(B, {
    d <- simulate_pots(effect = 0, sd_pot = sd_pot)
    summary(lm(height ~ treatment, data = d))$coefficients[2, 4]
  })
  mean(p < 0.05)
}
set.seed(102)
tibble(sd_pot = c(3, 1, 0.5, 0),
       wrong_analysis_false_positive = sapply(sd_pot, false_positive))

# ---- Exercise 3: sample size for 90% power ----
n80 <- pwr.t.test(d = 5 / 4, sig.level = 0.05, power = 0.80, type = "two.sample")$n
n90 <- pwr.t.test(d = 5 / 4, sig.level = 0.05, power = 0.90, type = "two.sample")$n
c(n_power_80 = ceiling(n80), n_power_90 = ceiling(n90),
  additional_per_group = ceiling(n90) - ceiling(n80))

# ---- Exercise 4: a messy spreadsheet and its tidy version ----
messy_own <- tibble(
  sample = 1:5,
  `Treatment 1` = c(12.1, 11.8, 12.6, 13.0, 12.2),
  `Treatment 2` = c(14.2, 13.9, 14.8, 15.1, 14.0)
)
tidy_own <- messy_own |>
  pivot_longer(cols = -sample, names_to = "treatment", values_to = "response") |>
  mutate(treatment = factor(treatment))
tidy_own
str(tidy_own)   # sample is numeric, treatment is a factor, response is numeric
