# Chapter 1. Foundations of experimental design and data structuring
# Planning and Analysis of Experiments with R, Volume 1.
# Run this script from the repository root, for example with
#   source("scripts/ch01_foundations.R")
# The setup file loads the packages and the plot theme used in the book.

# ---- setup -------------------------------------------------------

# Chunk options and packages used throughout the chapter.

library(ggplot2)   # graphics
library(dplyr)     # data manipulation
library(tidyr)     # reshaping tables
library(pwr)       # power and sample size

theme_set(theme_minimal(base_size = 12))

# A fixed seed makes every random number in this chapter reproducible.
# If you run the code with the same seed, you will get the same results as the book.
set.seed(2026)

# ---- confounding -------------------------------------------------

n <- 200                                   # number of plots
true_effect <- 2                           # the real effect of the fertilizer
soil_quality <- rnorm(n, mean = 0, sd = 1) # hidden soil quality (higher is better)

# Observational study: farmers fertilize good soil more often.
# plogis() turns soil quality into a probability between 0 and 1.
fert_obs <- rbinom(n, size = 1, prob = plogis(1.5 * soil_quality))

# Experiment: the investigator flips a fair coin for each plot.
# We shuffle exactly half "0" and half "1" so the groups have equal size.
fert_exp <- sample(rep(c(0, 1), each = n / 2))

# Yield depends on the fertilizer, on the soil, and on random noise.
make_yield <- function(fert) 50 + true_effect * fert + 4 * soil_quality + rnorm(n, 0, 3)

comparison <- bind_rows(
  tibble(study = "Observational", fert = fert_obs, yield = make_yield(fert_obs)),
  tibble(study = "Randomized experiment", fert = fert_exp, yield = make_yield(fert_exp))
)

# Estimated effect = mean yield with fertilizer minus mean yield without it.
comparison |>
  group_by(study, fert) |>
  summarise(mean_yield = mean(yield), .groups = "drop") |>
  pivot_wider(names_from = fert, values_from = mean_yield, names_prefix = "fert_") |>
  mutate(estimated_effect = fert_1 - fert_0, true_effect = true_effect) |>
  knitr::kable(digits = 2)

# ---- randomization -----------------------------------------------

samples <- tibble(sample_id = 1:12)

# rep(..., each = 4) builds the list of labels (4 of each coating).
# sample() shuffles that list, which is the random assignment.
samples$coating <- sample(rep(c("Coating A", "Coating B", "Coating C"), each = 4))

samples

# ---- replication -------------------------------------------------

# One simulated experiment with n units per group.
# Group 1 has mean 0, group 2 has mean delta, and both have standard deviation sigma.
one_experiment <- function(n, delta = 2, sigma = 4) {
  g1 <- rnorm(n, mean = 0, sd = sigma)
  g2 <- rnorm(n, mean = delta, sd = sigma)
  mean(g2) - mean(g1)                      # estimated difference
}

group_sizes <- c(3, 5, 10, 20, 40)

replication_demo <- bind_rows(lapply(group_sizes, function(n) {
  tibble(n = n, estimate = replicate(1000, one_experiment(n)))
}))

ggplot(replication_demo, aes(x = factor(n), y = estimate)) +
  geom_boxplot(fill = "grey85") +
  geom_hline(yintercept = 2, linetype = "dashed") +
  labs(x = "Replicates per group", y = "Estimated difference between groups")

# ---- pots-function -----------------------------------------------

# Simulate a pot experiment.
#   effect   = true fertilizer effect on plant height
#   n_pots   = pots per treatment
#   n_plants = plants measured per pot
#   sd_pot   = variation between pots (shared by all plants in a pot)
#   sd_plant = variation between plants within the same pot
simulate_pots <- function(effect = 0, n_pots = 3, n_plants = 5, sd_pot = 3, sd_plant = 2) {
  d <- expand.grid(plant = 1:n_plants, pot = 1:(2 * n_pots)) |> as_tibble()

  # Pots 1 to n_pots are controls, the rest receive fertilizer.
  d$treatment <- factor(ifelse(d$pot <= n_pots, "Control", "Fertilizer"),
                        levels = c("Control", "Fertilizer"))

  pot_shift <- rnorm(2 * n_pots, mean = 0, sd = sd_pot)   # one random value per pot

  d$height <- 30 +
    ifelse(d$treatment == "Fertilizer", effect, 0) +
    pot_shift[d$pot] +                                    # same shift for all plants of a pot
    rnorm(nrow(d), mean = 0, sd = sd_plant)               # plant-to-plant noise
  d
}

# One dataset in which the fertilizer does NOT work (effect = 0).
pots <- simulate_pots(effect = 0)

# ---- pots-plot ---------------------------------------------------

ggplot(pots, aes(x = treatment, y = height, color = factor(pot))) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  labs(x = NULL, y = "Plant height (cm)", color = "Pot")

# ---- pots-two-analyses -------------------------------------------

# Wrong: 30 plants treated as 30 independent replicates.
wrong_fit <- lm(height ~ treatment, data = pots)

# Right: one number per pot (the experimental unit).
pot_means <- pots |>
  group_by(pot, treatment) |>
  summarise(height = mean(height), .groups = "drop")
right_fit <- lm(height ~ treatment, data = pot_means)

# p-value for the treatment effect in each analysis.
p_wrong <- summary(wrong_fit)$coefficients["treatmentFertilizer", "Pr(>|t|)"]
p_right <- summary(right_fit)$coefficients["treatmentFertilizer", "Pr(>|t|)"]

c(wrong_analysis = p_wrong, right_analysis = p_right)

# ---- pots-false-positives ----------------------------------------

one_run <- function() {
  d <- simulate_pots(effect = 0)                      # no true effect
  m <- d |> group_by(pot, treatment) |> summarise(height = mean(height), .groups = "drop")
  c(
    wrong = summary(lm(height ~ treatment, data = d))$coefficients[2, 4],
    right = summary(lm(height ~ treatment, data = m))$coefficients[2, 4]
  )
}

p_values <- t(replicate(2000, one_run()))

# Proportion of experiments declared "significant" when nothing is going on.
false_positive_rate <- colMeans(p_values < 0.05)
round(false_positive_rate, 3)

# ---- power-two-groups --------------------------------------------

delta <- 5          # smallest difference that matters (in measurement units)
sigma <- 4          # expected standard deviation among units of the same treatment
d <- delta / sigma  # standardized effect size (Cohen's d)

# Solve for the sample size per group that gives 80% power.
two_groups <- pwr.t.test(d = d, sig.level = 0.05, power = 0.80,
                         type = "two.sample", alternative = "two.sided")
two_groups

# ---- power-anova -------------------------------------------------

# Hypothetical true means for four treatments, and the within-treatment standard deviation.
mu    <- c(50, 52, 55, 58)
sigma <- 4

# Cohen's f = (standard deviation of the treatment means) / sigma.
# We use the population formula (divide by k, not by k - 1).
f <- sqrt(mean((mu - mean(mu))^2)) / sigma

four_groups <- pwr.anova.test(k = length(mu), f = f, sig.level = 0.05, power = 0.80)
four_groups

# ---- power-simulation --------------------------------------------

n_per_group <- ceiling(two_groups$n)

# Run the experiment 5000 times and record whether p < 0.05 each time.
detected <- replicate(5000, {
  coating_1 <- rnorm(n_per_group, mean = 0,     sd = 4)
  coating_2 <- rnorm(n_per_group, mean = delta, sd = 4)
  t.test(coating_1, coating_2)$p.value < 0.05
})

mean(detected)   # should be close to the target power of 0.80

# ---- power-curve -------------------------------------------------

curve_data <- expand.grid(n = 2:40, d = c(0.5, 0.8, 1.0, 1.25)) |>
  as_tibble() |>
  mutate(power = mapply(function(n, d) {
    pwr.t.test(n = n, d = d, sig.level = 0.05, type = "two.sample")$power
  }, n, d))

ggplot(curve_data, aes(x = n, y = power, color = factor(d))) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.8, linetype = "dotted") +
  labs(x = "Experimental units per group", y = "Power", color = "Cohen's d")

# ---- messy -------------------------------------------------------

# A spreadsheet as it is often typed by hand (simulated).
messy <- tibble(
  sample = 1:5,
  `Coating A` = round(rnorm(5, mean = 20, sd = 3), 1),
  `Coating B` = round(rnorm(5, mean = 17, sd = 3), 1),
  `Coating C` = round(rnorm(5, mean = 22, sd = 3), 1)
)
messy$`Coating B`[4] <- NA   # one sample was lost during the experiment

messy

# ---- tidy --------------------------------------------------------

tidy <- messy |>
  pivot_longer(cols = -sample,                 # reshape every column except "sample"
               names_to = "coating",
               values_to = "corrosion_um") |>
  mutate(
    coating = factor(sub("Coating ", "", coating)),  # keep only "A", "B", "C" and store as a factor
    unit_id = row_number()                           # a unique label for each experimental unit
  ) |>
  select(unit_id, coating, corrosion_um)

tidy

# ---- save-data ---------------------------------------------------

dir.create("data", showWarnings = FALSE)                  # create a folder for the datasets
write.csv(tidy, "data/ch01_corrosion_tidy.csv", row.names = FALSE)

# ---- types -------------------------------------------------------

# Three doses, six units each. The true mean response is NOT a straight line
# (it rises sharply from dose 1 to dose 2, then levels off).
dose_data <- tibble(
  dose_code = rep(1:3, each = 6),
  response  = rnorm(18, mean = rep(c(10, 18, 19), each = 6), sd = 2)
)

fit_as_number <- lm(response ~ dose_code, data = dose_data)          # dose treated as a number
fit_as_factor <- lm(response ~ factor(dose_code), data = dose_data)  # dose treated as a category

# Number of estimated coefficients and share of variation explained.
tibble(
  model = c("Dose as a number", "Dose as a factor"),
  coefficients = c(length(coef(fit_as_number)), length(coef(fit_as_factor))),
  r_squared = c(summary(fit_as_number)$r.squared, summary(fit_as_factor)$r.squared)
) |>
  knitr::kable(digits = 3)

# ---- glimpse -----------------------------------------------------

glimpse(tidy)   # shows each column name and its type: <fct> for factors, <dbl> for numbers

# ---- save-data -----------------------------------------------
# Saves the simulated datasets of this chapter to data/.
dir.create("data", showWarnings = FALSE)
if (exists("pots")) write.csv(pots, "data/ch01_pots.csv", row.names = FALSE)
if (exists("dose_data")) write.csv(dose_data, "data/ch01_dose_data.csv", row.names = FALSE)
