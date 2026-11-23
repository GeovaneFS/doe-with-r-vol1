# Solutions for the exercises of Chapter 5
# Run from the repository root: source("solutions/ch05_solutions.R")
invisible(capture.output(source("scripts/ch05_rcbd.R")))

# A function that repeats the simulation of the chapter with other settings.
simulate_rcbd <- function(sd_block = 1.5, sd_error = 0.5, slope_sd = 0, seed = 505) {
  set.seed(seed)
  d <- expand_grid(day = factor(blocks), temp = factor(names(temps), levels = names(temps)))
  day_eff <- rnorm(length(blocks), 0, sd_block); names(day_eff) <- blocks
  # Interaction: each day reacts to temperature with its own slope (0 = additive model).
  slope <- rnorm(length(blocks), 0, slope_sd); names(slope) <- blocks
  d$moisture <- unname(temps[as.character(d$temp)]) +
                unname(day_eff[as.character(d$day)]) +
                unname(slope[as.character(d$day)]) * (as.integer(d$temp) - 3) +
                rnorm(nrow(d), 0, sd_error)
  d
}

# ---- Exercise 1: block standard deviation of 0.2 instead of 1.5 ----
mse_compare <- function(sd_block) {
  d <- simulate_rcbd(sd_block = sd_block)
  c(sd_block = sd_block,
    mse_rcbd = sigma(lm(moisture ~ day + temp, data = d))^2,
    mse_crd  = sigma(lm(moisture ~ temp, data = d))^2)
}
rbind(mse_compare(1.5), mse_compare(0.2))

# ---- Exercise 2: two missing values in different blocks and treatments ----
d2 <- rcbd
idx <- c(which(d2$day == "Day2" & d2$temp == "T70"), which(d2$day == "Day5" & d2$temp == "T90"))
d2$moisture[idx] <- NA
fit2 <- lm(moisture ~ day + temp, data = d2)
emm_ls <- as.data.frame(emmeans(fit2, ~ temp))[, c("temp", "emmean")]

# Yates iteration: start with the overall mean and update each missing cell with the
# single-missing-plot formula, using the current values of the other cells.
filled <- d2
miss_idx <- idx
filled$moisture[miss_idx] <- mean(d2$moisture, na.rm = TRUE)
r <- nlevels(d2$day); t <- nlevels(d2$temp)
for (it in 1:100) {
  old <- filled$moisture[miss_idx]
  for (i in miss_idx) {
    B <- sum(filled$moisture[filled$day  == filled$day[i]])  - filled$moisture[i]
    T <- sum(filled$moisture[filled$temp == filled$temp[i]]) - filled$moisture[i]
    G <- sum(filled$moisture) - filled$moisture[i]
    filled$moisture[i] <- (r * B + t * T - G) / ((r - 1) * (t - 1))
  }
  if (max(abs(filled$moisture[miss_idx] - old)) < 1e-10) break
}
yates_means <- tapply(filled$moisture, filled$temp, mean)
tibble(temp = emm_ls$temp, least_squares = emm_ls$emmean, yates = as.numeric(yates_means),
       difference = least_squares - yates)
rbind(true = rcbd$moisture[idx], yates = filled$moisture[idx],
      least_squares = predict(fit2, newdata = rcbd[idx, ]))

# ---- Exercise 3: a day-by-temperature interaction ----
d3 <- simulate_rcbd(slope_sd = 1)
ggplot(d3, aes(temp, moisture, group = day, colour = day)) + geom_line() + geom_point() +
  labs(x = "Drying temperature", y = "Moisture (%)", colour = "Block")
fit3 <- lm(moisture ~ day + temp, data = d3)
d3$sq <- fitted(fit3)^2
anova(lm(moisture ~ day + temp + sq, data = d3))["sq", ]

# ---- Exercise 4: random day effect and five missing values ----
set.seed(54)
d4 <- rcbd
d4$moisture[sample(nrow(d4), 5)] <- NA
fit_fixed  <- lm(moisture ~ day + temp, data = d4)
fit_random <- lmer(moisture ~ temp + (1 | day), data = d4)
tibble(temp = levels(d4$temp),
       se_fixed_blocks  = summary(emmeans(fit_fixed, ~ temp))$SE,
       se_random_blocks = summary(emmeans(fit_random, ~ temp))$SE)
