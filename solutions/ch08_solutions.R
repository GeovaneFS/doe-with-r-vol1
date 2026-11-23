# Solutions for the exercises of Chapter 8
# Run from the repository root: source("solutions/ch08_solutions.R")
invisible(capture.output(source("scripts/ch08_split_plot.R")))

# A function that repeats the simulation of the chapter with other settings.
simulate_sp <- function(n_blocks = 4, sd_wp = 1.2, seed = 808) {
  set.seed(seed)
  b <- factor(paste0("B", 1:n_blocks))
  d <- expand_grid(block = b, irrigation = irrigation, variety = variety) |>
    mutate(whole_plot = factor(paste(block, irrigation, sep = "_")))
  be <- rnorm(nlevels(b), sd = 0.8); names(be) <- levels(b)
  we <- rnorm(nlevels(d$whole_plot), sd = sd_wp); names(we) <- levels(d$whole_plot)
  ie <- ifelse(d$irrigation == "High" & d$variety == "V3", 1.5, 0)
  d$yield <- 20 + irr_eff[as.character(d$irrigation)] + var_eff[as.character(d$variety)] + ie +
             be[as.character(d$block)] + we[as.character(d$whole_plot)] + rnorm(nrow(d), sd = 0.7)
  d
}
form <- yield ~ irrigation * variety + (1 | block) + (1 | block:irrigation)

# ---- Exercise 1: drop the whole-plot term ----
fit_nowp <- lmer(yield ~ irrigation * variety + (1 | block), data = sp)
rbind(correct = anova(fit)["irrigation", c("F value", "DenDF", "Pr(>F)")],
      without_wp_term = anova(fit_nowp)["irrigation", c("F value", "DenDF", "Pr(>F)")])

# ---- Exercise 2: whole-plot standard deviation of 3 ----
width <- function(d) {
  m <- lmer(form, data = d)
  ci <- as.data.frame(confint(emmeans(m, ~ irrigation)))
  mean(ci$upper.CL - ci$lower.CL)
}
c(sd_wp_1.2 = width(simulate_sp(sd_wp = 1.2)), sd_wp_3 = width(simulate_sp(sd_wp = 3)))

# ---- Exercise 3: delete an entire whole plot ----
sp3 <- sp |> filter(!(block == "B2" & irrigation == "Medium"))
fit3 <- lmer(form, data = sp3)
before <- as.data.frame(pairs(emmeans(fit, ~ irrigation)))[, c("contrast", "SE", "df")]
after  <- as.data.frame(pairs(emmeans(fit3, ~ irrigation)))[, c("contrast", "SE", "df")]
cbind(before, SE_after = after$SE, df_after = after$df)

# ---- Exercise 4: eight blocks instead of four, power for irrigation ----
power_irrigation <- function(n_blocks, B = 200) {
  p <- vapply(1:B, function(i) {
    m <- suppressWarnings(suppressMessages(lmer(form, data = simulate_sp(n_blocks = n_blocks, seed = 1000 + i))))
    anova(m)["irrigation", "Pr(>F)"]
  }, numeric(1))
  mean(p < 0.05)
}
c(four_blocks = power_irrigation(4), eight_blocks = power_irrigation(8))
