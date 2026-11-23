# Chapter 7. Factorial experiments
# Planning and Analysis of Experiments with R, Volume 1.
# Run this script from the repository root, for example with
#   source("scripts/ch07_factorial.R")
# The setup file loads the packages and the plot theme used in the book.

# ---- setup -------------------------------------------------------
source("R/setup.R")
library(car)
library(emmeans)
set.seed(707)

# ---- simulate ----------------------------------------------------

# True mean yields. The catalyst effect is bigger at the highest temperature (an interaction).
means <- expand_grid(
  temp     = factor(c("Low", "Medium", "High"), levels = c("Low", "Medium", "High")),
  catalyst = factor(c("X", "Y"))
) |>
  mutate(mu = c(60, 62,        # Low:    X, Y
                68, 72,        # Medium: X, Y
                74, 84))       # High:   X, Y

reaction <- means |>
  slice(rep(1:n(), each = 4)) |>                    # four replicate runs per combination
  mutate(yield = rnorm(n(), mean = mu, sd = 2.5)) |>
  dplyr::select(-mu)

head(reaction)

# ---- interaction-plot --------------------------------------------

reaction |>
  group_by(temp, catalyst) |>
  summarise(mean_yield = mean(yield), .groups = "drop") |>
  ggplot(aes(x = temp, y = mean_yield, colour = catalyst, group = catalyst)) +
  geom_line() + geom_point(size = 2) +
  labs(x = "Temperature", y = "Mean yield (%)", colour = "Catalyst")

# ---- contrasts ---------------------------------------------------

options(contrasts = c("contr.sum", "contr.poly"))    # unordered factors: sum; ordered factors: polynomial
getOption("contrasts")

# ---- fit ---------------------------------------------------------

fit <- lm(yield ~ temp * catalyst, data = reaction)    # temp * catalyst = temp + catalyst + temp:catalyst
car::Anova(fit, type = 3)

# ---- simple-effects ----------------------------------------------

emm <- emmeans(fit, ~ catalyst | temp)      # means of catalyst, separately at each temperature
emm

pairs(emm, adjust = "none")                 # Y - X at each temperature (three planned questions)

# ---- simple-effects-ci -------------------------------------------

confint(pairs(emm), adjust = "holm")

# ---- emm-plot ----------------------------------------------------

emmip(fit, catalyst ~ temp, CIs = TRUE) +
  labs(x = "Temperature", y = "Estimated mean yield (%)", colour = "Catalyst")

# ---- unbalanced --------------------------------------------------

reaction_ub <- reaction
reaction_ub$yield[c(2, 3, 9, 15, 16, 22)] <- NA        # six runs lost

table(reaction_ub$temp[!is.na(reaction_ub$yield)],
      reaction_ub$catalyst[!is.na(reaction_ub$yield)])  # counts are now unequal

# ---- type1-order -------------------------------------------------

fit_a <- lm(yield ~ temp * catalyst, data = reaction_ub)
fit_b <- lm(yield ~ catalyst * temp, data = reaction_ub)

anova(fit_a)     # temp first
anova(fit_b)     # catalyst first: the sums of squares for the main effects differ

# ---- type2-type3 -------------------------------------------------

fit_ub <- lm(yield ~ temp * catalyst, data = reaction_ub)    # fitted with contr.sum, set above

car::Anova(fit_ub, type = 2)
car::Anova(fit_ub, type = 3)

# ---- unbalanced-emm ----------------------------------------------

emmeans(fit_ub, ~ catalyst | temp)

# ---- quantitative-simulate ---------------------------------------

rates <- c(0, 40, 80, 120, 160)                        # kg N / ha

crop <- tibble(rate = rep(rates, each = 4)) |>
  mutate(
    # A curved response that levels off (quadratic), plus noise
    yield = 3.0 + 0.045 * rate - 0.00017 * rate^2 + rnorm(n(), sd = 0.25)
  )

# ---- polynomial-contrasts ----------------------------------------

crop$rate_f <- factor(crop$rate, ordered = TRUE)       # ordered factor uses contr.poly by default

fit_f <- lm(yield ~ rate_f, data = crop)
summary(fit_f)$coefficients                             # .L, .Q, .C, ^4 = linear, quadratic, cubic, quartic

# ---- regression --------------------------------------------------

fit_quad <- lm(yield ~ poly(rate, 2, raw = TRUE), data = crop)    # raw = TRUE gives coefficients in the original units
fit_lin  <- lm(yield ~ rate, data = crop)
fit_cat  <- lm(yield ~ rate_f, data = crop)                        # saturated: one mean per dose

anova(fit_lin, fit_quad, fit_cat)      # linear vs quadratic vs saturated model

# ---- dose-response-plot ------------------------------------------

grid <- tibble(rate = seq(0, 160, length.out = 100))
pred <- predict(fit_quad, newdata = grid, interval = "confidence")
grid <- bind_cols(grid, as_tibble(pred))

ggplot() +
  geom_ribbon(data = grid, aes(rate, ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(data = grid, aes(rate, fit)) +
  geom_point(data = crop, aes(rate, yield)) +
  labs(x = "Nitrogen rate (kg/ha)", y = "Yield (t/ha)")

# ---- optimum -----------------------------------------------------

b <- coef(fit_quad)                      # intercept, linear, quadratic
optimum <- -b[2] / (2 * b[3])            # vertex of the parabola: -b1 / (2 * b2)
round(unname(optimum), 1)

# ---- mixed-factorial ---------------------------------------------

varieties <- c("V1", "V2")
mixed <- expand_grid(rate = rates, variety = varieties, rep = 1:4) |>
  mutate(
    variety = factor(variety),
    # V2 responds more strongly to nitrogen than V1
    yield = 3 + ifelse(variety == "V1", 0.030, 0.050) * rate -
            ifelse(variety == "V1", 0.00010, 0.00020) * rate^2 +
            rnorm(n(), sd = 0.25)
  )

fit_mixed <- lm(yield ~ variety * poly(rate, 2), data = mixed)
car::Anova(fit_mixed, type = 3)

# ---- save-data -----------------------------------------------
# Saves the simulated datasets of this chapter to data/.
dir.create("data", showWarnings = FALSE)
if (exists("reaction")) write.csv(reaction, "data/ch07_reaction.csv", row.names = FALSE)
if (exists("crop")) write.csv(crop, "data/ch07_crop.csv", row.names = FALSE)
if (exists("mixed")) write.csv(mixed, "data/ch07_mixed.csv", row.names = FALSE)
