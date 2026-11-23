# Solutions for the exercises of Chapter 7
# Run from the repository root: source("solutions/ch07_solutions.R")
invisible(capture.output(source("scripts/ch07_factorial.R")))
options(contrasts = c("contr.sum", "contr.poly"))

# ---- Exercise 1: no interaction, catalyst Y always 5 units better ----
set.seed(701)
no_int <- expand_grid(temp = factor(c("Low", "Medium", "High"), levels = c("Low", "Medium", "High")),
                      catalyst = factor(c("X", "Y"))) |>
  mutate(mu = c(60, 65, 68, 73, 74, 79)) |>        # Y = X + 5 at every temperature
  slice(rep(1:n(), each = 4)) |>
  mutate(yield = rnorm(n(), mean = mu, sd = 2.5))
fit1 <- lm(yield ~ temp * catalyst, data = no_int)
car::Anova(fit1, type = 3)
emmip(fit1, catalyst ~ temp)

# ---- Exercise 2: Type III under contr.treatment versus contr.sum ----
fit_sum <- lm(yield ~ temp * catalyst, data = reaction_ub)       # contr.sum is active
options(contrasts = c("contr.treatment", "contr.poly"))
fit_trt <- lm(yield ~ temp * catalyst, data = reaction_ub)
options(contrasts = c("contr.sum", "contr.poly"))                # restore
t3_sum <- car::Anova(fit_sum, type = 3)
t3_trt <- car::Anova(fit_trt, type = 3)
cbind(term = rownames(t3_sum), contr_sum = round(t3_sum$`Pr(>F)`, 4),
      contr_treatment = round(t3_trt$`Pr(>F)`, 4))

# ---- Exercise 3: cubic polynomial versus quadratic ----
fit_q <- lm(yield ~ poly(rate, 2), data = crop)
fit_c <- lm(yield ~ poly(rate, 3), data = crop)
anova(fit_q, fit_c)

# ---- Exercise 4: optimum nitrogen rate per variety with a bootstrap interval ----
optimum <- function(d) {
  b <- coef(lm(yield ~ rate + I(rate^2), data = d))
  -b[2] / (2 * b[3])
}
set.seed(704)
boot_opt <- function(d, B = 1000) {
  est <- replicate(B, optimum(d[sample(nrow(d), replace = TRUE), ]))
  c(estimate = optimum(d), quantile(est, c(0.025, 0.975), na.rm = TRUE))
}
do.call(rbind, lapply(split(mixed, mixed$variety), boot_opt))
