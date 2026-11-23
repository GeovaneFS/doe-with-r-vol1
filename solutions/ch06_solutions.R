# Solutions for the exercises of Chapter 6
# Run from the repository root: source("solutions/ch06_solutions.R")
invisible(capture.output(source("scripts/ch06_latin_square.R")))

# ---- Exercise 1: build and randomize a 4 x 4 Latin square ----
set.seed(601)
k <- 4
trts <- LETTERS[1:k]
sq <- outer(1:k, 1:k, function(i, j) trts[((i + j - 2) %% k) + 1])   # cyclic square
sq <- sq[sample(k), sample(k)]                                        # shuffle rows and columns
sq <- matrix(sample(trts)[match(sq, trts)], nrow = k)                 # relabel treatments
sq
# Each treatment appears exactly once in every row and every column:
all(apply(sq, 1, function(x) length(unique(x)) == k))
all(apply(sq, 2, function(x) length(unique(x)) == k))

# ---- Exercise 2: no column effect ----
simulate_lsq <- function(col_eff, row_effect = row_eff, extra = function(d) 0, seed = 602) {
  set.seed(seed)
  expand_grid(row = 1:t_n, col = 1:t_n) |>
    mutate(treatment = factor(square[cbind(row, col)]),
           row = factor(row), col = factor(col)) |>
    mutate(yield = trt_means[as.character(treatment)] + row_effect[as.integer(row)] +
                   col_eff[as.integer(col)] + extra(pick(everything())) + rnorm(n(), sd = 1))
}
d2 <- simulate_lsq(col_eff = rep(0, t_n))
c(latin_square = sigma(lm(yield ~ row + col + treatment, data = d2))^2,
  rows_only    = sigma(lm(yield ~ row + treatment, data = d2))^2)

# ---- Exercise 3: a strong row-by-treatment interaction ----
d3 <- simulate_lsq(col_eff = col_eff,
                   extra = function(d) 2.5 * (as.integer(d$row) - 3) * (d$treatment == "C"))
fit3 <- lm(yield ~ row + col + treatment, data = d3)
d_base <- simulate_lsq(col_eff = col_eff)                 # same seed, no interaction
c(error_ms_with_interaction = sigma(fit3)^2,
  error_ms_without = sigma(lm(yield ~ row + col + treatment, data = d_base))^2)
ggplot(tibble(fitted = fitted(fit3), resid = resid(fit3), treatment = d3$treatment),
       aes(fitted, resid, colour = treatment)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_point(size = 2) +
  labs(x = "Fitted values (t/ha)", y = "Residuals (t/ha)")

# ---- Exercise 4: three random missing plots ----
set.seed(604)
lsq4 <- lsq
lsq4$yield[sample(nrow(lsq4), 3)] <- NA
fit4 <- lm(yield ~ row + col + treatment, data = lsq4)
lost <- table(lsq4$treatment[is.na(lsq4$yield)])
lost                                          # plots lost per treatment
# The residual standard deviation changes by chance, so compare the standard errors
# in units of sigma, which isolates the effect of the lost plots.
tibble(treatment = levels(lsq$treatment),
       se_complete = summary(emmeans(fit, ~ treatment))$SE / sigma(fit),
       se_missing  = summary(emmeans(fit4, ~ treatment))$SE / sigma(fit4))
