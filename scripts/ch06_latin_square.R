# Chapter 6. The Latin square design
# Planning and Analysis of Experiments with R, Volume 1.
# Run this script from the repository root, for example with
#   source("scripts/ch06_latin_square.R")
# The setup file loads the packages and the plot theme used in the book.

# ---- setup -------------------------------------------------------
source("R/setup.R")
library(car)
library(emmeans)
library(lme4)
library(lmerTest)
set.seed(606)

# ---- make-square -------------------------------------------------

t_n <- 5
treatments <- LETTERS[1:t_n]

# Cyclic Latin square: each row shifts the treatment order by one position
square <- outer(1:t_n, 1:t_n, function(i, j) treatments[((i + j - 2) %% t_n) + 1])

# Randomize: permute rows, columns, and treatment labels
square <- square[sample(t_n), sample(t_n)]
square <- matrix(sample(treatments)[match(square, treatments)], nrow = t_n)
square

# ---- check-square ------------------------------------------------

all(apply(square, 1, function(r) length(unique(r)) == t_n))   # rows
all(apply(square, 2, function(c) length(unique(c)) == t_n))   # columns

# ---- simulate ----------------------------------------------------

trt_means <- c(A = 30, B = 33, C = 36, D = 37, E = 31)       # true yields (t/ha)
row_eff   <- seq(-3, 3, length.out = t_n)                     # fertility gradient
col_eff   <- c(2, 1, 0, -1, -2)                                # moisture gradient

lsq <- expand_grid(row = 1:t_n, col = 1:t_n) |>
  mutate(
    treatment = factor(square[cbind(row, col)]),
    row = factor(row),
    col = factor(col),
    yield = trt_means[as.character(treatment)] +
            row_eff[as.integer(row)] +
            col_eff[as.integer(col)] +
            rnorm(n(), sd = 1)
  )

head(lsq)

# ---- layout-plot -------------------------------------------------

ggplot(lsq, aes(x = col, y = row, fill = yield)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = treatment)) +
  scale_y_discrete(limits = rev) +
  labs(x = "Column", y = "Row", fill = "Yield (t/ha)")

# ---- fit ---------------------------------------------------------

fit <- lm(yield ~ row + col + treatment, data = lsq)
car::Anova(fit, type = 2)

# ---- emmeans -----------------------------------------------------

emm <- emmeans(fit, ~ treatment)
emm
pairs(emm, adjust = "tukey")

# ---- compare-models ----------------------------------------------

fit_row  <- lm(yield ~ row + treatment, data = lsq)      # row blocks only
fit_none <- lm(yield ~ treatment, data = lsq)            # no blocking, CRD analysis

round(c(latin_square = sigma(fit)^2,
        rows_only    = sigma(fit_row)^2,
        no_blocks    = sigma(fit_none)^2), 2)

# ---- lmer --------------------------------------------------------

fit_mixed <- lmer(yield ~ treatment + (1 | row) + (1 | col), data = lsq)
anova(fit_mixed)
emmeans(fit_mixed, ~ treatment)

# ---- missing -----------------------------------------------------

lsq_na <- lsq
lsq_na$yield[7] <- NA                           # one plot lost

fit_na <- lm(yield ~ row + col + treatment, data = lsq_na)
car::Anova(fit_na, type = 2)
emmeans(fit_na, ~ treatment)

# ---- save-data -----------------------------------------------
# Saves the simulated datasets of this chapter to data/.
dir.create("data", showWarnings = FALSE)
if (exists("lsq")) write.csv(lsq, "data/ch06_lsq.csv", row.names = FALSE)
