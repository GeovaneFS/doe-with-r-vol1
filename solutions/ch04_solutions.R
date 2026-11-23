# Solutions for the exercises of Chapter 4
# Run from the repository root: source("solutions/ch04_solutions.R")
invisible(capture.output(source("scripts/ch04_emmeans.R")))

# ---- Exercise 1: Tukey versus Dunnett intervals involving the control ----
tk <- as.data.frame(confint(pairs(emm, adjust = "tukey")))
dn <- as.data.frame(confint(contrast(emm, method = "trt.vs.ctrl", ref = "Control")))
tk_ctrl <- tk[grepl("Control", tk$contrast), ]
tibble(comparison = dn$contrast,
       tukey_width  = tk_ctrl$upper.CL - tk_ctrl$lower.CL,
       dunnett_width = dn$upper.CL - dn$lower.CL)

# ---- Exercise 2: (A + B) / 2 versus (C + D) / 2 ----
levels(resin$formulation)                    # Control, A, B, C, D
w <- c(Control = 0, A = 1/2, B = 1/2, C = -1/2, D = -1/2)
sum(w)                                       # must be zero
contrast(emm, method = list("A,B vs C,D" = w))
confint(contrast(emm, method = list("A,B vs C,D" = w)))

# ---- Exercise 3: ratios to the control on the log scale, Dunnett ----
emm_log2 <- emmeans(lm(log(strength) ~ formulation, data = resin), ~ formulation)
ratios <- contrast(emm_log2, method = "trt.vs.ctrl", ref = "Control")
summary(ratios, infer = TRUE, type = "response")   # ratio = formulation / Control

# ---- Exercise 4: 20 formulations with the same true mean, 200 experiments ----
set.seed(404)
one_experiment <- function(k = 20, n = 4) {
  d <- tibble(formulation = factor(rep(paste0("F", 1:k), each = n)),
              strength = rnorm(k * n, mean = 50, sd = 3))
  e <- emmeans(lm(strength ~ formulation, data = d), ~ formulation)
  c(none  = sum(as.data.frame(pairs(e, adjust = "none"))$p.value  < 0.05),
    tukey = sum(as.data.frame(pairs(e, adjust = "tukey"))$p.value < 0.05),
    fdr   = sum(as.data.frame(pairs(e, adjust = "fdr"))$p.value   < 0.05))
}
res <- t(replicate(200, one_experiment()))
rbind(mean_false_discoveries = colMeans(res),
      share_with_any_false   = colMeans(res > 0))
