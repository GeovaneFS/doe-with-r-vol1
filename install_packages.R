# Installs the packages used in the book (Volume 1).
# Run once:  source("install_packages.R")
pkgs <- c(
  "ggplot2", "dplyr", "tidyr", "knitr",   # data handling and graphics
  "pwr",                                  # power and sample size
  "car", "emmeans",                       # ANOVA tables and comparisons
  "nlme", "lme4", "lmerTest",             # variance structures and random effects
  "MASS", "patchwork"                     # Box-Cox profile and combined figures
)
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing)
cat("All packages are installed.\n")
