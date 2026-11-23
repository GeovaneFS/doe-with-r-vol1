# Runs every chapter script in order and writes the datasets to data/.
# Usage: from the repository root, run  source("run_all.R")
grDevices::pdf(NULL)   # no plot windows; figures are not saved by this runner
.scripts <- sort(list.files("scripts", pattern = "^ch[0-9]{2}_.*\\.R$", full.names = TRUE))
for (.s in .scripts) {
  cat("Running", .s, "\n")
  source(.s)
  rm(list = setdiff(ls(), c(".scripts", ".s")))   # every chapter starts clean
}
cat("Done. Datasets are in data/.\n")
