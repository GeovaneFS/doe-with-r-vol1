# Runs the solution script of every chapter and prints the results.
# Usage: from the repository root, run  source("run_solutions.R")
# Each script can also be run alone, for example source("solutions/ch02_solutions.R").
grDevices::pdf(NULL)   # no plot windows
.files <- sort(list.files("solutions", pattern = "^ch[0-9]{2}_solutions\\.R$", full.names = TRUE))
for (.f in .files) {
  cat("\n==========", .f, "==========\n")
  source(.f, print.eval = TRUE)
  rm(list = setdiff(ls(), c(".files", ".f")))
}
