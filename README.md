[README.md](https://github.com/user-attachments/files/35252021/README.md)
# Planning and Analysis of Experiments with R. Volume 1

Companion code and data for the book *Planning and Analysis of Experiments with R,
Volume 1. Linear and Mixed Models for Designed Experiments*, by Geovane Fernandes da Silva.

This repository contains the R code of every chapter, the simulated datasets, and
code for the exercises. It has no explanation of the results. How to read and interpret each
output, and what to conclude from it, is taught in the book.

## What is here

| Folder or file | Content |
|---|---|
| `scripts/` | One script per chapter, with the code exactly as it appears in the book |
| `solutions/` | Code that solves the four exercises of each chapter |
| `data/` | Simulated datasets written by the chapter scripts (CSV) |
| `R/setup.R` | Shared setup: packages, plot theme, and the book palette |
| `install_packages.R` | Installs the packages used in the book |
| `run_all.R` | Runs all chapter scripts and rewrites the datasets in `data/` |
| `run_solutions.R` | Runs all solution scripts |

## How to use it

1. Install R (version 4.3 or newer; the book was produced with R 4.6.0) and, optionally, RStudio.
2. Download this repository (green **Code** button, then **Download ZIP**) or clone it.
3. Open R with the repository folder as the working directory. In RStudio, open the project folder or use `setwd()`.
4. Install the packages once:

```r
source("install_packages.R")
```

5. Run a chapter script, or all of them:

```r
source("scripts/ch02_crd.R")   # one chapter
source("run_all.R")            # every chapter, and rewrites data/
```

6. Try an exercise first, then compare with the solution:

```r
source("solutions/ch02_solutions.R", print.eval = TRUE)
```

Each solution script runs the chapter script first, so all objects of the chapter exist.
Random numbers are controlled with `set.seed()`. With the same version of R and of the
packages your numbers will match the book. Small differences in the last decimals can
appear with other versions and are not a reason for concern.

## Chapters

| Chapter | Topic | Script |
|---|---|---|
| 1 | Foundations of experimental design and data structuring | `scripts/ch01_foundations.R` |
| 2 | The completely randomized design | `scripts/ch02_crd.R` |
| 3 | Model diagnostics and remedies | `scripts/ch03_diagnostics.R` |
| 4 | Comparing treatments with `emmeans` | `scripts/ch04_emmeans.R` |
| 5 | The randomized complete block design | `scripts/ch05_rcbd.R` |
| 6 | The Latin square design | `scripts/ch06_latin_square.R` |
| 7 | Factorial experiments | `scripts/ch07_factorial.R` |
| 8 | Split-plot designs as mixed models | `scripts/ch08_split_plot.R` |

## Software

The analyses use `lm()`, `lme4::lmer()` with `lmerTest`, `car::Anova()`, `emmeans`, and `ggplot2`.
The book was produced with R 4.6.0, ggplot2 4.0.4, dplyr 1.2.1, tidyr 1.3.2, patchwork 1.3.2,
pwr 1.3-0, lme4 2.1.0, lmerTest 3.2.1, nlme 3.1-172, car 3.1.5, emmeans 2.0.4, and MASS 7.3.66.

## Licenses

Code is released under the MIT License (`LICENSE`). The simulated data are released under
CC BY 4.0 (`LICENSE-DATA.md`). The text and the figures of the book are not covered by these licenses.

## Use of artificial intelligence

The author designed, structured, and directed the book and this repository, established their
scope and methods, and takes full responsibility for their content. Generative AI served only as
a drafting and copyediting aid under the author's supervision. All code in this repository was
run, and the solutions were checked by running them.

## How to cite

If you use this material, please cite the book. See `CITATION.cff`.
