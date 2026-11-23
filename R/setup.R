# Shared setup sourced by every chapter.
# Loads the packages used for data handling and graphics and fixes the look of
# plots and printed output so that every chapter is consistent.
suppressPackageStartupMessages({
  library(ggplot2)   # graphics
  library(dplyr)     # data manipulation
  library(tidyr)     # reshaping tables
})

# Book palette (colour-blind friendly, readable in grayscale print)
book_colors <- c("#1F4E79", "#C0504D", "#4E8F3A", "#E08A1E", "#6B4C9A", "#2A9D8F")

options(
  width = 72,                       # keeps printed output inside the 7 x 10 inch page
  digits = 5,                       # fewer digits in printed numbers
  ggplot2.discrete.colour = book_colors,
  ggplot2.discrete.fill   = book_colors,
  tibble.width = 72
)

# Clean, light theme for all figures
theme_set(
  theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = "grey88", linewidth = 0.3),
      axis.line          = element_line(colour = "grey40", linewidth = 0.3),
      axis.ticks         = element_line(colour = "grey40", linewidth = 0.3),
      axis.title         = element_text(colour = "grey20"),
      legend.position    = "bottom",
      legend.title       = element_text(size = 9),
      plot.title         = element_text(face = "bold", size = 11),
      strip.text         = element_text(face = "bold")
    )
)

# Printed output has no prefix: the gray bar in the book separates it from code
knitr::opts_chunk$set(comment = "")
