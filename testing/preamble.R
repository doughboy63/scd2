# adjust as needed
setwd("~/GitHub/csv_scd2/testing/")

# Setup ------------------------------------------------------------------------

# pacman is used to manage required libraries
if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
  library("pacman")
}

# required libraries - can verify by running p_loaded()
p_load(tidyverse, fs, janitor, digest, logger)

# sets log text colours
log_layout(layout_glue_colors)