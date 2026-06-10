
source("~/GitHub/csv_scd2/testing/preamble.R")
source("~/GitHub/csv_scd2/testing/hash.R")

historic_file <- ""
load_file <- "load1.csv"
key_cols <- "name"
run_date <- ymd("2026/01/01")

source("~/GitHub/csv_scd2/testing/body.R")

write_csv(out, "historic-init.csv")

historic_file <- "historic-init.csv"
load_file <- "load2.csv"
key_cols <- "name"
run_date <- ymd("2026/01/15")

source("~/GitHub/csv_scd2/testing/body.R")