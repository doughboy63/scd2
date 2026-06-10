
source("~/GitHub/csv_scd2/testing/preamble.R")
source("~/GitHub/csv_scd2/testing/hash.R")

key_cols <- "name"
historic_file <- ""
run_date <- ymd("2026/01/01")

for (i in 1:6) {
  run_date <- run_date + days(10)
  load_file <- paste0("load", i, ".csv")
  source("~/GitHub/csv_scd2/testing/body.R")
  historic_file <- paste0("historic", i, ".csv")
  write_excel_csv(out, historic_file)
}
