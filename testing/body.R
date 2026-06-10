





if (!is.Date(run_date)) {
  log_fatal("The run_date parameter is not a date!")
}

# Checking for historic file.  If I have it, then check that it is well formed.
have_historic <- file_exists(historic_file)
if (have_historic) {
  historic_table <- read_csv(historic_file, show_col_types = FALSE) %>% 
    clean_names()
  
  historic_cols <- colnames(historic_table)
  required_historic_cols <- c(key_cols, "effective_date", "expiry_date", "current_record")
  
  if (!all(required_historic_cols %in% historic_cols)) {
    log_fatal(paste0("Historic file (", historic_file, ") is malformed. Table must have all key columns, effective_date, expiry_date, current_record!"))
  } 
  
  value_cols = setdiff(historic_cols, required_historic_cols)
} else {
  log_warn("The historic file targeted (", historic_file, ") cannot be found!")
}

# Checking for load file.  If I have it, then check that it is well formed.
have_load <- file_exists(load_file)
if (have_load) {
  load_table <- read_csv(load_file, , show_col_types = FALSE) %>% 
    clean_names() 
  
  load_cols <- colnames(load_table)
  
  if (!all(key_cols %in% load_cols)) {
    log_fatal(paste0("Load file (", load_file, ") is malformed. Table must have all key columns!"))
    
  } 
} else {
  log_fatal("The load file targeted (", load_file, ") cannot be found!")
}

# default infinity date
INF_DATE <- ymd("9999/12/31")

# If I don't have a historic file, turn the load file into a historic file. 
if (!have_historic & have_load) {
  log_info("Converting load table (", load_file, ") into the historic table.")
  
  out <- load_table %>%   
    mutate(effective_date = run_date) %>% 
    mutate(expiry_date = INF_DATE) %>% 
    mutate(current_record = TRUE)
  
} else {
  
  required_load_cols <- c(key_cols, value_cols)
  
  if (!all(required_load_cols %in% load_cols)) {
    log_fatal(paste0("Load file (", load_file, ") is malformed. Table must have all key columns!"))
  }
  
  hashed_load_table <- load_table %>% 
    mutate(load_date = run_date) %>% 
    make_hash(key_cols)
  
  hashed_historic_table <- historic_table %>% 
    make_hash(key_cols)
  
  key_value_lookup <- bind_rows(hashed_historic_table, hashed_load_table) %>% 
    select(-c(effective_date, expiry_date, current_record, load_date))
  
  historic_hashes <- hashed_historic_table %>% 
    select(key_hash, value_hash, effective_date, expiry_date)  
  
  load_hashes <- hashed_load_table %>% 
    select(key_hash, value_hash, load_date) 
  
  new_hashes <- anti_join(load_hashes, historic_hashes, by = join_by(key_hash, value_hash))
  
  capped_hashes <- new_hashes %>% 
    select(key_hash, load_date) %>% 
    left_join(historic_hashes, ., by = join_by(key_hash)) %>% 
    mutate(expiry_date = if_else(is.na(load_date), expiry_date, load_date - days())) %>% 
    select(-load_date)
  
  out <- new_hashes %>% 
    rename(effective_date = load_date) %>% 
    mutate(expiry_date = INF_DATE) %>% 
    bind_rows(capped_hashes, .) %>%
    left_join(., key_value_lookup, by = join_by(key_hash, value_hash)) %>% 
    arrange(key_hash, effective_date) %>% 
    select(-c(key_hash, value_hash)) %>% 
    mutate(current_record = expiry_date == INF_DATE) %>%  
    relocate(c("effective_date", "expiry_date", "current_record"), .after = last_col())

}
