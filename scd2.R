



# adjust as needed
setwd("~/GitHub/scd2/testing")

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


# Functions --------------------------------------------------------------------

make_hash <- function(.data, key_cols) {
  # helper function:  allows for easy comparison of key and value columns in the csv_scd2 function
  
  ignore_cols <- c("effective_date", "expiry_date", "load_date", "current_record")
  
  all_cols <- .data %>%  
    colnames() %>% 
    setdiff(ignore_cols) 
  
  value_cols <- setdiff(all_cols, key_cols)
  
  if (length(all_cols) == length(value_cols)) {
    log_fatal("The key columns do not exist in the table!")
  } else {
    .data <- .data %>% 
      unite("key_combined", all_of(key_cols), sep = "|", remove = FALSE) %>% 
      mutate(key_hash = sapply(key_combined, digest, algo = "sha256")) %>% 
      unite("value_combined", all_of(value_cols), sep = "|", remove = FALSE) %>%
      mutate(value_hash = sapply(value_combined, digest, algo = "sha256")) %>% 
      select(-c(key_combined, value_combined))
    
    return(.data)
  }  
}

csv_scd2 <- function (historic_file, load_file, key_cols, run_date = today()) {
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
    load_table <- read_csv(load_file, show_col_types = FALSE) %>% 
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
    
    # need to verify that key hashes are unique in load table
    duplicate_load_key_hashes <- hashed_load_table %>% 
      group_by(key_hash) %>% 
      summarize(
        n_key = n(),
        .groups = "drop"
      ) %>% 
      filter(n_key > 1)
    
    if (nrow(duplicate_load_key_hashes) > 0) {
      log_fatal("Duplicate key hashes detected in load file!")
    } 
    
    hashed_historic_table <- historic_table %>% 
      make_hash(key_cols)
    
    key_value_lookup <- bind_rows(hashed_historic_table, hashed_load_table) %>% 
      select(-c(effective_date, expiry_date, current_record, load_date)) %>% 
      distinct()
    
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
    
    return(out)
  }
    
    
}

