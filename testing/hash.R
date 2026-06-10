# hash function allows to easy comparison of key and value columns
make_hash <- function(.data, key_cols) {
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