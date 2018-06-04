## Update MTS from ICO website and sync with google drive file
getMTS <- function() {
  # Table location shouldn't change
  pdfTable <- 'http://www.ico.org/prices/m3-exports.pdf'
  
  # Use the tabulizer package to extract from pdf
  mts <- tabulizer::extract_tables(pdfTable, output = 'data.frame')[[1]] %>%
    gather(., -X, key = month, value = value) %>%
    # Fix some of the results
    mutate(
      # Use case_when (dplyr) to correct some of the names
      X = case_when(
        grepl("Ivoire", .$X) ~ "Cote d'Ivoire",
        grepl("Congo, Dem. Rep. of", .$X) ~ "Congo, DR",
        TRUE ~ .$X
      ),
      # Trim white space,
      X = trimws(X),
      # Create dates
      month = lubridate::dmy(paste0("01.", month)),
      value = as.numeric(gsub("[[:space:]]", "", value))
    ) %>%
    # Remove Colombian, Other and Brazilian; just keep Arabica/Robusta
    filter(!(X %in% c('Colombian Milds', 'Other Milds', 'Brazilian Naturals'))) %>%
    rename(country = X) %>%
    as_data_frame()
  
  # Update any existing data
  mtsKey <- googlesheets::gs_key('1oGCBkr_LjMuCYU7k6_zp0PEfP3X-SvcNXZdtYzGOLNA')
  oldMTS <- gs_read(mtsKey)
  
  if(class(oldMTS$month) != "Date") {
    oldMTS$month <- lubridate::dmy(oldMTS$month)
  }
  
  oldMTS <- filter(oldMTS, month < min(mts$month))
  
  # Bind together
  newMTS <- bind_rows(mts, oldMTS) %>%
    arrange(month) %>% 
    write_csv('newMTS.csv')
  
  # Upload back to googlesheets
  googlesheets::gs_upload('newMTS.csv', sheet_title = "MTS", overwrite = TRUE)
  
  return(newMTS)
 
}
