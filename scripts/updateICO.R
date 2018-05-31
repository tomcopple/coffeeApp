## Update ICO production numbers and save in google drive.

updateICO <- function() {
  library(tidyverse)
  library(googlesheets)
  
  prodPDF <- "http://www.ico.org/prices/po-production.pdf"
  
  icoProdRaw <- tabulizer::extract_tables(prodPDF, output = "data.frame")[[1]]
  
  names(icoProdRaw) <- slice(icoProdRaw, 1)
  
  icoProd <- icoProdRaw %>% 
    select(country = 1, 2:5) %>% 
    slice(10:n()) %>% 
    mutate(country = case_when(
      stringr::str_detect(.$country, "Ivoire") ~ "Cote d'Ivoire",
      stringr::str_detect(.$country, "Congo, Dem") ~ "Congo, DR",
      stringr::str_detect(.$country, "Lao") ~ "Laos",
      TRUE ~ .$country
    )) %>% 
    gather(., -country, key = year, value = production) %>% 
    mutate(production = as.numeric(stringr::str_replace_all(production, "[[:space:]]", ""))) %>% 
    mutate(date = lubridate::today(), source = "ICO")
  
  write_csv(icoProd, "icoProd.csv")
  
  googlesheets::gs_upload('icoProd.csv', sheet_title = "icoProd", overwrite = TRUE)
  
  return(icoProd)
  
}