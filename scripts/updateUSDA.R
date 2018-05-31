## Update USDA production data from the website?

updateUSDA <- function() {
  
  library(tidyverse)
  url <- "https://apps.fas.usda.gov/psdonline/downloads/psd_coffee_csv.zip"
  tempfile <- tempfile()
  download.file(url, destfile = tempfile, quiet = TRUE)
  usdaRaw <- readr::read_csv(file = unz(tempfile, "psd_coffee.csv"), col_types = readr::cols())
  
  usda <- usdaRaw %>% 
    select(country = Country_Name, year = Market_Year, series = Attribute_Description, value = Value) %>% 
    mutate(year = year-1) %>% 
    # mutate(year = as.character(year - 1)) %>% 
    filter(series == "Production") %>% 
    mutate(country = case_when(
      .$country == "Congo (Brazzaville)" ~ "Congo",
      .$country == "Congo, (Kinshasa)" ~ "Congo, DR",
      .$country == "Yemen (Sanaa)" ~ "Yemen",
      TRUE ~ .$country
    )) %>% 
    # value = as.character(value)) %>% 
    filter(year >= max(year) - 4) %>% 
    select(country, year, production = value) %>% 
    mutate(date = lubridate::today(), source = "USDA")
  
  write_csv(usda, "newUSDA.csv")
  
 # googlesheets::gs_key('14Qlc9UYRQju3uOVO8-cEOjFDwf4Z5TnOLlcyHUgW9Ls') %>% 
 #    gs_read()
 #  
  googlesheets::gs_upload('newUSDA.csv', sheet_title = "usdaProd", overwrite = TRUE)
    
  return(usda)
  
}
