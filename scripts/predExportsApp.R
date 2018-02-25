# Get a prediction dataframe for a specific country, and a specific dataset

# Function to get prediction. NB Copy into separate script later. 
predExportsApp <- function(enterCountry, dataset = mts) {
    
    
    exports <- full_join(dataset, countryList) %>% 
        select(-continent, -type) %>% 
        arrange(month) %>% 
        # Filter for selected country
        filter(country == enterCountry) %>% 
        # Add the crop year
        mutate(cropYear = ifelse(
            lubridate::month(month) == match(cyGroup, month.abb), lubridate::year(month), NA
        ),
        cropMonth = month.abb[lubridate::month(month)]) %>% 
        tidyr::fill(cropYear) %>% 
        filter(cropYear <= lubridate::year(month)) %>% 
        na.omit()
    
    # Get average flow for complete crop years
    exportsAv <- exports %>% 
        group_by(cropYear) %>% 
        filter(n() == 12) %>% 
        mutate(totalExports = sum(value), share = value/totalExports) %>% 
        ungroup() %>% 
        group_by(cropMonth) %>% 
        mutate(avShare = mean(share)) %>% 
        ungroup() %>% 
        distinct(cropMonth, avShare) %>% 
        mutate(cumSum = cumsum(avShare))
    
    # Now get the prediction
    # First get basics: min/max, last year and current actual
    prediction <- 
        full_join(
            x = exports %>% 
                group_by(cropYear) %>% 
                filter(n() == 12) %>% 
                group_by(cropMonth) %>% 
                summarise(min = min(value), max = max(value)),
            y = exports %>% 
                group_by(cropYear) %>% 
                filter(n() == 12) %>% 
                ungroup() %>% 
                filter(cropYear == max(cropYear)) %>% 
                select(cropMonth, month, lastYear = value),
            by = "cropMonth"
        ) %>% 
        full_join(
            x = .,
            y = exports %>% 
                filter(cropYear == max(cropYear)) %>% 
                select(cropMonth, actual = value),
            by = "cropMonth"
        ) %>% 
        full_join(
            x = .,
            y = exportsAv,
            by = "cropMonth"
        ) %>% 
        arrange(month) %>% 
        ## This bit gets complicated, just want to make a prediction based on
        ## the most recent month of data, so it looks more likely
        ### Make a copy of actual data, and fill down so prediction months are comparing to most recent month
        mutate(actualCopy = actual) %>% 
        tidyr::fill(actualCopy) %>% 
        ### Then compare the average share against most recent month for predictions
        mutate(avShareActual = ifelse(is.na(actual), actual, avShare)) %>% 
        tidyr::fill(avShareActual) %>% 
        mutate(
            diff = (avShare - avShareActual)/avShareActual,
            pred = ifelse(is.na(actual), actualCopy * (1 + diff), actual),
            cropMonth = forcats::fct_inorder(cropMonth),
            totalPred = sum(pred),
            avFlow = avShare * totalPred
        )
    
    return(prediction)
}
