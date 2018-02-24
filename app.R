# Shiny App formatted with shinymaterial
# Two main functions (tabs): goCompare.R and predExports.R

options(shiny.autoreload = TRUE)

library(shiny)
library(shinymaterial)
library(tidyverse)
library(readxl)
library(plotly)
library(lubridate)
# setwd("~/Dropbox/Work/coffeeApp")
# library(DT)

# Import list of countries for drop-down
countryList <-
    read_excel(file.path("data", "country-list.xlsx")) %>%
    filter(type == "Exporting")

# Define UI for application that draws a histogram
ui <- material_page(
    tags$head(tags$style(
        HTML("
             .card .card-title {
             font-size: 14px !important;
             }
             ")
        )),
    
    
    # Application title
    title = "CoffeeStatsApp",
    
    # Country list sidebar
    material_side_nav(
        material_radio_button(
            input_id = "countryName",
            label = "Select country",
            color = "#469689",
            choices = countryList$country
        ),
        fixed = TRUE
    ),
    
    # Define tabs
    material_tabs(tabs = c(
        "Production" = "prod_tab",
        "Exports" = "exp_tab"
    )),
    
    # Production tab content
    material_tab_content(tab_id = "prod_tab",
                         material_row(
                             material_column(width = 6,
                                             material_card(tableOutput("tableProd"))),
                             material_column(width = 6,
                                             material_card(plotlyOutput("graphProd")))
                         )),
    
    material_tab_content(tab_id = "exp_tab",
                         material_row(
                             material_column(width = 6,
                                             material_card(tableOutput("expTab"))),
                             material_column(width = 6,
                                             material_card(plotlyOutput("expGraph")))
                         ))
        )

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Load data from USDA. NB Just saved as static data file. To update, run coffeestats::getUSDA()
    # then copy resulting file into data folder as usda.csv
    usda <-
        readr::read_csv(file = file.path("data", "usda.csv"))
    
    # Load data from ICO. Same as above
    ico <-
        readr::read_csv(file = file.path("data", "ico.csv"))
    
    # Get MTS. From static file, but needs to be updateable when new export data is released
    mts <-
        readr::read_csv(file = file.path("data", "mts.csv"),
                        col_types = readr::cols())
    
    # Get production-estimates, from Google Drive
    library(googlesheets)
    productionEstimates <-
        googlesheets::gs_key('1H_QsSAgzlgvoB3XWvilhiBUmISlLLd7FI6Q1yDhJMew') %>%
        gs_read() %>%
        group_by(country, source, year, date) %>%
        summarise(production = sum(production)) %>%
        mutate(date = lubridate::dmy(date))
    
    # Get my estimates from the flowsheets?
    flow <-
        readxl::read_excel(path = file.path("data", "flow.xlsx")) %>%
        tidyr::gather(.,
                      -series,
                      -source,
                      -country,
                      key = year,
                      value = value) %>%
        mutate(country = ifelse(grepl("Ivoire", country), "Cote d'Ivoire", country),
               year = as.integer(year)) %>%
        filter(source == "ME")
    
    # Combine production data?
    prodData <- dplyr::bind_rows(
        usda %>% filter(series == "Production", value > 0, year >= max(year) - 5) %>% select(country, year, production = value) %>% mutate(date = lubridate::today(), source = "USDA"),
        ico %>% select(country, year, production) %>% mutate(date = lubridate::today(), source = "ICO") %>% na.omit(),
        productionEstimates,
        flow %>% filter(series == "Production") %>% mutate(source = "Estimate", date = lubridate::today()) %>% select(country, year, source, date, production = value)
    ) %>%
        na.omit() %>%
        filter(year > max(year) - 4) %>%
        mutate(source = forcats::fct_relevel(source, "Estimate"))
    
    # Get table of production data
    output$tableProd <- renderTable(
        filter(prodData, country == input$countryName) %>%
            mutate(production = prettyNum(round(production, 0), big.mark = ",")) %>%
            select(-country,-date) %>%
            spread(key = year, value = production),
        digits = 0,
        na = "",
        striped = TRUE,
        width = "100%"
    )
    
    # output$tableProd <- DT::renderDataTable(
    #     filter(prodData, country == input$countryName) %>%
    #         mutate(production = prettyNum(round(production,0), big.mark = ",")) %>%
    #         select(-country, -date) %>%
    #         spread(key = year, value = production)
    # )
    
    output$graphProd <- renderPlotly({
        plot_ly(
            data = filter(prodData, country == input$countryName) %>%
                arrange(source, year, production) %>% group_by(source),
            type = "scatter",
            mode = "lines",
            x = ~ year,
            y = ~ production,
            color = ~ source,
            text = ~ paste0(source, ": ", round(production, 1), "<br>", date),
            hoverinfo = "text"
        ) %>%
            plotly::layout(
                title = paste("Production by", input$countryName),
                xaxis = list(title = "", showgrid = FALSE),
                yaxis = list(title = "", zeroline = FALSE)
            )
    })
    
    # output$graphProd <- renderPlot(
    #     prodData %>%
    #         filter(country == input$countryName) %>%
    #         ggplot(aes(x = year, y = production, group = source, color = source)) +
    #         geom_line() +
    #         scale_x_discrete("") +
    #         scale_y_continuous("") +
    #         ggtitle(paste("Proudction by", input$countryName))
    #         # theme(title = paste("Production by", input$countryName))
    #
    #
    # )
    
    
    # Export tab!
    # Import scaleExports & scaleExportsRaw and join together
    scaleGG <-
        googlesheets::gs_key('1FL04PtMs9hQBQDfeCY_WgG7qdre7uMuLVa-FkjuvlXE')
    scaleExports <- scaleGG %>% gs_read(ws = 'Sheet1')
    scaleExportsRaw <- scaleGG %>% gs_read(ws = 'Sheet2')
    
    
    # Get tables and graphs
    getCountryRaw <- reactive({
        full_join(
            x = filter(scaleExportsRaw, country == input$countryName) %>%
                arrange(month) %>%
                mutate(cropMonth = ordered(
                    lubridate::month(month),
                    levels = unique(lubridate::month(month)),
                    labels = unique(month.abb[lubridate::month(month)])
                )),
            y = filter(scaleExports, country == input$countryName),
            by = "country"
        )
    })
    
    getCountrySummary <- reactive({
        countryRaw <- getCountryRaw()
        full_join(
            x = countryRaw %>%
                filter(cropYear.x == cropYear.y) %>%
                select(country, cropMonth, actual = value, scaleExports),
            y = countryRaw %>%
                filter(cropYear.x != cropYear.y) %>%
                group_by(cropMonth) %>%
                mutate(
                    min = min(value),
                    max = max(value),
                    prediction = avShare * scaleExports
                ) %>%
                select(country, cropMonth, min, max, prediction) %>%
                slice(1)
        ) %>%
            full_join(
                x = .,
                y = countryRaw %>%
                    filter(cropYear.x == cropYear.y - 1) %>%
                    select(country, cropMonth, lastYear = value)
            ) %>%
            mutate(exportsToDate = cumsum(actual))
    })
    

    
    output$expGraph <- renderPlotly({
        countrySummary <- getCountrySummary()
        print(head(countrySummary))
        print(countrySummary$cropMonth)
        countrySummary <- countrySummary %>%
            mutate(cropMonth = ordered(
                cropMonth, levels = unique(cropMonth)
            ))
        print(countrySummary$cropMonth)
        plot_ly(countrySummary, x = ~cropMonth) %>%
            add_ribbons(
                name = "Min/max",
                line = list(width = 0),
                ymin = ~ min,
                ymax = ~ max,
                hoverinfo = "none",
                fillcolor = "#ffe090",
                opacity = 0.4
            ) %>%
            add_lines(
                name = "Prediction",
                y = ~ prediction,
                line = list(color = "#cc593d", dash = "dash"),
                text = ~ paste0(
                    cropMonth,
                    " prediction: ",
                    format(round(prediction, 1), big.mark = ",")
                ),
                hoverinfo = "text"
            ) %>%
            add_lines(
                name = "Actual",
                y = ~ actual,
                line = list(color = "#cc593d"),
                text = ~ paste0(cropMonth, " actual: ",
                                format(round(actual, 1), big.mark = ",")),
                hoverinfo = "text"
            ) %>%
            add_lines(
                name = "Last year",
                y = ~ lastYear,
                line = list(color = "#659fb5"),
                text = ~ paste0(cropMonth, " last year: ",
                                format(round(lastYear, 1), big.mark = ",")),
                hoverinfo = "text"
            ) %>%
            layout(
                title = paste("Export prediction function for", input$countryName),
                xaxis = list(title = ""),
                yaxis = list(
                    title = "",
                    separatethousands = TRUE,
                    ticklen = 20,
                    tickcolor = "#FFF"
                )
            )
    })
    
    output$expTab <- renderTable({
        countrySummary <- getCountrySummary()
        countrySummary %>% 
            select(Month = cropMonth, Actual = actual, Prediction = prediction, 
                   exportsToDate, totalPrediction = scaleExports)
    },
    digits = 0,
    na = "",
    format.args = list(big.mark = ",")
    # striped = TRUE
    )
}

# Run the application
shinyApp(ui = ui, server = server)

