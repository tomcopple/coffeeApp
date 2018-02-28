# Shiny App formatted with shinymaterial
# Two main functions (tabs): goCompare.R and predExports.R

options(shiny.autoreload = TRUE)

library(shiny)
library(shinymaterial)
library(tidyverse)
library(readxl)
library(plotly)
library(lubridate)
library(googlesheets)
# setwd("coffeeApp")

# Import list of countries for drop-down
countryList <- readr::read_csv(file.path("data", "countries.csv")) %>% 
    filter(type == "Exporting")

# Define UI for application that draws a histogram
ui <- material_page(
    
    ## Some extra CSS to make card text a bit smaller
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
        "Exports" = "exp_tab",
        "Production" = "prod_tab",
        "Maintenance" = "maintenance_tab"
    )),
    
    # Production tab content
    material_tab_content(
        tab_id = "prod_tab",
        material_row(
            material_column(width = 6,
                            material_card(tableOutput("tableProd"))),
            material_column(width = 6,
                            material_card(plotlyOutput("graphProd")))
            )),
    
    # Export tab content
    material_tab_content(
        tab_id = "exp_tab",
        material_row(
            material_column(
                width = 6,
                material_card(tableOutput("expTab")),
                material_card(tableOutput("expTab2"))
                ),
            material_column(
                width = 6,
                material_card(plotlyOutput("expGraph")),
                ## Add exports card
                material_card(
                    h4("Add new export figure"),
                    p("NB This will be overwritten by the next ICO update"),
                    material_date_picker("enterMonth", "Enter export month"),
                    material_number_box("enterValue", "Enter value ('000 bags)",
                                        min_value = 0, max_value = 5000, initial_value = ""),
                    material_button("submitAddExp", "Submit", icon = "send"),
                    material_button("undoAddExp", "Undo", icon = "undo"))
                )
            )
        ),
    material_tab_content(
        tab_id = "maintenance_tab",
        material_card(
            material_button("updateICO", label = "Update ICO fundamentals", icon = "refresh")
        ), 
        material_card(
            material_button("updateUSDA", label = "Update all USDA data", icon = "refresh")
        ),
        material_card(
            material_button("getMTS", label = "Get ICO monthly trade stats", icon = "update")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Get Data
    ## 1. USDA, ICO, flow & MTS from local files
    usda <- readr::read_csv(file = file.path("data", "usda.csv"))
    ico <- readr::read_csv(file = file.path("data", "ico.csv"))
    mts <- readr::read_csv(file = file.path("data", "mts.csv"), col_types = readr::cols())
    flow <- readxl::read_excel(path = file.path("data", "flow.xlsx")) %>%
        tidyr::gather(., -series, -source, -country, key = year, value = value) %>%
        mutate(country = ifelse(grepl("Ivoire", country), "Cote d'Ivoire", country),
               year = as.integer(year)) %>%
        filter(source == "ME")
    # NB Eventually these will be updated and synced to Google Drive
    # Probably easier to just combine with the other production-estimates below?
    
    ## 2. Other production estimates from Google Drive
    productionEstimates <-
        googlesheets::gs_key('1H_QsSAgzlgvoB3XWvilhiBUmISlLLd7FI6Q1yDhJMew') %>%
        gs_read() %>%
        group_by(country, source, year, date) %>%
        summarise(production = sum(production)) %>%
        mutate(date = lubridate::dmy(date))
    
    ## 3. scaleExports from Google Drive
    ## Not anymore, just needs MTS
    
    ##  Combine production data? Might be easier to just save like this. 
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
    output$tableProd <- renderTable({
        filter(prodData, country == input$countryName) %>%
            # mutate(production = prettyNum(round(production, 0), big.mark = ",")) %>%
            select(-country,-date) %>%
            spread(key = year, value = production)
    },  digits = 0, na = "", format.args = list(big.mark = ","))
    
    # Plotly graph of production estimates
    output$graphProd <- renderPlotly({
        plot_ly(
            data = filter(prodData, country == input$countryName) %>%
                arrange(source, year, production) %>% group_by(source),
            type = "scatter", mode = "lines",
            x = ~ year, y = ~ production, color = ~ source,
            text = ~ paste0(source, ": ", round(production, 1), "<br>", date),
            hoverinfo = "text"
        ) %>%
            plotly::layout(
                title = paste("Production by", input$countryName),
                xaxis = list(title = "", showgrid = FALSE),
                yaxis = list(title = "", zeroline = FALSE)
            )
    })
    
    # Export tab
    source("scripts/predExportsApp.R")
    values <- reactiveValues(exportData = NULL)
    
    # If country name changes, run again
    observeEvent(input$countryName, {
        values$exportData <- predExportsApp(input$countryName)
    })
    
    # If someone input data and clicks submit, run again
    observeEvent(input$submitAddExp, {
        enterMonth <- input$enterMonth %>% 
            lubridate::dmy() %>% 
            lubridate::floor_date(unit = "month")
        enterValue <- input$enterValue
        enterCountry <- input$countryName
        
        # enterValue <- 1500
        # enterCountry <- "Colombia"
        # enterMonth <- lubridate::dmy("01-01-2018")
        
        if(!is.na(enterValue) & enterValue > 0) {
            newMTS <- bind_rows(
                mts, 
                data_frame(
                    country = enterCountry,
                    month = enterMonth,
                    value = enterValue
                )
            )
            
            values$exportData <- predExportsApp(enterCountry, dataset = newMTS)
        }
    })
    
    # If someone clicks undo, just go back to normal. 
    observeEvent(input$undoAddExp, {
        values$exportData <- predExportsApp(input$countryName)
    })
   
    # Plotly graph of exports
    output$expGraph <- renderPlotly({
        countrySummary <- values$exportData
        country <- input$countryName
        
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
                name = "Prediction", showlegend = FALSE,
                y = ~ pred,
                line = list(color = "#cc593d", dash = "dash"),
                text = ~ paste0(
                    cropMonth,
                    " prediction: ",
                    format(round(pred, 1), big.mark = ",")
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
                name = "Average flow",
                y = ~avFlow,
                line = list(color = "#4aac71", shape = "spline"),
                text = ~paste0(cropMonth, " avg flow: ",
                               format(round(avFlow, 1), big.mark = ",")),
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
                title = paste("Export flow for", country),
                xaxis = list(title = ""),
                yaxis = list(
                    title = "",
                    separatethousands = TRUE,
                    ticklen = 20,
                    tickcolor = "#FFF"
                )
            )
    })
    
    # Table of exports to date (excluding totalPrediction)
    output$expTab <- renderTable({
        countrySummary <- values$exportData
        country <- input$countryName
        countrySummary %>%
            mutate(
                exportsToDate = cumsum(actual),
                Month = ifelse(is.na(actual), 
                               paste0("<em>", cropMonth, "</em>"), 
                               as.character(cropMonth)),
                Exports = ifelse(is.na(actual), 
                                 paste0("<em>", prettyNum(round(pred,0),big.mark = ","), "</em>"),
                                 prettyNum(round(actual, 0), big.mark = ","))) %>% 
            select(Month, Exports, exportsToDate)
        },
        digits = 0, na = "", format.args = list(big.mark = ","),
        sanitize.text.function = function(x) x)
    
    # Table 2: summary of exports last year and prediction for this year
    output$expTab2 <- renderTable({
        country <- input$countryName
        countrySummary <- values$exportData
        countrySummary %>%
            mutate(totalLastYear = sum(lastYear)) %>%
            mutate(country = country) %>% 
            select(country, totalPrediction = totalPred, totalLastYear) %>%
            na.omit() %>%
            distinct()},
        digits = 0, na = "", format.args = list(big.mark = ","))
    
}

# Run the application
shinyApp(ui = ui, server = server)
