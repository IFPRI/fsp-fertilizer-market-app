# #Fertilizer dashboard
#CURRENT VERSION: 2026_03_11
# ── Packages ──────────────────────────────────────────────────────────────────
library(shiny)
library(lubridate)
library(plotly)
library(shinyalert)
library(tidyverse)
library(magrittr)
library(maps)
library(igoR)
library(countrycode)
library(giscoR)
library(rjson)
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)
library(shinyBS)
library(shinycssloaders)
library(colorspace)
library(shinydashboard)
library(rsconnect)

# ── Options ───────────────────────────────────────────────────────────────────
options(scipen = 999)

# ── Data loading ──────────────────────────────────────────────────────────────
today <- Sys.Date()

load(file = "./countryShapeFiles.RData")
world <- sf::st_cast(world, "MULTIPOLYGON")

world_forDepRatio <- world %>%
  dplyr::rename("Country_Code" = ISO3_CODE)

countryClassificationImported <- readRDS("countryClassification.rds")
countryClassification <- countryClassificationImported %>%
  select(ISO3Code, WBIncomeClassification) %>%
  dplyr::rename(ISO3_CODE = ISO3Code) %>%
  filter(!is.na(ISO3_CODE))

# Fertilizer and natural gas prices from WB Pink Sheets
WB_pricesImported <- readRDS("WBPinkSheetFertilizer_clean.rds")

WB_fertilizer_prices <- WB_pricesImported %>%
  filter(Commodity %in% c("DAP", "TSP", "Urea", "Potassium chloride")) %>%
  filter(!str_detect(Unit, "Index"))

WB_NatGas_prices <- WB_pricesImported %>%
  filter(Commodity %in% c("Natural gas, US", "Natural gas, Europe")) %>%
  filter(!str_detect(Unit, "Index"))

WB_pricesIndexed <- WB_pricesImported %>%
  filter(str_detect(Unit, "Index")) %>%
  filter(!Commodity %in% c("Crude oil, average", "Natural gas index", "Phosphate rock"))

prices         <- readRDS("prices.rds")
fertilizer.use <- readRDS("FertilizerUse.rds")
usebroad       <- fertilizer.use

applicationImported <- readRDS("application.rds")
application         <- applicationImported

applicationForYield <- application %>%
  dplyr::rename(ISO3_CODE = "Country_Code") %>%
  filter(!is.na(ISO3_CODE)) %>%
  left_join(countryClassification)

# Import dependency
importdep_world <- fertilizer.use %>%
  filter(variable == "DependencyRatio")

# ── Input choices based on data availability ──────────────────────────────────
yearBasedOnDataAvail <- usebroad %>%
  count(Year) %>% arrange(desc(Year)) %>% pull(Year)

yearBasedOnUseDataAvail <- usebroad %>%
  filter(variable == "Agricultural Use") %>%
  count(Year) %>% arrange(desc(Year)) %>% pull(Year)

yearBasedOnApplicationDataAvail <- application %>%
  count(Year) %>% arrange(desc(Year)) %>% pull(Year)

yearBasedOnUseDataAvailYields <- c("All years", yearBasedOnUseDataAvail)

yearBasedOnProductionDataAvail <- usebroad %>%
  filter(variable == "Production") %>%
  count(Year) %>% arrange(desc(Year)) %>% pull(Year)

countryClassificationForInput <- c(
  "All countries", "Low Income", "Lower Middle Income",
  "Upper Middle Income", "High Income"
)

# ── General cleaning ──────────────────────────────────────────────────────────
prices <- prices %>%
  mutate(date = as.Date(date, "%Y-%m-%d"))

`%notin%` <- Negate(`%in%`)

# Application rate (excluding aggregates)
appRate <- application %>%
  filter(variable == "Application rate") %>%
  filter(Country_Name %notin% c("World", "European Union")) %>%
  dplyr::rename(ISO3_CODE = "Country_Code")

# Production (excluding aggregates)
production_tobefiltered <- usebroad %>%
  filter(variable == "Production") %>%
  filter(Country_Name != "World") %>%
  filter(Country_Name != "European Union") %>%
  dplyr::rename(ISO3_CODE = "Country_Code")

# ── Color scales ──────────────────────────────────────────────────────────────
scale_color_manual_fertizerCategories <- function(...) {
  ggplot2:::scale_color_manual(
    values = setNames(
      c("firebrick2", "darkolivegreen4", "deepskyblue2"),
      c("Nitrogenous", "Phosphate", "Potash")
    ), ...
  )
}

scale_color_manual_fertizerProducts <- function(...) {
  ggplot2:::scale_color_manual(
    values = setNames(
      c("darkred", "darkseagreen3", "dodgerblue", "darkkhaki", "springgreen3", "darksalmon"),
      c("Ammonia", "DAP", "MOP", "MAP", "Phosphate Rock", "Urea")
    ), ...
  )
}

scale_color_manual_fertizerNutrientCategories <- function(...) {
  ggplot2:::scale_color_manual(
    values = setNames(
      c("firebrick2", "darkolivegreen4", "deepskyblue2", "sienna1"),
      c("N", "P", "K", "NPK Index")
    ), ...
  )
}

scale_color_manual_natGas <- function(...) {
  ggplot2:::scale_color_manual(
    values = setNames(
      c("salmon2", "seagreen3"),
      c("Natural Gas - EU", "Natural Gas - USA")
    ), ...
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- fluidPage(
  
  titlePanel(
    title      = div(img(src = "Logo_FSP_EC_RGB - Copy.png", height = 50), "Fertilizer Dashboard"),
    windowTitle = "Fertilizer Dashboard"
  ),
  
  navbarPage(
    title    = "",
    selected = "Prices",
    
    # ── PRICES TAB ────────────────────────────────────────────────────────────
    tabPanel("Prices", fluid = TRUE,
             column(12,
                    tabsetPanel(id = "prices_tabs",
                                
                                # Fertilizer prices
                                tabPanel("Fertilizer prices",
                                         fluidRow(column(12, HTML("<br/>"),
                                                         fluidRow(
                                                           column(4,
                                                                  wellPanel(
                                                                    sliderInput("Date_FertPrices", "Date range",
                                                                                min       = min(WB_fertilizer_prices$Date),
                                                                                max       = max(WB_fertilizer_prices$Date),
                                                                                value     = c(as.Date("2020-01-01"), max(WB_fertilizer_prices$Date)),
                                                                                timeFormat = "%b %Y"
                                                                    ),
                                                                    selectInput(
                                                                      inputId  = "Fert_selected",
                                                                      label    = tags$span(
                                                                        "Select fertilizer type:",
                                                                        bsButton("productdescriptions", label = "", icon = icon("info"),
                                                                                 style = "info", size = "small")
                                                                      ),
                                                                      choices  = unique(WB_fertilizer_prices$Commodity),
                                                                      selected = unique(WB_fertilizer_prices$Commodity),
                                                                      multiple = TRUE,
                                                                      selectize = TRUE
                                                                    ),
                                                                    bsPopover(
                                                                      id        = "productdescriptions",
                                                                      title     = "Description of fertilizer products",
                                                                      content   = paste0(
                                                                        "<b>DAP (diammonium phosphate)</b>: spot, f.o.b. US Gulf<br>",
                                                                        "<b>Urea</b>: prill spot f.o.b. Middle East (from March 2022); previously f.o.b. Black Sea.<br>",
                                                                        "<b>TSP (triple superphosphate)</b>: spot, import US Gulf<br>",
                                                                        "<b>Potassium chloride (muriate of potash)</b>: Brazil CFR granular spot price (from January 2020); previously f.o.b. Vancouver"
                                                                      ),
                                                                      placement = "right",
                                                                      trigger   = "hover",
                                                                      options   = list(container = "body")
                                                                    )
                                                                  )
                                                           ),
                                                           column(8, plotlyOutput("plot1"))
                                                         )
                                         ))
                                ),
                                
                                # Natural gas prices
                                tabPanel("Fertilizer input prices",
                                         fluidRow(column(12, HTML("<br/>"),
                                                         fluidRow(
                                                           column(4,
                                                                  wellPanel(
                                                                    sliderInput("Date_NatGasPrices", "Date range",
                                                                                min       = min(WB_NatGas_prices$Date),
                                                                                max       = max(WB_NatGas_prices$Date),
                                                                                value     = c(as.Date("2020-01-01"), max(WB_NatGas_prices$Date)),
                                                                                timeFormat = "%b %Y"
                                                                    )
                                                                  )
                                                           ),
                                                           column(8, plotlyOutput("plotNatGas"))
                                                         )
                                         ))
                                ),
                                
                                # Fertilizer & natural gas indexed
                                tabPanel("Fertilizer and natural gas price relationship",
                                         fluidRow(column(12, HTML("<br/>"),
                                                         fluidRow(
                                                           column(4,
                                                                  wellPanel(
                                                                    sliderInput("Date_NatGas_fert_Prices", "Date range",
                                                                                min       = min(WB_pricesIndexed$Date),
                                                                                max       = max(WB_pricesIndexed$Date),
                                                                                value     = c(as.Date("2000-01-01"), max(WB_pricesIndexed$Date)),
                                                                                timeFormat = "%b %Y"
                                                                    ),
                                                                    selectInput(
                                                                      inputId  = "FertNatGas_selected",
                                                                      label    = tags$span(
                                                                        "Select items:",
                                                                        bsButton("productdescriptions_natGasFert", label = "", icon = icon("info"),
                                                                                 style = "info", size = "small")
                                                                      ),
                                                                      choices  = unique(WB_pricesIndexed$Commodity),
                                                                      selected = unique(WB_pricesIndexed$Commodity),
                                                                      multiple = TRUE,
                                                                      selectize = TRUE
                                                                    ),
                                                                    bsPopover(
                                                                      id        = "productdescriptions_natGasFert",
                                                                      title     = "Description of fertilizer products",
                                                                      content   = paste0(
                                                                        "<b>DAP</b>: spot, f.o.b. US Gulf<br>",
                                                                        "<b>Urea</b>: prill spot f.o.b. Middle East (from March 2022); previously f.o.b. Black Sea.<br>",
                                                                        "<b>TSP</b>: spot, import US Gulf<br>",
                                                                        "<b>Potassium chloride</b>: Brazil CFR granular spot price (from January 2020); previously f.o.b. Vancouver<br>",
                                                                        "<b>Natural gas Europe</b>: from April 2015, Netherlands TTF; earlier, average import border price incl. UK.<br>",
                                                                        "<b>Natural gas United States</b>: spot price at Henry Hub, Louisiana"
                                                                      ),
                                                                      placement = "right",
                                                                      trigger   = "hover",
                                                                      options   = list(container = "body")
                                                                    )
                                                                  )
                                                           ),
                                                           column(8, plotlyOutput("plotNatGasFert"))
                                                         )
                                         ))
                                )
                                
                    ) # end tabsetPanel prices
             )
    ), # end Prices tab
    
    # ── TRADE TAB ─────────────────────────────────────────────────────────────
    tabPanel("Trade", fluid = TRUE,
             column(12,
                    tabsetPanel(id = "trade_tabs",
                                
                                # Main exporters and importers
                                tabPanel("Main exporters and importers",
                                         fluidRow(column(12, HTML("<br/>"),
                                                         column(4,
                                                                wellPanel(
                                                                  helpText("Import and export quantities are shown in metric tons nutrient equivalent."),
                                                                  radioButtons(
                                                                    inputId  = "impExpQuantity_nutrient",
                                                                    label    = "Select:",
                                                                    choices  = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"),
                                                                    selected = "Nitrogenous"
                                                                  ),
                                                                  selectInput(
                                                                    inputId  = "impExpQuantity_year",
                                                                    label    = "Year:",
                                                                    choices  = yearBasedOnDataAvail,
                                                                    selected = max(yearBasedOnDataAvail)
                                                                  )
                                                                )
                                                         ),
                                                         column(8,
                                                                fluidRow(
                                                                  column(6, withSpinner(plotlyOutput("mainImporters"))),
                                                                  column(6, withSpinner(plotlyOutput("mainExporters")))
                                                                )
                                                         )
                                         ))
                                ),
                                
                                # Net imports
                                tabPanel("Net imports",
                                         fluidRow(column(12, HTML("<br/>"),
                                                         column(4,
                                                                wellPanel(
                                                                  helpText("Net imports are the total imports minus total exports of a country."),
                                                                  radioButtons(
                                                                    inputId  = "netImportsOutput",
                                                                    label    = "Select countries shown:",
                                                                    choices  = list("Top 10 net importers/exporters" = 1, "All countries (map output)" = 2),
                                                                    selected = 1
                                                                  ),
                                                                  radioButtons(
                                                                    inputId  = "nutrientImports",
                                                                    label    = "Select:",
                                                                    choices  = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"),
                                                                    selected = "Nitrogenous"
                                                                  ),
                                                                  selectInput(
                                                                    inputId  = "dateImports",
                                                                    label    = "Year:",
                                                                    choices  = yearBasedOnDataAvail,
                                                                    selected = max(yearBasedOnDataAvail)
                                                                  )
                                                                )
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.netImportsOutput == 2",
                                                           column(8, withSpinner(plotlyOutput("mapNetImports", height = "500px")))
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.netImportsOutput == 1",
                                                           column(8,
                                                                  fluidRow(column(12, withSpinner(plotlyOutput("plotImportsByNutrient")))),
                                                                  fluidRow(column(12,
                                                                                  hr(style = "margin-bottom:0;"),
                                                                                  helpText("Source: FAOSTAT (with adjustments)", style = "font-size:12px;")
                                                                  ))
                                                           )
                                                         )
                                         ))
                                ),
                                
                                # Import dependency
                                tabPanel("Import dependency",
                                         fluidRow(column(12, HTML("<br/>"),
                                                         column(3,
                                                                wellPanel(
                                                                  helpText("Ratio between imports and agricultural use (0 = no imports; 1 = domestic use fully supplied by imports)."),
                                                                  radioButtons(
                                                                    inputId  = "dep_nutrient",
                                                                    label    = "Select:",
                                                                    choices  = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"),
                                                                    selected = "Nitrogenous"
                                                                  ),
                                                                  selectInput(
                                                                    inputId  = "impDepMapYear",
                                                                    label    = "Year:",
                                                                    choices  = yearBasedOnDataAvail,
                                                                    selected = max(yearBasedOnDataAvail)
                                                                  )
                                                                )
                                                         ),
                                                         column(9,
                                                                fluidRow(column(12, withSpinner(plotlyOutput("map_impdep", height = "500px")))),
                                                                fluidRow(column(12,
                                                                                hr(style = "margin-bottom:0; margin-top:0;"),
                                                                                helpText("Source: Computation based on FAOSTAT", style = "font-size:12px;")
                                                                ))
                                                         )
                                         ))
                                )
                                
                    ) # end tabsetPanel trade
             )
    ), # end Trade tab
    
    # ── USE TAB ───────────────────────────────────────────────────────────────
    tabPanel("Use", fluid = TRUE,
             column(12,
                    tabsetPanel(id = "use_tabs",
                                
                                # Agricultural use
                                tabPanel("Use (in MT)",
                                         fluidRow(column(12, HTML("<br/>"),
                                                         column(4,
                                                                wellPanel(
                                                                  helpText("Agricultural use of fertilizer in metric tons nutrient equivalent."),
                                                                  selectInput(
                                                                    inputId  = "useYear",
                                                                    label    = "Select year:",
                                                                    choices  = yearBasedOnUseDataAvail,
                                                                    selected = max(yearBasedOnUseDataAvail)
                                                                  ),
                                                                  radioButtons(
                                                                    inputId  = "useNutrient",
                                                                    label    = "Select nutrient:",
                                                                    choices  = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"),
                                                                    selected = "Nitrogenous"
                                                                  )
                                                                )
                                                         ),
                                                         column(8,
                                                                fluidRow(column(12, withSpinner(plotlyOutput("usePlot")))),
                                                                fluidRow(column(12,
                                                                                hr(style = "margin-bottom:0;"),
                                                                                helpText("Source: FAOSTAT", style = "font-size:12px;")
                                                                ))
                                                         )
                                         ))
                                ),
                                
                                # Application rates
                                tabPanel("Application rates",
                                         fluidRow(column(12, HTML("<br/>"),
                                                         column(3,
                                                                wellPanel(
                                                                  helpText("Application rates are the total kilograms (kg) of nutrient per hectare (ha) of cropland."),
                                                                  selectInput(
                                                                    inputId  = "yearApplicationRt",
                                                                    label    = "Select year:",
                                                                    choices  = yearBasedOnApplicationDataAvail,
                                                                    selected = max(yearBasedOnApplicationDataAvail)
                                                                  ),
                                                                  radioButtons(
                                                                    inputId  = "nutrientApplicationRt",
                                                                    label    = "Select nutrient:",
                                                                    choices  = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"),
                                                                    selected = "Nitrogenous"
                                                                  )
                                                                )
                                                         ),
                                                         column(9,
                                                                fluidRow(column(12, withSpinner(plotlyOutput("applicationRateMap", height = "500px")))),
                                                                fluidRow(column(12,
                                                                                hr(style = "margin-bottom:0;"),
                                                                                helpText("Source: Computation based on FAOSTAT", style = "font-size:12px;")
                                                                ))
                                                         )
                                         ))
                                ),
                                
                                # Fertilizer usage and yields
                                tabPanel("Fertilizer usage and yields",
                                         fluidRow(column(12, HTML("<br/>"),
                                                         column(3,
                                                                wellPanel(
                                                                  helpText("Relation between application rates and cereal yields."),
                                                                  selectInput(
                                                                    inputId  = "yearUseYields",
                                                                    label    = "Select year:",
                                                                    choices  = yearBasedOnUseDataAvailYields,
                                                                    selected = max(yearBasedOnUseDataAvailYields)
                                                                  ),
                                                                  checkboxGroupInput(
                                                                    inputId  = "nutrientUseYields",
                                                                    label    = "Select nutrient:",
                                                                    choices  = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"),
                                                                    selected = c("Nitrogenous", "Phosphate", "Potash")
                                                                  ),
                                                                  selectizeInput(
                                                                    inputId  = "yieldCountryClassification",
                                                                    label    = "Select country classification:",
                                                                    choices  = countryClassificationForInput,
                                                                    selected = "All countries"
                                                                  ),
                                                                  hr(style = "border-top: 1px solid #000000;"),
                                                                  radioButtons(
                                                                    inputId  = "useYieldLine",
                                                                    label    = "Output shown:",
                                                                    choices  = list("Include trendline" = 1, "No trendline" = 2),
                                                                    selected = 1
                                                                  )
                                                                )
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.useYieldLine == 1",
                                                           column(9,
                                                                  fluidRow(column(12, withSpinner(plotlyOutput("useYields")))),
                                                                  fluidRow(column(12,
                                                                                  hr(),
                                                                                  helpText("Source: Computation based on FAOSTAT (fertilizer use, cropland, and yields)")
                                                                  ))
                                                           )
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.useYieldLine == 2",
                                                           column(9,
                                                                  fluidRow(column(12, withSpinner(plotlyOutput("useYieldsNoLine")))),
                                                                  fluidRow(column(12,
                                                                                  hr(),
                                                                                  helpText("Source: Computation based on FAOSTAT (fertilizer use, cropland, and yields)")
                                                                  ))
                                                           )
                                                         )
                                         ))
                                )
                                
                    ) # end tabsetPanel use
             )
    ), # end Use tab
    
    # ── PRODUCTION TAB ────────────────────────────────────────────────────────
    tabPanel("Production", fluid = TRUE,
             fluidRow(column(12, HTML("<br/>"),
                             column(3,
                                    wellPanel(
                                      helpText("Fertilizer production in metric tons by country."),
                                      selectInput(
                                        inputId  = "productionYear",
                                        label    = "Select year:",
                                        choices  = yearBasedOnProductionDataAvail,
                                        selected = max(yearBasedOnProductionDataAvail)
                                      ),
                                      radioButtons(
                                        inputId  = "productionNutrient",
                                        label    = "Select nutrient:",
                                        choices  = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"),
                                        selected = "Nitrogenous"
                                      ),
                                      hr(style = "border-top: 1px solid #000000;"),
                                      radioButtons(
                                        inputId  = "productionMapOrBubble",
                                        label    = "Output shown:",
                                        choices  = list("Map" = 1, "Treemap" = 2),
                                        selected = 2
                                      )
                                    )
                             ),
                             conditionalPanel(
                               condition = "input.productionMapOrBubble == 2",
                               column(9,
                                      fluidRow(column(12, withSpinner(plotlyOutput("productionPlot")))),
                                      fluidRow(column(12,
                                                      hr(style = "margin-bottom:0;"),
                                                      helpText("Source: FAOSTAT", style = "font-size:12px;")
                                      ))
                               )
                             ),
                             conditionalPanel(
                               condition = "input.productionMapOrBubble == 1",
                               column(9,
                                      fluidRow(column(12, withSpinner(plotlyOutput("productionMap", height = "500px")))),
                                      fluidRow(column(12,
                                                      hr(style = "margin-bottom:0;"),
                                                      helpText("Source: FAOSTAT", style = "font-size:12px;")
                                      ))
                               )
                             )
             ))
    ) # end Production tab
    
  ) # end navbarPage
) # end fluidPage


# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # Welcome modal
  shinyalert(
    title              = "Welcome to the Fertilizer Dashboard",
    text               = "The top bar allows you to explore fertilizer prices, trade, use, and production.",
    size               = "m",
    closeOnEsc         = TRUE,
    closeOnClickOutside = TRUE,
    showConfirmButton  = TRUE,
    confirmButtonText  = "OK",
    confirmButtonCol   = "#AEDEF4"
  )
  
  # ── PRICES ─────────────────────────────────────────────────────────────────
  
  output$plot1 <- renderPlotly({
    WB_fertilizer_prices %>%
      filter(Date >= min(input$Date_FertPrices), Date <= max(input$Date_FertPrices)) %>%
      filter(Commodity %in% input$Fert_selected) %>%
      plot_ly(x = ~Date, y = ~Value, color = ~Commodity, type = "scatter", mode = "lines") %>%
      layout(
        title  = "Fertilizer Prices Over Time",
        xaxis  = list(title = ""),
        yaxis  = list(title = WB_fertilizer_prices %>% slice(1) %>% pull(Unit)),
        legend = list(title = list(text = "Fertilizer")),
        annotations = list(list(
          x = 1, y = -0.1, xref = "paper", yref = "paper",
          showarrow = FALSE, text = "Source: World Bank Pink Sheet",
          xanchor = "right", font = list(size = 8, color = "gray")
        ))
      )
  })
  
  output$plotNatGas <- renderPlotly({
    WB_NatGas_prices %>%
      filter(Date >= min(input$Date_NatGasPrices), Date <= max(input$Date_NatGasPrices)) %>%
      plot_ly(x = ~Date, y = ~Value, color = ~Commodity, type = "scatter", mode = "lines") %>%
      layout(
        title  = "Natural Gas Prices Over Time",
        xaxis  = list(title = ""),
        yaxis  = list(title = WB_NatGas_prices %>% slice(1) %>% pull(Unit)),
        legend = list(title = list(text = "Natural Gas Market")),
        annotations = list(list(
          x = 1, y = -0.1, xref = "paper", yref = "paper",
          showarrow = FALSE, text = "Source: World Bank Pink Sheet",
          xanchor = "right", font = list(size = 8, color = "gray")
        ))
      )
  })
  
  output$plotNatGasFert <- renderPlotly({
    base <- WB_pricesIndexed %>%
      filter(Date >= min(input$Date_NatGas_fert_Prices),
             Date <= max(input$Date_NatGas_fert_Prices)) %>%
      filter(Commodity %in% input$FertNatGas_selected)
    
    plot_ly() %>%
      add_trace(
        data = base %>% filter(Commodity %in% c("DAP", "TSP", "Urea", "Potassium chloride")),
        x = ~Date, y = ~Value, color = ~Commodity,
        type = "scatter", mode = "lines"
      ) %>%
      add_trace(
        data = base %>% filter(Commodity == "Natural gas, US"),
        x = ~Date, y = ~Value, type = "scatter", mode = "lines",
        line = list(color = "black"), name = "Natural gas, US"
      ) %>%
      add_trace(
        data = base %>% filter(Commodity == "Natural gas, Europe"),
        x = ~Date, y = ~Value, type = "scatter", mode = "lines",
        line = list(color = "black", dash = "dot"), name = "Natural gas, EU"
      ) %>%
      layout(
        title  = "Indexed Fertilizer and Natural Gas Prices",
        xaxis  = list(title = ""),
        yaxis  = list(title = WB_pricesIndexed %>% slice(1) %>% pull(Unit)),
        annotations = list(list(
          x = 1, y = -0.1, xref = "paper", yref = "paper",
          showarrow = FALSE, text = "Source: World Bank Pink Sheet",
          xanchor = "right", font = list(size = 8, color = "gray")
        ))
      )
  })
  
  # ── TRADE – Main importers / exporters ─────────────────────────────────────
  
  impAndExportQuant_base <- reactive({
    usebroad %>%
      filter(variable %in% c("Export quantity", "Import quantity")) %>%
      filter(Nutrient == input$impExpQuantity_nutrient) %>%
      filter(Year == input$impExpQuantity_year) %>%
      filter(Country_Name != "World", Country_Name != "European Union") %>%
      filter(value != 0)
  })
  
  importQuant_ready <- reactive({
    top5 <- impAndExportQuant_base() %>%
      filter(variable == "Import quantity") %>%
      group_by(Nutrient) %>% arrange(desc(value)) %>% slice(1:5) %>% ungroup()
    
    row <- impAndExportQuant_base() %>%
      filter(variable == "Import quantity") %>%
      group_by(Nutrient) %>% arrange(desc(value)) %>% slice(6:n()) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Country_Name = "Rest of the world", Country_Code = "RoW",
             variable = "Import quantity")
    
    bind_rows(top5, row) %>%
      group_by(Nutrient) %>%
      mutate(shares = value / sum(value)) %>% ungroup() %>%
      mutate(Country_Name = factor(Country_Name,
                                   levels = c(setdiff(unique(Country_Name), "Rest of the world"), "Rest of the world")))
  })
  
  exportQuant_ready <- reactive({
    top5 <- impAndExportQuant_base() %>%
      filter(variable == "Export quantity") %>%
      group_by(Nutrient) %>% arrange(desc(value)) %>% slice(1:5) %>% ungroup()
    
    row <- impAndExportQuant_base() %>%
      filter(variable == "Export quantity") %>%
      group_by(Nutrient) %>% arrange(desc(value)) %>% slice(6:n()) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Country_Name = "Rest of the world", Country_Code = "RoW",
             variable = "Export quantity")
    
    bind_rows(top5, row) %>%
      group_by(Nutrient) %>%
      mutate(shares = value / sum(value)) %>% ungroup() %>%
      mutate(Country_Name = factor(Country_Name,
                                   levels = c(setdiff(unique(Country_Name), "Rest of the world"), "Rest of the world")))
  })
  
  output$mainImporters <- renderPlotly({
    plot_ly(importQuant_ready(),
            x = ~Nutrient, y = ~shares, color = ~Country_Name, type = "bar",
            text = ~paste0(Country_Name, " ", round(shares * 100, 0), "%"),
            textposition = "inside",
            insidetextfont = list(color = "black", size = 14, family = "Arial"),
            hoverinfo = "text", showlegend = FALSE
    ) %>%
      layout(
        title   = "Top fertilizer importers by nutrient",
        xaxis   = list(title = "", tickfont = list(size = 14)),
        yaxis   = list(title = "Share of global imports", tickformat = ".0%"),
        barmode = "stack",
        annotations = list(list(
          x = 0.05, y = -0.1, xref = "paper", yref = "paper",
          text = "Source: FAOSTAT (with adjustments)", showarrow = FALSE,
          font = list(size = 10), xanchor = "center", yanchor = "top"
        ))
      )
  })
  
  output$mainExporters <- renderPlotly({
    plot_ly(exportQuant_ready(),
            x = ~Nutrient, y = ~shares, color = ~Country_Name, type = "bar",
            text = ~paste0(Country_Name, " ", round(shares * 100, 0), "%"),
            textposition = "inside",
            insidetextfont = list(color = "black", size = 14, family = "Arial"),
            hoverinfo = "text", showlegend = FALSE
    ) %>%
      layout(
        title   = "Top fertilizer exporters by nutrient",
        xaxis   = list(title = "", tickfont = list(size = 14)),
        yaxis   = list(title = "Share of global exports", tickformat = ".0%"),
        barmode = "stack",
        annotations = list(list(
          x = 0.05, y = -0.1, xref = "paper", yref = "paper",
          text = "Source: FAOSTAT (with adjustments)", showarrow = FALSE,
          font = list(size = 10), xanchor = "center", yanchor = "top"
        ))
      )
  })
  
  # ── TRADE – Net imports ────────────────────────────────────────────────────
  
  netImportsByNutrient <- reactive({
    usebroad %>%
      filter(variable == "Net Imports") %>%
      filter(Year == input$dateImports) %>%
      filter(Nutrient == input$nutrientImports) %>%
      filter(Country_Name != "World", Country_Name != "European Union") %>%
      mutate(Net = case_when(
        value > 0 ~ "Net importer",
        value < 0 ~ "Net exporter",
        TRUE      ~ "Balanced"
      )) %>%
      group_by(Year, Nutrient) %>%
      arrange(desc(value), .by_group = TRUE) %>%
      slice(c(1:10, (n() - 10):n()))
  })
  
  netImportWorld <- reactive({
    usebroad %>%
      filter(variable == "Net Imports") %>%
      filter(Year == input$dateImports) %>%
      filter(Nutrient == input$nutrientImports) %>%
      filter(Country_Name != "World", Country_Name != "European Union") %>%
      left_join(world_forDepRatio, .) %>%
      mutate(
        text_intermediate = paste(NAME_ENGL, "\n", value, "\nnet imports in MT"),
        text = ifelse(is.na(value), "No data available for this country", text_intermediate)
      )
  })
  
  output$plotImportsByNutrient <- renderPlotly({
    plot_ly(netImportsByNutrient(),
            y = ~Country_Name, x = ~value, color = ~Net, type = "bar"
    ) %>%
      layout(
        title  = paste(input$nutrientImports, "fertilizer net imports —", input$dateImports),
        yaxis  = list(categoryorder = "total ascending", title = ""),
        xaxis  = list(title = "Metric tons")
      )
  })
  
  output$mapNetImports <- renderPlotly({
    netImports_gg <- netImportWorld() %>%
      ggplot() +
      geom_sf(aes(fill = value, text = text), show.legend = TRUE) +
      coord_sf(crs = "ESRI:54030",
               xlim = c(-11000804, 12909125), ylim = c(-3000000, 7500000),
               expand = FALSE) +
      scale_fill_continuous_divergingx(palette = "Earth", rev = TRUE, mid = 0) +
      labs(fill = "MT") +
      theme(
        plot.background = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank()
      )
    
    netImports_gg %>%
      ggplotly(tooltip = "text") %>%
      style(hoveron = "fill") %>%
      config(scrollZoom = TRUE) %>%
      layout(
        title  = list(
          text = paste0("Net imports — ", input$dateImports,
                        "<br><sup>Metric tons nutrient equivalent</sup>"),
          x    = 0.5,
          xanchor = "center"
        ),
        margin = list(t = 80, b = 20, l = 10, r = 10)
      )
  })
  
  # ── TRADE – Import dependency ──────────────────────────────────────────────
  
  importDepData <- reactive({
    importdep_world %>%
      mutate_if(is.numeric, ~round(., 2)) %>%
      filter(Nutrient == input$dep_nutrient) %>%
      filter(Year == input$impDepMapYear) %>%
      left_join(world_forDepRatio, .) %>%
      mutate(
        text_intermediate = paste(Country_Name, "\nDependency ratio:", value),
        text = ifelse(is.na(value), "No data available for this country", text_intermediate)
      )
  })
  
  output$map_impdep <- renderPlotly({
    importdep_world_gg <- importDepData() %>%
      ggplot() +
      geom_sf(aes(fill = value, text = text, color = NAME_ENGL), show.legend = TRUE) +
      scale_fill_gradient(low = "#e7e1ef", high = "#dd1c77") +
      coord_sf(crs = "ESRI:54030",
               xlim = c(-11000804, 12909125), ylim = c(-3000000, 7500000),
               expand = FALSE) +
      labs(fill = "Ratio") +
      theme(
        plot.background = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank()
      )
    
    ggplotly(importdep_world_gg, tooltip = "text") %>%
      config(scrollZoom = TRUE) %>%
      layout(
        title  = list(
          text = paste0(
            input$dep_nutrient, " import dependency ratio — ", input$impDepMapYear,
            "<br><sup>Ratio between imports and agricultural use</sup>"
          ),
          x       = 0.5,
          xanchor = "center"
        ),
        margin = list(t = 80, b = 20, l = 10, r = 10)
      ) %>%
      style(hoveron = "fills", line.color = toRGB("gray64")) %>%
      plotly::hide_legend()
  })
  
  # ── USE – Agricultural use treemap ────────────────────────────────────────
  
  use <- reactive({
    usebroad %>%
      filter(variable == "Agricultural Use") %>%
      filter(Nutrient == input$useNutrient) %>%
      filter(Year == input$useYear) %>%
      filter(Country_Name != "World", Country_Name != "European Union") %>%
      filter(value != 0) %>%
      mutate(valueComma = scales::comma(value))
  })
  
  output$usePlot <- renderPlotly({
    use_data <- use() %>% arrange(desc(value))
    
    plot_ly(
      data       = use_data,
      type       = "treemap",
      labels     = ~Country_Name,
      parents    = "",
      values     = ~value,
      text       = ~paste0("<b>", Country_Name, "</b><br>", valueComma, " MT nutrient equivalent"),
      hoverinfo  = "text",
      textinfo   = "label+percent entry"
    ) %>%
      layout(
        title = paste(input$useNutrient, "fertilizer agricultural use —", input$useYear),
        margin = list(t = 50),
        annotations = list(list(
          x = 1, y = -0.05, xref = "paper", yref = "paper",
          showarrow = FALSE, text = "Source: FAOSTAT",
          xanchor = "right", font = list(size = 8, color = "gray")
        ))
      )
  })
  
  # ── USE – Application rate map ────────────────────────────────────────────
  
  appRate_filtered <- reactive({
    application %>%
      mutate(Year = as.numeric(Year)) %>%
      mutate_if(is.numeric, ~round(., 2)) %>%
      filter(variable == "Application rate") %>%
      filter(Country_Name %notin% c("World", "European Union")) %>%
      dplyr::rename(ISO3_CODE = "Country_Code") %>%
      filter(Nutrient %in% input$nutrientApplicationRt) %>%
      filter(Year == input$yearApplicationRt) %>%
      mutate(
        text_intermediate = paste(Country_Name, "\n", value, "\nApplication Kg per Ha"),
        text = ifelse(is.na(value), "No data available for this country", text_intermediate)
      )
  })
  
  appRateWorld <- reactive({
    world %>%
      left_join(appRate_filtered(), by = "ISO3_CODE") %>%
      mutate(
        value = as.numeric(value),
        text_intermediate = paste(Country_Name, "\n", value, "Kg/Ha"),
        text = ifelse(is.na(value), "No data available for this country", text_intermediate)
      )
  })
  
  output$applicationRateMap <- renderPlotly({
    appRate_gg <- appRateWorld() %>%
      ggplot() +
      geom_sf(aes(fill = value, text = text), color = NA, show.legend = TRUE) +
      scale_fill_viridis_b() +
      coord_sf(crs = "ESRI:54030",
               xlim = c(-11000804, 12909125), ylim = c(-3000000, 7500000),
               expand = FALSE) +
      theme(
        plot.background = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank()
      )
    
    appRate_gg %>%
      ggplotly(tooltip = "text") %>%
      config(scrollZoom = TRUE) %>%
      style(hoveron = "fill") %>%
      layout(
        title  = list(
          text = paste0(
            "Average application rate",
            "<br><sup>", input$nutrientApplicationRt, " fertilizer — ", input$yearApplicationRt, "</sup>"
          ),
          x       = 0.5,
          xanchor = "center"
        ),
        margin = list(t = 80, b = 20, l = 10, r = 10)
      )
  })
  
  # ── USE – Fertilizer usage and yields ─────────────────────────────────────
  
  appRateYield_filtered <- reactive({
    applicationForYield %>%
      filter(variable == "Application rate") %>%
      filter(Nutrient %in% input$nutrientUseYields) %>%      # %in% for checkboxGroupInput
      filter(if (input$yearUseYields != "All years") Year == as.numeric(input$yearUseYields) else TRUE) %>%
      filter(if (input$yieldCountryClassification != "All countries")
        WBIncomeClassification %in% input$yieldCountryClassification else TRUE) %>%
      filter(Country_Name != "World", Country_Name != "European Union") %>%
      filter(`Yield, Cereals` >= 0, `Yield, Cereals` < 10) %>%
      mutate(text = paste(Country_Name, "\n", value, "\n", `Yield, Cereals`)) %>%
      dplyr::rename(`Application rate` = value)
  })
  
  observeEvent(input$yieldCountryClassification, {
    if (input$yieldCountryClassification != "All countries") {
      updateSelectInput(session, "yearUseYields", selected = "All years")
    }
  })
  
  output$useYields <- renderPlotly({
    useYields_gg <- appRateYield_filtered() %>%
      ggplot(aes(x = `Application rate`, y = `Yield, Cereals`, color = Nutrient)) +
      geom_point(aes(text = Country_Name), size = 1.5, shape = 1) +
      stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 0.5) +
      scale_color_manual_fertizerCategories() +
      labs(
        title = paste("Application rate vs. cereal yields —", input$yearUseYields),
        x = "Kg applied per Ha",
        y = "Cereal yield (tons per Ha)"
      )
    ggplotly(useYields_gg)
  })
  
  output$useYieldsNoLine <- renderPlotly({
    useYields_gg <- appRateYield_filtered() %>%
      ggplot(aes(x = `Application rate`, y = `Yield, Cereals`, color = Nutrient)) +
      geom_point(aes(text = Country_Name), size = 1.5, shape = 1) +
      scale_color_manual_fertizerCategories() +
      labs(
        x = "Kg applied per Ha",
        y = "Cereal yield (tons per Ha)"
      )
    ggplotly(useYields_gg)
  })
  
  # ── PRODUCTION – Treemap ──────────────────────────────────────────────────
  
  production <- reactive({
    usebroad %>%
      filter(variable == "Production") %>%
      filter(Nutrient == input$productionNutrient) %>%
      filter(Year == input$productionYear) %>%
      filter(Country_Name != "World", Country_Name != "European Union") %>%
      filter(value != 0) %>%
      mutate(valueComma = scales::comma(value))
  })
  
  output$productionPlot <- renderPlotly({
    prod_data <- production() %>% arrange(desc(value))
    
    plot_ly(
      data      = prod_data,
      type      = "treemap",
      labels    = ~Country_Name,
      parents   = "",
      values    = ~value,
      text      = ~paste0("<b>", Country_Name, "</b><br>", valueComma, " MT nutrient equivalent"),
      hoverinfo = "text",
      textinfo  = "label+percent entry"
    ) %>%
      layout(
        title = paste(input$productionNutrient, "fertilizer production —", input$productionYear),
        margin = list(t = 50),
        annotations = list(list(
          x = 1, y = -0.05, xref = "paper", yref = "paper",
          showarrow = FALSE, text = "Source: FAOSTAT",
          xanchor = "right", font = list(size = 8, color = "gray")
        ))
      )
  })
  
  # ── PRODUCTION – Map ──────────────────────────────────────────────────────
  
  productionMapFiltered <- reactive({
    production_tobefiltered %>%
      filter(Nutrient == input$productionNutrient) %>%
      filter(Year == input$productionYear) %>%
      left_join(world, .) %>%
      mutate(
        valueComma        = scales::comma(value),
        text_intermediate = paste(Country_Name, "\n", valueComma, "\nMetric Tons Nutrient Equivalent"),
        text              = ifelse(is.na(value), "No data available for this country", text_intermediate)
      )
  })
  
  output$productionMap <- renderPlotly({
    productionMap_gg <- productionMapFiltered() %>%
      ggplot() +
      geom_sf(aes(fill = value, text = text, color = NAME_ENGL)) +
      scale_fill_continuous_divergingx(palette = "Earth", rev = TRUE) +
      coord_sf(crs = "ESRI:54030",
               xlim = c(-11000804, 12909125), ylim = c(-3000000, 7500000),
               expand = FALSE) +
      labs(fill = "Production in MT") +
      theme(
        plot.background = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank()
      )
    
    ggplotly(productionMap_gg, tooltip = "text") %>%
      config(scrollZoom = TRUE) %>%
      layout(
        title  = list(
          text = paste0(
            input$productionNutrient, " fertilizer production — ", input$productionYear,
            "<br><sup>Metric tons nutrient equivalent</sup>"
          ),
          x       = 0.5,
          xanchor = "center"
        ),
        margin = list(t = 80, b = 20, l = 10, r = 10)
      ) %>%
      style(hoveron = "fills", line.color = toRGB("gray64")) %>%
      plotly::hide_legend()
  })
  
} # end server

# ── Run ───────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)


#deployApp(appName = "FertilizerDashboard_30July2024")
