#version before 24Jan23


############loading packages###########
if("shiny" %in% installed.packages()[,1] == FALSE){
  install.packages("shiny")}
if("lubridate" %in% installed.packages()[,1] == FALSE){
  install.packages("lubridate")}
if("ggvis" %in% installed.packages()[,1] == FALSE){
  install.packages("ggvis")}
if("dplyr" %in% installed.packages()[,1] == FALSE){
  install.packages("dplyr")}
if("plotly" %in% installed.packages()[,1] == FALSE){
  install.packages("plotly")}
if("shinyalert" %in% installed.packages()[,1] == FALSE){
  install.packages("shinyalert")}
if("tidyverse" %in% installed.packages()[,1] == FALSE){
  install.packages("tidyverse")}
if("rsconnect" %in% installed.packages()[,1] == FALSE){
  install.packages("rsconnect")}
if("googlesheets4" %in% installed.packages()[,1] == FALSE){
  install.packages("googlesheets4")}
if("magrittr" %in% installed.packages()[,1] == FALSE){
  install.packages("magrittr")}
if("rvest" %in% installed.packages()[,1] == FALSE){
  install.packages("rvest")}
if("maps" %in% installed.packages()[,1] == FALSE){
  install.packages("maps")}
if("countrycode" %in% installed.packages()[,1] == FALSE){
  install.packages("countrycode")}
if("giscoR" %in% installed.packages()[,1] == FALSE){
  install.packages("giscoR")}
if("igoR" %in% installed.packages()[,1] == FALSE){
  install.packages("igoR")}
if("rjson" %in% installed.packages()[,1] == FALSE){
  install.packages("rjson")}

library(shiny)
library(lubridate)
library(ggvis)
library(dplyr)
library(plotly)
library(shinyalert)
library(tidyverse)
library(rsconnect)
library(googlesheets4)
library(magrittr)
library(rvest)
library(maps)

#for import dep/map
library(igoR)
library(countrycode)
library(giscoR)
library(rjson)


####Importing data####

#World map data old version
url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"

#setwd("C:/Users/BRICE/Dropbox (IFPRI)/FertilizerPageApp")

#p_fert <- read_excel('./Fertilizers all v2.xlsx')
#load(file="./p_fert.RData")
load(file="./imports.RData")
prices <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 8)
importdep <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 10)

#world map data (new version)
world <- gisco_get_countries(year = "2010") 

# reading in import dep. data
importdep <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 10)
importdep_world <- importdep %>%
  left_join(world,.) %>%
  select(NAME_ENGL, K2O, N, P2O5, geometry)%>%
  pivot_longer(cols = c(K2O, N, P2O5), 
               names_to = "nutrient",
               values_to = "value") %>%
  filter(NAME_ENGL!= "ANTARCTICA")

#general cleaning
prices <- prices %>%
  mutate(date=as.Date(date, "%Y-%m-%d"))

#product prices 
p_fert_long <- prices %>%
  filter(Product %in% c("Ammonia", "DAP", "MAP", "MOP",
                        "Phosphate Rock", "Urea"))

#nat.gas prices
p_natgas_long <- prices %>%
  filter(Product %in% c("Natural Gas - EU", "Natural Gas - USA"))

#nutrient data
p_nutrient_long <- prices %>%
  filter(Product %in% c("N","P", "K", "NPK Index"))


#######START OF UI######


ui <- fluidPage(
  navbarPage("Fertilizer Dashboard",
             tabPanel("Fertilizer Prices", fluid = TRUE,
                      # titlePanel("Fertilizer Dashboard"),
                      
                      column(12,tabsetPanel(id="prices_tabs",
                                            
                                            tabPanel("Prices by fertilizer type",
                                                     fluidRow(column(12,
                                                                     fluidRow(
                                                                       column(4,
                                                                              wellPanel(
                                                                                # p(strong("How-to")),
                                                                                h4("How-to"),
                                                                                helpText("Indicate the date range below or directly in the visualization by clicking and dragging the desired range."),
                                                                                # h4("Filter"),
                                                                                #  p(strong("Date")),
                                                                                #  helpText("Indicate the date range below or by clicking and dragging
                                                                                #           on the visualization"),
                                                                                sliderInput("Date", "Date range",
                                                                                            min = min(p_fert_long$date),
                                                                                            max = max(p_fert_long$date),
                                                                                            value = c(as.Date("2019-01-04", "%Y-%m-%d"), max(p_fert_long$date)),
                                                                                            timeFormat="%b %Y"
                                                                                            # ticks = FALSE
                                                                                ),
                                                                                
                                                                                # helpText("Use the sliders above to indicate the date range. 
                                                                                #          You can also control this and other features within the visualization
                                                                                #          itself."),
                                                                                
                                                                                
                                                                                # checkboxGroupInput(inputId = "Fert_selected",
                                                                                # p(strong("Select:")),
                                                                                #helpText("Select whether to display average prices per product."),
                                                                                
                                                                                
                                                                                selectInput(inputId = "Fert_selected",
                                                                                            label = "Select/remove fertilizers (click space below):",
                                                                                            choices =  c("Ammonia", "DAP",  "MAP",
                                                                                                         "MOP", "Phosphate Rock", "Urea"
                                                                                            ),
                                                                                            selected = c("Ammonia", "DAP",  "MAP",
                                                                                                         "MOP", "Phosphate Rock", "Urea"
                                                                                            ),
                                                                                            
                                                                                            
                                                                                            multiple = TRUE,
                                                                                            selectize =TRUE
                                                                                            #  width = "220px"
                                                                                            
                                                                                ),
                                                                                radioButtons("index", label = "Select what to show on y axis:",
                                                                                             choices = list("Actual prices" = 1, "Indexed values" = 2),
                                                                                             selected = 1
                                                                                )
                                                                                
                                                                              )
                                                                       ),
                                                                       column(8,
                                                                              #ggvisOutput("plot1")
                                                                              # plotOutput("plot1")
                                                                              plotlyOutput("plot1")
                                                                       ),
                                                                       # column(8,
                                                                       #        wellPanel("", 
                                                                       #                  includeHTML("FertilizerNarratives.html"))
                                                                       #        )
                                                                     )
                                                                     
                                                                     ### end of tab parenth
                                                     ))),
                                            
                                            ###########Tab on nutrient
                                            tabPanel("Prices by nutrient",
                                                     fluidRow(column(12,
                                                                     fluidRow(
                                                                       column(4,
                                                                              wellPanel(
                                                                                # p(strong("How-to")),
                                                                                h4("How-to"),
                                                                                helpText("Indicate the date range below or directly in the visualization."),
                                                                                # h4("Filter"),
                                                                                #  p(strong("Date")),
                                                                                #  helpText("Indicate the date range below or by clicking and dragging
                                                                                #           on the visualization"),
                                                                                sliderInput("Date2", "Date range",
                                                                                            min = min(p_nutrient_long$date),
                                                                                            max = max(p_nutrient_long$date),
                                                                                            value = c(as.Date("2019-01-04", "%Y-%m-%d"), max(p_nutrient_long$date)),
                                                                                            timeFormat="%b %Y"
                                                                                            # ticks = FALSE
                                                                                ),
                                                                                
                                                                                
                                                                                
                                                                                selectInput(inputId = "Nut_selected",
                                                                                            label = "Select Nutrients (click below):",
                                                                                            choices = c("K", "N",  "P", "NPK Index"
                                                                                            ),
                                                                                            selected = c("K", "N",  "P", "NPK Index"
                                                                                            ),
                                                                                            
                                                                                            multiple = TRUE,
                                                                                            selectize =TRUE
                                                                                            #  width = "220px"
                                                                                            
                                                                                )
                                                                                # radioButtons("index2", label = "Select what to show on y axis:",
                                                                                #              choices = list("Actual prices" = 1, "Indexed values" = 2),
                                                                                #              selected = 1
                                                                                # )
                                                                                
                                                                              )
                                                                       ),
                                                                       column(8,
                                                                              #ggvisOutput("plot1")
                                                                              # plotOutput("plot1")
                                                                              plotlyOutput("plot2")
                                                                       )
                                                                     )               
                                                                     
                                                     ))),# end of nutrient tab
                                            
                                            ###########Tab on narratives
                                            tabPanel("Monthly market developments",
                                                     fluidRow(column(12,
                                                                     fluidRow(
                                                                       column(12,
                                                                              wellPanel(
                                                                                includeHTML("FertilizerNarratives.html")
                                                                              )
                                                                       )
                                                                     ), 
                                                                     # fluidRow(
                                                                     #   column(12,
                                                                     #          plotOutput("plot1"))
                                                                     # )
                                                                     # column(6,
                                                                     #        #ggvisOutput("plot1")
                                                                     #        # plotOutput("plot1")
                                                                     #        plotlyOutput("plot3")
                                                                     # )
                                                                     
                                                                     
                                                     )))# end narrative tab
                                            
                      ))
                      
                      ###
             ),
             
             #########START OF INPUT COSTS TAB
             tabPanel("Input costs", fluid = TRUE,
                      ####Section - start of nat gas price tab
                      column(12,tabsetPanel(id="inputs_tabs",
                                            
                                            tabPanel("Natural gas prices",
                                                     fluidRow(column(12,
                                                                     
                                                                     
                                                                     fluidRow(
                                                                       column(4,
                                                                              wellPanel(
                                                                                # p(strong("How-to")),
                                                                                # h4("How-to"),
                                                                                helpText("Natural gas is a key input and fuel source for fertilizer production. This tab 
                             shows natural gas prices (just to show you how this works; need to connect to the same data feeding the current dashboard)."),
                                                                                
                                                                                sliderInput("Datenatgas", "Date range",
                                                                                            min = min(p_natgas_long$date),
                                                                                            max = max(p_natgas_long$date),
                                                                                            value = c(as.Date("2019-01-04", "%Y-%m-%d"), max(p_natgas_long$date)),
                                                                                            timeFormat="%b %Y"
                                                                                            # ticks = FALSE
                                                                                ),
                                                                                
                                                                                checkboxGroupInput(inputId = "Natgas_selected",
                                                                                                   label = "Select natural gas markets:",
                                                                                                   choices = c("Natural Gas - USA", "Natural Gas - EU"), 
                                                                                                   selected = c("Natural Gas - USA", "Natural Gas - EU")
                                                                                ),
                                                                                radioButtons("index_natgas", label = "Select what to show on y axis:",
                                                                                             choices = list("Actual prices" = 1, "Indexed values" = 2),
                                                                                             selected = 1
                                                                                )
                                                                              )
                                                                              
                                                                       ),
                                                                       column(8,
                                                                              plotlyOutput("plotnatgas")
                                                                       )
                                                                     )
                                                                     
                                                     ))), #end of nat gas prices tab
                                            
                                            
                                            ### Section - Start of nat gas compared to fert 
                                            tabPanel("Natural gas and fertilizer price relationship",
                                                     fluidRow(column(12,
                                                                     helpText("We could consider showing plotting natural gas prices and fertilizer prices together to show the relationship (using indexed values). 
                 Fertilizers shown could be the averages by fertilizer type.")
                                                                     
                                                     )))### Section - end of nat gas compared to fert
                                            
                      ))  #end of inputs tab
                      
                      
             ),  ##########END OF INPUT COSTS TAB
             
             
             ##############START OF MARKET CHARACTERISITCS TAB
             
             tabPanel("Market characteristics", fluid = TRUE,
                      fluidRow(
                        column(12,
                               wellPanel("", 
                                         includeHTML("FertilizerMarketCharacteristics.html"))
                        ) #end of column 12
                      ), #end of fluid row
                      column(12,tabsetPanel(id="MarketCharacteristics_tabs",
                                            
                                            tabPanel("Exporters and importers",
                                                     fluidRow(column(12,
                                                                     helpText("Note on design/structure: here we could include the current net importers tab, the import dependency ratio tab, and the main importers and exporters tab. 
                                                                          It seems it would make sense to consolidate at least the first and the third."),
                                                                     column(3,
                                                                            
                                                                            wellPanel(
                                                                              # p(strong("How-to")),
                                                                              # h4("How-to"),
                                                                              
                                                                              
                                                                              # checkboxGroupInput(inputId = "OnlyImporters",
                                                                              radioButtons(inputId = "OnlyImporters",
                                                                                           label = "Show:",
                                                                                           choices = list("Only net importers" = 2, "Only net exporters" = 3,"Both" = 1), 
                                                                                           selected = 1
                                                                              ),
                                                                              radioButtons(inputId = "DateImports", 
                                                                                           label = "Year:",
                                                                                           choices = list("2020" = 2020, "2010" = 2010),
                                                                                           selected = 2020
                                                                              ),
                                                                              
                                                                              
                                                                              radioButtons(inputId = "CountriesAllImports",
                                                                                           label = "Countries:",
                                                                                           choices = list("All countries" = 1, "Selection of countries" = 2), 
                                                                                           selected = 1
                                                                              ), 
                                                                            ) #end of well panel
                                                                            
                                                                     ),
                                                                     column(9,
                                                                            plotlyOutput("plotimports")
                                                                     )
                                                                     
                                                                     
                                                     )#end of column 12
                                                     )#end of fluidRow
                                            ),#end of tabPanel exp/imp,
                                            tabPanel("Import dependency",
                                                     fluidRow(
                                                       HTML('<br/>')
                                                     ),
                                                     fluidRow(column(12,
                                                                     column(3, 
                                                                            wellPanel(
                                                                              helpText("This tab shows the ratio
                                                                                       between imports and use."),
                                                                              radioButtons(inputId = "dep_nutrient",
                                                                                           label = "Select:",
                                                                                           choices = list("Nitrogenous" = 1, "Phosphate" = 3,"Potash" =2), 
                                                                                           selected = 1
                                                                              ),
                                                                              helpText("The ratio ranges from 0, no imports, to 1, 
                                                                                       domestic use fully supplied by imports.")
                                                                              
                                                                              
                                                                            ) #end of wellpanel
                                                                     ), #end of col3
                                                                     column(9,
                                                                            plotlyOutput("map_impdep")
                                                                            
                                                                     )
                                                                     
                                                                     
                                                     )
                                                     )#end of fluidRow
                                            ),#end of tabPanel import dependency
                                            
                                            tabPanel("Country profiles",
                                                     fluidRow(column(12,
                                                                     helpText("This would mirror the current version")               
                                                                     
                                                     )
                                                     )#end of fluidRow
                                            ),#end of tabPanel country profiles
                                            
                                            tabPanel("Fertilizer use",
                                                     fluidRow(column(12,
                                                                     helpText("Note on design: Here we could consider consolidating the current  Fertilizer Use and yields tab, Application rates tab, and the Production and Use tab. Choice 2 is to have these all as sub-tabs instead of trying to put all of the outputs on one sub-tab. Choice 3 is adding a Fertilizer Use top level tab with these under that."),
                                                                     
                                                                     
                                                                     
                                                     )
                                                     )#end of fluidRow
                                            ),#end of tabPanel fertilizer use
                                            
                                            
                                            
                                            tabPanel("Ukraine-Russia war exposure",
                                                     fluidRow(column(12,
                                                                     helpText("In addition to this tab, there is the
                                                                 tab on impact from trade restrictions on the 
                                                                 current version. We could either (1) add this as a sub tab to the right or (2) create
                                                                 a new high-level tab s or something like that, under which this tab and the
                                                                 one on restrictions would go.") ,              
                                                                     
                                                                     
                                                     )
                                                     )#end of fluidRow
                                            )#end of tabPanel Uk Russ
                                            
                                            
                                            
                                            
                                            
                                            
                      )) ###end of marketcharacteristics tabset panel (countaining subtabs)
                      
                      
                      
                      
             ),  ############END OF MARKET CHARACTERISTICS TAB
             
             #useShinyalert()
  )
)





server <- function(input, output) {
  
  
  shinyalert(
    title = "Welcome to the Fertilizer Dashboard",
    text = "Use the top bar of the dashboard to explore the tool's categories: fertilizer prices, input prices, and fertilizer market characteristics.",
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    type = "",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  # fertilizer_userselected <- reactive({
  #   p_fert_long %>%
  #     filter(
  #       Date >= min(input$Date),
  #       Date <= max(input$Date)
  #     )
  #   
  #   fertilizer_userselected <- as.data.frame(fertilizer_userselected) 
  
  
  
  
  # A reactive expression with the ggvis plot
  # vis <- reactive({
  
  
  #  output$plot1 <- renderPlot({
  #    p_fert_long %>%
  #      filter(name %in% input$Fert_selected) %>%
  #     ggplot(aes(x=Date, y=value, colour = name)) +
  #      geom_line(aes(name=name)) + 
  #      xlim(min(input$Date), max(input$Date))  
  #      #ggvis(x = Date, y = value)
  #     #might need to adjust the x=
  # })
  ####################THIS IS IF ACTUAL P SELECTED##############
  
  observeEvent(input$index,{
    if (input$index == 1) {
      
      output$plot1 <- renderPlotly({
        
        x <- p_fert_long %>%
          filter(Product %in% input$Fert_selected) %>%
          filter(variable == "PriceMT") %>%
          # filter(grepl('Average', name)) %>%
          ggplot(aes(x=date, y=value, colour = Product)) +
          geom_line(aes(name="Fertilizer")) +
          xlim(min(input$Date), max(input$Date)) +
          labs(x = "",y="Price/MT") 
        # theme(legend.position = "none")
        
        
        #theme(legend.position="none")
        #+
        # scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day",
        #          date_labels = "%Y %m %d")
        # +
        # scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
        
        l <- list(
          font = list(
            # family = "sans-serif",
            size = 10,
            color = "#000"
          )
        )
        #bgcolor = "#E2E2E2",bordercolor = "#FFFFFF",borderwidth = 2,
        # spacing = 1
        
        
        ggplotly(x, dynamicTicks = T) %>%
          layout(legend = l) %>%
          layout(legend=list(title=list(text='')))%>%
          layout(legend = list(orientation = 'h')) 
        
        
        
        # layout(list(x = 0.08, y = 0.97)
        #layout(showlegend = FALSE)
        #  layout(legend = "dfdfd")
        # 
        
        #legend(x, "lopleft")
        
        
        
        
        
        
      })
      
      
      
      
      # vis %>% bind_shiny("plot1")
      
      
      
      #end of observe event   
    }
  }) 
  
  
  ############THIS IS IF SHOWING INDEXED VALUES FOR FERT P###################
  
  observeEvent(input$index,{
    if (input$index == 2) {
      
      
      
      output$plot1 <- renderPlotly({
        
        x <- p_fert_long %>%
          filter(Product %in% input$Fert_selected) %>%
          filter(variable=="Index") %>%
          ggplot(aes(x=date, y=value, colour = Product)) +
          geom_line(aes(name="Fertilizer")) +
          xlim(min(input$Date), max(input$Date)) +
          labs(x = "",y="Price (index)") 
        # theme(legend.position = "none")
        
        
        #theme(legend.position="none")
        #+
        # scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day",
        #          date_labels = "%Y %m %d")
        # +
        # scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
        
        l <- list(
          font = list(
            # family = "sans-serif",
            size = 10,
            color = "#000"
          )
        )
        #bgcolor = "#E2E2E2",bordercolor = "#FFFFFF",borderwidth = 2,
        # spacing = 1
        
        
        ggplotly(x, dynamicTicks = T) %>%
          layout(legend = l) %>%
          layout(legend=list(title=list(text='')))%>%
          layout(legend = list(orientation = 'h'))
        
        # layout(list(x = 0.08, y = 0.97)
        #layout(showlegend = FALSE)
        #  layout(legend = "dfdfd")
        # 
        
        #legend(x, "lopleft")
        
        
        
        
        
        
      })
      
      
      
      
      # vis %>% bind_shiny("plot1")
      
      
      
      #end of observe event   
    }
  }) 
  
  ############Nutrient - indexed/not indexed##########
  
  
  
  output$plot2 <- renderPlotly({
    
    x_nutrient <- p_nutrient_long %>%
      filter(Product %in% input$Nut_selected) %>%
      #filter(variable == "PriceMT") %>%
      ggplot(aes(x=date, y=value, colour = Product)) +
      geom_line(aes(name="Nutrient")) +
      xlim(min(input$Date2), max(input$Date2)) +
      labs(x = "",y="Price (indexed values)") 
    
    l <- list(
      font = list(
        # family = "sans-serif",
        size = 10,
        color = "#000"
      )
    )
    ggplotly(x_nutrient, dynamicTicks = T) %>%
      layout(legend = l) %>%
      layout(legend=list(title=list(text='')))%>%
      layout(legend = list(orientation = 'h')) 
  })   #ENd of render plotly
  
  ##########SECTION: Beginning of nat gas tab - not index##########
  observeEvent(input$index_natgas,{
    if (input$index_natgas == 1) {
      
      
      output$plotnatgas <- renderPlotly({
        
        x_natgas <- p_natgas_long %>%
          filter(Product %in% input$Natgas_selected) %>%
          filter(variable=="USDpermmBTU") %>%
          ggplot(aes(x=date, y=value, colour = Product)) +
          geom_line(aes(name="Natural_Gas_Prices")) +
          xlim(min(input$Datenatgas), max(input$Datenatgas)) +
          labs(x = "",y="Price") 
        
        l <- list(
          font = list(
            # family = "sans-serif",
            size = 10,
            color = "#000"
          )
        )
        
        ggplotly(x_natgas, dynamicTicks = T) %>%
          layout(legend = l) %>%
          layout(legend=list(title=list(text='')))%>%
          layout(legend = list(orientation = 'h'))
        
      })
      
    } # end of nat gas index =1
    
    
    #natural gas = index
    if (input$index_natgas == 2) {
      
      
      output$plotnatgas <- renderPlotly({
        
        x_natgas <- p_natgas_long %>%
          filter(Product == input$Natgas_selected) %>%
          filter(variable=="Index") %>%
          ggplot(aes(x=date, y=value, colour = Product)) +
          geom_line(aes(name="Natural_Gas_Prices")) +
          xlim(min(input$Datenatgas), max(input$Datenatgas)) +
          labs(x = "",y="Price (index)") 
        
        l <- list(
          font = list(
            # family = "sans-serif",
            size = 10,
            color = "#000"
          )
        )
        
        ggplotly(x_natgas, dynamicTicks = T) %>%
          layout(legend = l) %>%
          layout(legend=list(title=list(text='')))%>%
          layout(legend = list(orientation = 'h'))
        
      })
      
    } # end of nat gas index =2
    
  }) #end of observe event
  #############ENd of nat gas tab
  
  
  ##########SECTION: Beginning of net imports
  observeEvent(input$OnlyImporters,{
    if (input$OnlyImporters == 1){
      x_imports <- imports    }
    
    if (input$OnlyImporters == 2){
      x_imports <- imports %>%
        filter(`Net`=="Net importer")
    }
    
    if (input$OnlyImporters == 3){
      x_imports <- imports %>%
        filter(`Net`=="Net exporter")
    }    
    
    
    
    
    
    output$plotimports <- renderPlotly({
      
      x_imports <- x_imports %>%
        ggplot(aes(x=`Country`, y=`Imports`, label=`Net`)) + 
        geom_bar(stat='identity', aes(fill=`Net`), width=.1)  +
        scale_fill_manual(name="", 
                          labels = c("Net exporter", "Net importer"), 
                          values = c("Net exporter"="#00ba38", "Net importer"="#f8766d")) + 
        labs(x = "",y="Amount in tons") +
        facet_grid(.~`Nutrient`,scales="free")+
        # geom_text(aes(label = round(`Imports`, 1),
        #           size = 1)
        #           )+
        
        #labs(subtitle="Normalised mileage from 'mtcars'", 
        #       title= "Diverging Bars") + 
        coord_flip() 
      
      
      ggplotly(x_imports) #%>%
      # layout(legend = l) %>%
      # layout(legend=list(title=list(text='')))%>%
      # layout(legend = list(orientation = 'h'))
      
    }) #end of renderplotly
    
    
    
  })#end of observe 
  
  #############ENd of net imports
  
  
  
  #######START of import dep map##############
  
  observeEvent(input$dep_nutrient,{
    # importdep_world <- importdep_world %>%
    #   filter(nutrient == input$dep_nutrient)
    
    
    if (input$dep_nutrient == 1){
      importdep_world <- importdep_world %>%
        filter(`nutrient` == "N")
    }
    if (input$dep_nutrient == 2){
      importdep_world <- importdep_world %>%
        filter(`nutrient` == "K2O")
    }
    if (input$dep_nutrient == 3){
      importdep_world <- importdep_world %>%
        filter(`nutrient` == "P2O5")
    }
    
    output$map_impdep <- renderPlotly({
      
      importdep_world <- importdep_world %>%
        ggplot() +
        geom_sf(aes(fill = value), color = NA, show.legend = TRUE) +
        #  scale_fill_viridis_b()+
        coord_sf(crs = "ESRI:54030", xlim = c(-11000804, 12909125), ylim = c(-2485711, 6071856), expand = FALSE) +
        #coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE) +    
        # facet_wrap(vars(year),
        #            ncol = 1,
        #            strip.position = "left"
        # ) +
        #scale_fill_manual(
        #  values = "#74A9CF",
        #   na.value = "#E0E0E0"
        # ) +
        #theme_void() +
        labs(
          title = "Import DEP",
          caption = gisco_attributions()
        ) +
        theme(plot.caption = element_text(face = "italic"),
              #panel.background = element_blank(),
              plot.background = element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank()
        )
      # guides(fill = guide_legend(title = "Unit: 1000", title.position = "bottom", title.theme =element_text(size = 10, face = "bold",colour = "gray70",angle = 0)))
      
      # ggplotly(importdep_world) 
      #  importdep_world <- plot_ly()
      # importdep_world <- importdep_world %>% add_trace(
      #   type="choropleth",
      #  # geojson=geojson
      #   locations=importdep_world$geometry,
      #   z=importdep_world$value
      #  # colorscale="Viridis",
      # #  featureidkey="properties.district"
      # )
      # # importdep_world <- importdep_world %>% layout(
      # #   geo = g
      # # )
      # importdep_world <- importdep_world %>% colorbar(title = "Bergeron Votes")
      # importdep_world <- importdep_world %>% layout(
      #   title = "2013 Montreal Election"
      # )
      
    })  #end of observe event
    
  })
  
  ### end of imp dep map
  
  
} # end of server   


# Run the application 
shinyApp(ui = ui, server = server)