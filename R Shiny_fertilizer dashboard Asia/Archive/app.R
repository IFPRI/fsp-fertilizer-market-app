
#app before 13Jan22
#loading packages
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


library(shiny)
library(lubridate)
library(ggvis)
library(dplyr)
library(plotly)
library(shinyalert)
library(tidyverse)
library(rsconnect)
library(googlesheets4)

#setwd("C:/Users/BRICE/Dropbox (IFPRI)/FertilizerPageApp")

#p_fert <- read_excel('./Fertilizers all v2.xlsx')
load(file="./p_fert.RData")
load(file="./imports.RData")
prices <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 8)

t1 <- prices %>%
  tabyl(eye_color)

p_fert <- p_fert %>%
  rowwise()%>%
  mutate("Urea Average" = mean(c(`Urea Gulf NOLA (gran)`, `Urea Mediterranean`), na.rm = TRUE),
         "DAP Average" = mean(c(`DAP US Gulf Nola`,`DAP Baltic`), na.rm = TRUE),
         "Ammonia Average" = mean(c(`Ammonia Middle East`, `Ammonia Western Europe`, `Ammonia US Gulf NOLA`), na.rm = TRUE),
         "Potash Average" = mean(c(`Potash Baltic Standard`, `Potash Gulf NOLA` ), na.rm = TRUE)
  )


p_fert_long <- pivot_longer(p_fert, cols = 2:21)

p_fert_long <-p_fert_long %>%
  mutate(Date=as.Date(Date, "%Y-%m-%d"),
         value=as.numeric(value))%>%
  filter(name!="Natural Gas BNGC")

#nat.gas prices

p_natgas_long <- pivot_longer(p_fert, cols = 2:21)

p_natgas_long <-p_natgas_long %>%
  mutate(Date=as.Date(Date, "%Y-%m-%d"),
         value=as.numeric(value))%>%
  filter(name=="Natural Gas BNGC")

##setting up fake by nutrient data
p_nutrient <- p_fert %>%
  select(c(`Date`, `Ammonia US Gulf NOLA`, `Potash Gulf NOLA`, `DAP US Gulf Nola`)) %>%
  rename("Nitrogen" = "Ammonia US Gulf NOLA",
         "Potassium" = "Potash Gulf NOLA",
         "Phospate" = "DAP US Gulf Nola"
         )

p_nutrient_long <- pivot_longer(p_nutrient, cols = 2:4)

p_nutrient_long <-p_nutrient_long %>%
  mutate(Date=as.Date(Date, "%Y-%m-%d"),
         value=as.numeric(value))

###



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
                         min = min(p_fert_long$Date),
                         max = max(p_fert_long$Date),
                         value = c(as.Date("2017-01-01", "%Y-%m-%d"), max(p_fert_long$Date)),
                         timeFormat="%b %Y"
                        # ticks = FALSE
             ),
           
           # helpText("Use the sliders above to indicate the date range. 
           #          You can also control this and other features within the visualization
           #          itself."),
           
             
             #another way
             # dateRangeInput(inputId = "Date", "Date",
             #                start = "2020-01-01",
             #                end = max(p_fert_long$Date),
             #                min = min(p_fert_long$Date),   
             #                max = max(p_fert_long$Date),
             #                format = "%Y-%m-%d",
             #                startview = "month",
             #                weekstart = 0
             # ),
           # checkboxGroupInput(inputId = "Fert_selected",
          # p(strong("Select:")),
           #helpText("Select whether to display average prices per product."),
           radioButtons("avg", label = "Select:",
                        choices = list("Average prices" = 1, "Individual fertilizer prices" = 2),
                                       selected = 1
                        ),
           
           selectInput(inputId = "Fert_selected",
                       label = "Select/remove fertilizers (click space below):",
                       choices = list(`N fertilizers` = list("Ammonia Middle East", "Ammonia US Gulf NOLA",  "Ammonia Western Europe",
                                                           "Urea Gulf NOLA (gran)",  "Urea Gulf Prill"),
                                      `P fertilizers` = list ("DAP Baltic", "DAP US Gulf NOLA" = "DAP US Gulf Nola"),
                                      `K fertilizers` =  list ("Potash Baltic Standard", "Potash Gulf NOLA")
                                      ),
                        selected = c("Ammonia Middle East", "Ammonia US Gulf NOLA",  "Ammonia Western Europe",
                                     "Urea Gulf NOLA (gran)",  "Urea Gulf Prill",
                                     "DAP Baltic", "DAP US Gulf NOLA" = "DAP US Gulf Nola",
                                     "Potash Baltic Standard", "Potash Gulf NOLA"
                                     ),

                       # choices = c("Ammonia Black Sea", "Ammonia Middle East", "Ammonia US Gulf NOLA",  "Ammonia Western Europe",  "DAP Baltic", "DAP US Gulf NOLA" = "DAP US Gulf Nola",
                       #                     "Potash Baltic Standard", "Potash Gulf NOLA", "Urea Black Sea Prill", "Urea Gulf NOLA (gran)",  "Urea Gulf Prill",
                       #                     "Urea Mediterranean", "Urea Mediterranean CFR Pril",  "Urea Middle East Gran", "Urea Middle East Prill" 
                       #                   )
                      # selected = c("Ammonia Middle East","Ammonia Black Sea", "Ammonia Middle East", "Ammonia US Gulf NOLA")
                                    #,  "Ammonia Western Europe",  "DAP Baltic", "DAP US Gulf NOLA" = "DAP US Gulf Nola",
                          #     "Potash Baltic Standard", "Potash Gulf NOLA", "Urea Black Sea Prill", "Urea Gulf NOLA (gran)",  "Urea Gulf Prill"
                               #,"Urea Mediterranean", "Urea Mediterranean CFR Pril"
                        
           
                       multiple = TRUE,
                      selectize =TRUE
                     #  width = "220px"
                       
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
                                                 min = min(p_nutrient_long$Date),
                                                 max = max(p_nutrient_long$Date),
                                                 value = c(as.Date("2017-01-01", "%Y-%m-%d"), max(p_nutrient_long$Date)),
                                                 timeFormat="%b %Y"
                                                 # ticks = FALSE
                                     ),
                                     
                                     
                                     
                                     selectInput(inputId = "Nut_selected",
                                                 label = "Select Nutrients (click below):",
                                                 choices = c("Nitrogen", "Phosphate",  "Potassium"
                                                                ),
                                                 selected = c("Nitrogen", "Phosphate",  "Potassium"
                                                 ),
                                                 
                                                 multiple = TRUE,
                                                 selectize =TRUE
                                                 #  width = "220px"
                                                 
                                     )
                                     
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
                                min = min(p_natgas_long$Date),
                                max = max(p_natgas_long$Date),
                                value = c(as.Date("2017-01-01", "%Y-%m-%d"), max(p_natgas_long$Date)),
                                timeFormat="%b %Y"
                                # ticks = FALSE
                    ),
                 
                    radioButtons(inputId = "Natgas_selected",
                                label = "Select natural gas markets:",
                                choices = list("Natural gas - USA" = 1, "Natural gas - EU" = 2), 
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
                                        fluidRow(column(12,
                                                        helpText("See previous sub-tab note")               
                                                        
                                                        
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
  ####################THIS IS IF AVG SELECTED##############
  
  observeEvent(input$avg,{
    if (input$avg == 1) {
      
      output$plot1 <- renderPlotly({
        
        x <- p_fert_long %>%
          filter(grepl('Average', name)) %>%
          ggplot(aes(x=Date, y=value, colour = name)) +
          geom_line(aes(name="Fertilizer")) +
          xlim(min(input$Date), max(input$Date)) +
          labs(x = "",y="Price") 
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
  
  
  ############THIS IS IF AVG NOT SELECTED####################
  
  observeEvent(input$avg,{
    if (input$avg == 2) {
     
    
  
   output$plot1 <- renderPlotly({
    
       x <- p_fert_long %>%
       filter(name %in% input$Fert_selected) %>%
      ggplot(aes(x=Date, y=value, colour = name)) +
       geom_line(aes(name="Fertilizer")) +
       xlim(min(input$Date), max(input$Date)) +
         labs(x = "",y="Price") 
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
    
 ############Tab 2 
  output$plot2 <- renderPlotly({
    
    x_nutrient <- p_nutrient_long %>%
      ggplot(aes(x=Date, y=value, colour = name)) +
      geom_line(aes(name="Nutrient")) +
      xlim(min(input$Date2), max(input$Date2)) +
      labs(x = "",y="Price") 

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
  })
  
  
  ########END TAB 2
  
  ##########SECTION: Beginning of nat gas tab
  output$plotnatgas <- renderPlotly({
    
    x_natgas <- p_natgas_long %>%
      #filter(name %in% input$natgas_selected) %>%
      ggplot(aes(x=Date, y=value, colour = name)) +
      geom_line(aes(name="Natural Gas Prices")) +
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
  
    
} # end of server   
  

# Run the application 
shinyApp(ui = ui, server = server)