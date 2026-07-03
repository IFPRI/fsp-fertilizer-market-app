library(shiny)
library(readxl)
library(dplyr)
library(plotly)
library(ggplot2)
library(datawizard)
library(DT)
library(reactable)

# setwd("C:\\Users\\DKengneKuate\\Dropbox (IFPRI)\\Darline_countryPages")
# getwd()

options(scipen = 999)    #removes exponential notation

NationalUse <- read_excel("Fertilizer Information desk.xlsx", 
                          sheet = "NationalUse")

NationalUse <- NationalUse %>%
  filter (variable != "DependencyRatio")

#User interface
ui=shinyUI(
  navbarPage(id="fsp","Fertilizer Dashboard",
             tabPanel("Country Profile",
  fluidRow(selectInput("country_dropdown",label ="Select a country:", choices=setdiff(c("All",NationalUse$Country_Name %>% unique()),"World"),multiple=F,width = 800), 
                  column(7, h3("National balance in Metric tons"), plotlyOutput("NationalBalance")), 
                  ),
  
  fluidRow(column(7, h3("Rank, among all countries"), reactable::reactableOutput("Rank")))
  ))
  
)



# Define server function
server <- shinyServer(function(input, output, session) {
  # observeEvent(input$country_dropdown,{
  #   x <- NationalUse %>% dplyr::filter(Country_Name==isolate(input$country_dropdown)) %>% dplyr::select(Country_Name) %>% unique()
  #   x <- c("All", x$Country_Name)
  #   updateSelectInput(session, inputId="dropdown", choices=x)
  # })
  observeEvent(input$country_dropdown,{
    country_dropdown_df<-reactive({
      if(input$country_dropdown=="All"){
        df<-NationalUse %>% dplyr::filter(Country_Name=="World")
      }
      else(
        df <- NationalUse %>% dplyr::filter(Country_Name==input$country_dropdown)) 
      })
    
    output$NationalBalance <- renderPlotly({
      df<-country_dropdown_df()
      ggplot(df) +
        aes(x = variable, y = value) +
        geom_col(aes(fill = variable)) +
        facet_grid(Year~Nutrient, scales = "free_y", switch = "y") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        #theme(axis.text.y = element_text(angle = 0, hjust = 0))+
        theme(legend.position = "none")+
        #theme(axis.line.y.left =  TRUE)+
        xlab("") +
        scale_y_continuous(name = "Metric Tons, Nutrient")
        tickformat = ".3s"
        #scale_y_continuous(name = "Metric Tons, Nutrient",
        #                   breaks = c(-200000000, 0, 200000000), position = 'left',
        #                   labels = c('-20M', '0M', '20M'))
      
    })
    output$Rank <- reactable::renderReactable({
      orange_pal <- function(x) rgb(colorRamp(c("red", "yellow"))(x), maxColorValue = 255)
      df <- reshape_wider(
        country_dropdown_df(), id_cols = c("Country_Name", "Nutrient", "variable"), 
        names_from = "Year", values_from = "Ranks")
      #datatable(df)
      reactable(df %>% select(-1), 
                groupBy = "Nutrient", paginateSubRows = T, bordered = T, defaultPageSize = 25,
                columns = list(
                  "2000" = colDef(
                    width = 60,
                    style = function(value) {
                      normalized <- (value - min(df[,4])) / (max(df[,4]) - min(df[,4]))
                      color <- orange_pal(normalized)
                      list(background = color)
                    }
                  ),
                  "2010" = colDef(
                    width = 60,
                    style = function(value) {
                      normalized <- (value - min(df[,5])) / (max(df[,5]) - min(df[,5]))
                      color <- orange_pal(normalized)
                      list(background = color)
                    }
                  ),
                  "2020" = colDef(
                    width = 60,
                    style = function(value) {
                      normalized <- (value - min(df[,6])) / (max(df[,6]) - min(df[,6]))
                      color <- orange_pal(normalized)
                      list(background = color)
                    }
                  )
                )
                )
    })

})
})

# Run the application 
shinyApp(ui = ui, server = server)