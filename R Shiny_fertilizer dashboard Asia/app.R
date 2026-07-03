# FERTILIZER DASHBOARD FOR ASIAN PORTAL
# Last update: Dec 16, 2024

library(shiny)
library(lubridate)
library(ggvis)
# library(dplyr) 
library(plotly)
library(shinyalert)
library(tidyverse)
library(rsconnect)
#library(googlesheets4)
library(magrittr)
library(rvest)
library(maps)
library(dplyr)

#for import dep/map
library(igoR)
library(countrycode)
library(giscoR)
library(rjson)

#for bubbles
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

#other
library(shinyBS)
library(shinycssloaders)

#color
library(colorspace)

library(shinydashboard)

# connect to the database



ADBCountries <- readRDS("data/ADBcountries.rds")
asia_pacific_iso3<- ADBCountries

####Importing data####

options(scipen = 999)    #removes exponential notation




#World map data old version
#url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"

today <- Sys.Date()

#p_fert <- read_excel('./Fertilizers all v2.xlsx')
#load(file="./p_fert.RData")
#world map data (new version)
#world <- gisco_get_countries(year = "2016")
load(file="data/countryShapeFiles.RData")

world = sf::st_cast(world, "MULTIPOLYGON")

world_forDepRatio <- world %>%
  dplyr::rename(Country_Code= ISO3_CODE)


prices <- readRDS("data/prices.rds")

fertilizer.use <- readRDS("data/FertilizerUse.rds") %>%
  filter(Country_Code %in% asia_pacific_iso3)
usebroad <- fertilizer.use %>%
  filter(Country_Code %in% asia_pacific_iso3)

application <- readRDS("data/application.rds") %>%
  filter(Country_Code %in% asia_pacific_iso3)

#### Price ####
#fert and natGasPrices from WB pink sheets
WB_pricesImported <-readRDS("data/WBPinkSheetFertilizer_clean.rds")
WB_fertilizer_prices <- WB_pricesImported %>%
  filter(Commodity %in% c(#"Phosphate rock", 
    "DAP", "TSP", 
    "Urea", "Potassium chloride")) %>%
  filter(!str_detect(Unit, "Index"))  
WB_NatGas_prices <- WB_pricesImported %>%
  filter(Commodity %in% c("Natural gas, US", "Natural gas, Europe")) %>%
  filter(!str_detect(Unit, "Index"))  

# indexed version of WB data
WB_pricesIndexed <- WB_pricesImported %>%
  filter(str_detect(Unit, "Index"))  %>%
  filter(!Commodity %in% c("Crude oil, average", "Natural gas index","Phosphate rock"  ))

nutrients <- c("Nitrogenous", "Phosphate", "Potash")

# reading in import dep. data
#importdep <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 10)

importdep_world <- fertilizer.use %>%
  filter(variable == "DependencyRatio") 
  # left_join(world_forDepRatio,.) %>%
  # select(NAME_ENGL, K2O, N, P2O5, geometry)%>%
  # pivot_longer(cols = c(K2O, N, P2O5), 
  #              names_to = "nutrient",
  #              values_to = "value") %>%
  #filter(NAME_ENGL!= "ANTARCTICA") 

#general cleaning
prices <- prices %>%
  mutate(date=as.Date(date, "%Y-%m-%d")) 
# %>%
#   mutate(Product = replace(Product, Product ==  "Natural Gas - USD", "Natural Gas - EU"))

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

#application rate
#products <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 7, col_types = "?????inc??" )
`%notin%` <- Negate(`%in%`)


appRate <- application %>%
  filter(variable == "Application rate") %>%
  filter(Country_Name  %notin% c("World","European Union")) %>%
 # select(-8) %>%
  dplyr::rename(ISO3_CODE= "Country_Code") 


#### inputChoices based on data avail. ####
yearBasedOnDataAvail <- usebroad %>% count(Year) %>% arrange(desc(Year)) %>% pull(Year)
yearBasedOnUseDataAvail <- usebroad %>% filter(variable == "Agricultural Use") %>% count(Year) %>% arrange(desc(Year)) %>% pull(Year)
yearBasedOnUseDataAvailYields <- c(#"All years", 
  yearBasedOnUseDataAvail)

yearBasedOnPRoductionDataAvail <- usebroad %>% filter(variable == "Production") %>% count(Year) %>% arrange(desc(Year)) %>% pull(Year)
yearBasedOnImpDependencyDataAvail <- importdep_world %>% count(Year) %>% arrange(desc(Year)) %>% pull(Year)



 # select(7,2,9,3,5,1,6)
# 
# appRateWorld <- appRate %>%
#   left_join(world,.)


#productionMap

production_tobefiltered <- usebroad %>%
  filter(variable == "Production") %>%
  # filter(Product =="Potash") %>%
  #filter(Year==2020)%>% 
  filter(Country_Name != "World")%>% 
  filter(Country_Name != "European Union") %>%
  # filter(value !=0)%>%
  # mutate(Country_Code_Big = case_when(ranks<10~Country_Code)) %>%
  dplyr::rename(ISO3_CODE= "Country_Code") 

`%notin%` <- Negate(`%in%`)
productionWorld <- production_tobefiltered %>%
  left_join(world,.) %>%
 # filter(Country_Name  %notin% c("World","European Union")) %>%
  select(-1,-2,-5,-14,-15) 
  #rename(Country_Name = "NAME_ENGL")

# appRate <- appRate %>%
#   left_join(world,.) %>%
#   select(-1,-2,-4,-5)
# filter(NAME_ENGL!= "ANTARCTICA") 

#imp/exp quantity
# impQuantity <- usebroad %>%
#   filter(variable=="Import Quantity")
# 
# expQuantity <- usebroad %>%
#   filter(variable=="Export Quantity")

# #fertilizer use
# use <- usebroad %>%
#   filter(variable == "Agricultural Use")

# use <- usebroad %>%
#   filter(`variable` == "Agricultural Use") %>%
#      filter(`Product` == "Potash") %>%
#     filter(`Year`==2020)%>%
#   #filter(`Product` == input$useNutrient) %>%
#   #filter(`Year` == input$useYear)%>%
#   # filter(`Product` == input$useNutrient) %>%
#   #  filter(`Year`==input$useYear)%>%
#   filter(`Country_Name` != "World")%>%
#   filter(`Country_Name` != "European Union") %>%
#   filter(value !=0)%>%
#   mutate(Country_Code_Big = case_when(ranks<10~Country_Code))
# 
# packing <- circleProgressiveLayout(use$value, sizetype='area')
# use_2 <- cbind(use, packing)%>%
#   mutate(text = paste(Country_Name, "\n", value) )
# dat.gg <- circleLayoutVertices(packing, npoints=50)
# 
# plotBubble<- function() {
#   ggplot() +
#     geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id,
#                                                 tooltip = use_2$text[id],
#                                                 data_id = id), colour = "black", alpha = 0.6) +
#     scale_fill_viridis() +
#     geom_text(data = use_2, aes(x, y, label = Country_Code_Big), size=4, color="black") +
#     theme_void() +
#     theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") )
# }

#########Colors#######
# scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  

scale_color_manual_fertizerCategories <- function(...){
  ggplot2:::scale_color_manual(
    ...,
    values = setNames(c('firebrick2', 'darkolivegreen4', 'deepskyblue2'), c('Nitrogenous', 'Phosphate', 'Potash')),
    ...
  )
}


scale_color_manual_fertizerProducts<- function(...){
  ggplot2:::scale_color_manual(
    ...,
    values = setNames(c('darkred', 'darkseagreen3', 'dodgerblue', 'darkkhaki', 'springgreen3', 'darksalmon'), c('Ammonia', 'DAP', 'MOP', 'MAP', 'Phosphate Rock', 'Urea')),
    ...
  )
}

scale_color_manual_fertizerNutrientCategories <- function(...){
  ggplot2:::scale_color_manual(
    ...,
    values = setNames(c('firebrick2', 'darkolivegreen4', 'deepskyblue2', 'sienna1'), c('N', 'P', 'K', 'NPK Index')),
    ...
  )
}

scale_color_manual_natGas <- function(...){
  ggplot2:::scale_color_manual(
    ...,
    values = setNames(c('salmon2', 'seagreen3'), c('Natural Gas - EU', 'Natural Gas - USA')),
    ...
  )
}


scale_color_manual_natGasFertilizer <- function(...){
  ggplot2:::scale_color_manual(
    ...,
    values = setNames(c('salmon2', 'seagreen3','firebrick2', 'darkolivegreen4', 'deepskyblue2', 'lightgoldenrod2'), c('Natural Gas - EU', 'Natural Gas - USA','N', 'P', 'K', 'NPK Index')),
    ...
  )
}


#######START OF UI######


ui <- fluidPage(
  
  titlePanel(title=div(img(src="Logo_FSP_RGB_Asia.png", height = 50), "Fertilizer Market Dashboard - Asia and the Pacific"), windowTitle = "Fertilizer Dashboard"), #,align = "center")
  
  navbarPage(
  #  title=div(img(src="Logo_FSP_EC_RGB - Copy.png", height = 35), 
              ""
              #),
              ,
              selected = "Trade",  # Set "Trade" as the default selected tab
              
             # tabPanel("Global Prices", fluid = TRUE,
             #          # titlePanel("Fertilizer Dashboard"),
             #          
             #          column(12,tabsetPanel(id="prices_tabs",
             #                                
             #                                tabPanel("Prices by fertilizer type",
             #                                         
             #                                         fluidRow(column(12,
             #                                                         HTML('<br/>'),
             # 
             #                                                         fluidRow(
             #                                                           column(4,
             #                                                                  wellPanel(
             #                                                                   # p(strong("How-to")),
             #                                                                    fluidRow(align="center",
             #                                                                    actionButton("howToTabPrices", label = "How to use this tab", 
             #                                                                             #    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
             #                                                                                 )
             #                                                                    ),
             #                                                                    hr(),
             #                                                                    
             #                                                                   # HTML('<br/>'),
             #                                                                    # h4("How-to"),
             #                                                                    # helpText("Indicate the date range below or directly in the visualization by clicking and dragging the desired range."),
             #                                                                    # h4("Filter"),
             #                                                                    #  p(strong("Date")),
             #                                                                    #  helpText("Indicate the date range below or by clicking and dragging
             #                                                                    #           on the visualization"),
             #                                                                    # sliderInput("Date", "Date range",
             #                                                                    #             min = min(p_fert_long$date),
             #                                                                    #             max = max(p_fert_long$date),
             #                                                                    #             value = c(as.Date("2019-01-04", "%Y-%m-%d"), max(p_fert_long$date)),
             #                                                                    #             timeFormat="%b %Y"
             #                                                                    #             # ticks = FALSE
             #                                                                    # ),
             #                                                                   sliderInput("Date_FertPrices", "Date range",
             #                                                                               min = min(WB_fertilizer_prices$Date),
             #                                                                               max = max(WB_fertilizer_prices$Date),
             #                                                                               value = c(as.Date("2020-01-01", "%Y-%m-%d"), max(WB_fertilizer_prices$Date)),
             #                                                                               timeFormat="%b %Y"
             #                                                                               # ticks = FALSE
             #                                                                   ),
             #                                                                   selectInput(inputId = "Fert_selected",
             #                                                                               label = tags$span("Select fertilizer type:",shinyBS::bsButton("productdescriptions", label = "", icon = icon("info"), style = "info", size = "small") ),
             #                                                                               choices = unique(WB_fertilizer_prices$Commodity) , # Extract distinct values from Commodity column
             #                                                                               selected = unique(WB_fertilizer_prices$Commodity),
             #                                                                               multiple = TRUE,
             #                                                                               selectize =TRUE
             #                                                                   ),
             #                                                                   bsPopover(
             #                                                                     id = "productdescriptions",
             #                                                                     title = "Description of fertilizer products",
             #                                                                     content = paste0(
             #                                                                       
             #                                                                       "<b>DAP (diammonium phosphate)</b> : spot, f.o.b. US Gulf<br>",
             #                                                                       "<b>Urea</b> Urea (Ukraine): prill spot f.o.b. Middle East (from March 2022); previously, f.o.b. Black Sea. <br>",
             #                                                                       "<b>TSP (triple superphosphate)</b>: spot, import US Gulf <br>",
             #                                                                       "<b>Potassium chloride (muriate of potash)</b>: Brazil CFR granular spot price (from January 2020); previously, f.o.b. Vancouver"
             #                                                                     ) ,
             #                                                                     placement = "right",
             #                                                                     trigger = "hover",
             #                                                                     options = list(container = "body")
             #                                                                   ),
             #                                                                   #hr(),
             #                                                                   
             #                                                                    
             #                                                                    # helpText("Use the sliders above to indicate the date range. 
             #                                                                    #          You can also control this and other features within the visualization
             #                                                                    #          itself."),
             #                                                                    
             #                                                            
             #                                                                    # checkboxGroupInput(inputId = "Fert_selected",
             #                                                                    # p(strong("Select:")),
             #                                                                    #helpText("Select whether to display average prices per product."),
             #                                                                    
             #                                                                    #info popup here: https://stackoverflow.com/questions/70679604/info-icon-next-to-label-of-a-selectinput-in-shiny
             #                                                                    #https://stackoverflow.com/questions/16449252/tooltip-on-shiny-r
             #                                                               
             #                                                                   tags$div(title=paste0(
             #                                                                         "By default, all fertilizer types are",
             #                                                                         " selected. To remove fertilizers, click",
             #                                                                          " the fertilizer and press delete on",
             #                                                                          " your keyboard. That dropped fertilizer",
             #                                                                          " will then appear in a dropdown list",
             #                                                                          " for reselection.",
             #                                                                          " Alternatively, click the",
             #                                                                          " fertilizer name directly in the ",
             #                                                                          " visualization legend. Double click to",
             #                                                                          " show only that fertilizer. Single click",
             #                                                                          " to drop that fertilizer from the plot",
             #                                                                          " or to add it back if it was previously",
             #                                                                          " dropped.") ,
             #                                                                            
             #                                                                    # selectInput(inputId = "Fert_selected",
             #                                                                    #             label = tags$span("Select fertilizer type:",bsButton("productdescriptions", label = "", icon = icon("info"), style = "info", size = "extra-small") ),
             #                                                                    #                                                   
             #                                                                    #             choices =  c("Ammonia", "DAP",  "MAP",
             #                                                                    #                             "MOP", "Phosphate Rock", "Urea"
             #                                                                    #                         ),
             #                                                                    #             selected = c("Ammonia", "DAP",  "MAP",
             #                                                                    #                            "MOP", "Phosphate Rock", "Urea"
             #                                                                    #                         ),
             #                                                                    #             
             #                                                                    #         
             #                                                                    #             multiple = TRUE,
             #                                                                    #             selectize =TRUE
             #                                                                    #             #  width = "220px"
             #                                                                    #             
             #                                                                    # )#end of slider input
             #                                                                                
             #                                                                    ), #end of div
             #                                                                   
             #                                                                   checkboxInput(inputId = "moreInfoSelectFertilizer",
             #                                                                                 label = "Not sure how to select/unselect items?",
             #                                                                                 value = FALSE),
             #                                                                   conditionalPanel(
             #                                                                     condition = "input.moreInfoSelectFertilizer == true",
             #                                                                     helpText("All fertilizers are selected by default. To drop nutrients, click on the item(s)
             #                                                                                 above and press delete on your keyboard. You can also control which items are shown
             #                                                                                 on the visualization itself. Click the fertilizer name directly in the visualization legend. 
             #                                                                                 Double click to show only that fertilizer. Single click to drop that fertilizer from the plot 
             #                                                                                 or to add it back if it was previously dropped. ")
             #                                                                   ), #end of short conditional panel
             #                                                                    
             #                                                                    # fluidRow(
             #                                                                    #   column(12,tags$span("Information on fertilizer types:",bsButton("productdescriptions", label = "", icon = icon("info"), style = "info", size = "extra-small") )
             #                                                                    #   )
             #                                                                    # 
             #                                                                    # ),
             #                                                                   #  HTML('<br/>'),
             #                                                                    
             #                                                                    # bsPopover(
             #                                                                    #   id = "howToFertilizerSelection",
             #                                                                    #   title = "How to select/remove fertilizer types",
             #                                                                    #   content = paste0(
             #                                                                    #     "<p>By default, all fertilizer types are",
             #                                                                    #     " selected. To remove fertilizers, click",
             #                                                                    #      " the fertilizer and press delete on",
             #                                                                    #      " your keyboard. That dropped fertilizer",
             #                                                                    #      " will then appear in a dropdown list",
             #                                                                    #      " for reselection. <br>",
             #                                                                    #     "<hr>",
             #                                                                    #      " Alternatively, click the",
             #                                                                    #      " fertilizer name directly in the ",
             #                                                                    #      " visualization legend. Double click to",
             #                                                                    #      " show only that fertilizer. Single click",
             #                                                                    #      " to drop that fertilizer from the plot", 
             #                                                                    #      " or to add it back if it was previously",
             #                                                                    #      " dropped. </p>") ,
             #                                                                    #   placement = "right",
             #                                                                    #   trigger = "hover",
             #                                                                    #   options = list(container = "body")
             #                                                                    # ),
             #                                                                    # 
             #                                                                    bsPopover(
             #                                                                      id = "productdescriptions",
             #                                                                      title = "Description of fertilizer products",
             #                                                                      content = paste0(
             #                                                                        "<p>Fertilizers are categorized by primary",
             #                                                                        " nutrient: nitrogen, phosphorus, and ",
             #                                                                        "potassium.<br>", 
             #                                                                        "<b>Nitrogenous</b> fertilizers include ",
             #                                                                        "ammonia and urea. <br>",
             #                                                                        "<b>Phosphorus-based</b> fertilizers include ",
             #                                                                        "monoammonium phosphate (MAP) and",
             #                                                                        " diammonium phosphate (DAP).<br>",
             #                                                                        "<b> Potassium-based</b> fertilizers include",
             #                                                                        " muriate of potash (MOP).</p>") ,
             #                                                                      placement = "right",
             #                                                                      trigger = "hover",
             #                                                                      options = list(container = "body")
             #                                                                    ),
             #                                                                   
             #                                                                  # hr(),
             #                                                                   
             #                                                                    # fluidRow(align="center",
             #                                                                    # actionButton("popupFPrice", label = "More information on products", icon("paper-plane"), 
             #                                                                    #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
             #                                                                    # ),
             #                                                                    # HTML('<br/>'),
             #                                                                    
             #                                                                    radioButtons("index", 
             #                                                                                 label = tags$span("Select y axis variable",bsButton("infoPriceVsIndex", label = "", icon = icon("info"), style = "info", size = "extra-small") ),
             #                                                                                 choices = list("Prices (USD/MT)" = 1, "Indexed values (2019=100)" = 2),
             #                                                                                 selected = 1
             #                                                                    ),
             #                                                                    
             #                                                                    # fluidRow(
             #                                                                    #   column(12,tags$span("Information on this choice",bsButton("infoPriceVsIndex", label = "", icon = icon("info"), style = "info", size = "extra-small") )
             #                                                                    #   )
             #                                                                    #   
             #                                                                    # ),
             #                                                                    
             #                                                                    bsPopover(
             #                                                                      id = "infoPriceVsIndex",
             #                                                                      title = "Prices vs indexed prices",
             #                                                                      content = paste0(
             #                                                                        "<p>Actual prices are those in spot markets in",
             #                                                                        " USD per metric ton. The price index is calculated",
             #                                                                        " based on a base period of 2019 (price in 2019 = 100).</p>") ,
             #                                                                      placement = "right",
             #                                                                      trigger = "hover",
             #                                                                      options = list(container = "body")
             #                                                                    ),
             #                                                                    
             #                                                                    # hr(style = "border-top: 1px solid #000000;"
             #                                                                    # ),
             #                                                                    # #checkboxInput
             #                                                                    # checkboxInput(inputId = "explainerIndexPrices",
             #                                                                    #               label = "More info about this choice",
             #                                                                    #               value = FALSE
             #                                                                    # ),
             #                                                                    # conditionalPanel(
             #                                                                    #   condition = "input.explainerIndexPrices == true",
             #                                                                    #   helpText("Actual prices are those in spot markets in USD per metric ton. The price index is calculated based on a base period of 2019 (price in 2019 = 100).")
             #                                                                    # ) #end of short conditional panel
             #                                                                    
             #                                                                  )
             #                                                           ),
             #                                                           column(8,
             #                                                               
             #                                                                 # plotlyOutput("plot1")
             #                                                                 helpText("Currently under maintenance. Check back soon.")
             #                                                                 
             #                                                           )#,
             #                                                           # fluidRow(
             #                                                           #          hr(style="margin-bottom:0;"),
             #                                                           #          helpText("Source: own elaboration based on Bloomberg", style = "font-size:12px;")
             #                                                           # )
             #                                                           # column(8,
             #                                                           #        wellPanel("", 
             #                                                           #                  includeHTML("FertilizerNarratives.html"))
             #                                                           #        )
             #                                                         )
             #                                                         
             #                                                         ### end of tab parenth
             #                                         )
             #                                         # fluidRow(align="center",
             #                                         #   column(8,
             #                                         #          tags$span("Fertilizer product descriptions",bsButton("productDescriptionsOutput", label = "", icon = icon("info"), style = "info", size = "extra-small") ),
             #                                         #         
             #                                         #           bsPopover(
             #                                         #            id = "productDescriptionsOutput",
             #                                         #            title = "Description of fertilizer products",
             #                                         #            content = paste0(
             #                                         #              "<p>Fertilizers are categorized by primary",
             #                                         #              " nutrient: nitrogen, phosphorus, and ",
             #                                         #              "potassium.<br>", 
             #                                         #              "<b>Nitrogenous</b> fertilizers include ",
             #                                         #              "ammonia and urea. <br>",
             #                                         #              "<b>Phosphorus-based</b> fertilizers include ",
             #                                         #              "monoammonium phosphate (MAP) and",
             #                                         #              " diammonium phosphate (DAP).<br>",
             #                                         #              "<b> Potassium-based</b> fertilizers include",
             #                                         #              " muriate of potash (MOP).</p>") ,
             #                                         #            placement = "below",
             #                                         #            trigger = "hover",
             #                                         #            options = list(container = "body")
             #                                         #          )
             #                                         #          
             #                                         #   
             #                                         # ))
             #                                         
             #                                         )),
             #                                
             #                                ###########Tab on nutrient
             #                                tabPanel("Fertilizer input prices",
             #                                         fluidRow(column(12,
             #                                                         HTML('<br/>'),
             # 
             #                                                         fluidRow(
             #                                                           column(4,
             #                                                                  wellPanel(
             #                                                                    # p(strong("How-to")),
             #                                                                  #  HTML('<h5><b>Introduction</b></h5>'),
             #                                                                    helpText("Instead of product prices, prices are shown by nutrient. 
             #                                                                             All prices are indexed (2019=100)."),
             #                                                                    # h4("Filter"),
             #                                                                    #  p(strong("Date")),
             #                                                                    #  helpText("Indicate the date range below or by clicking and dragging
             #                                                                    #           on the visualization"),
             #                                                                    sliderInput("Date2", "Date range",
             #                                                                                min = min(p_nutrient_long$date),
             #                                                                                max = max(p_nutrient_long$date),
             #                                                                                value = c(as.Date("2019-01-04", "%Y-%m-%d"), max(p_nutrient_long$date)),
             #                                                                                timeFormat="%b %Y"
             #                                                                                # ticks = FALSE
             #                                                                    ),
             #                                                                    
             #                                                                    
             #                                                                    
             #                                                                    selectInput(inputId = "Nut_selected",
             #                                                                                label = tags$span("Select nutrient:",bsButton("nutrientDescriptions", label = "", icon = icon("info"), style = "info", size = "extra-small") ),
             #                                                                                choices = c("N", "P",  "K", "NPK Index"
             #                                                                                ),
             #                                                                                selected = c("N", "P",  "K", "NPK Index"
             #                                                                                ),
             #                                                                                
             #                                                                                multiple = TRUE,
             #                                                                                selectize =TRUE
             #                                                                                #  width = "220px"
             #                                                                                
             #                                                                    ),
             #                                                                    
             #                                                                    bsPopover(
             #                                                                      id = "nutrientDescriptions",
             #                                                                      title = "Description of nutrients",
             #                                                                      content = paste0(
             #                                                                        "<p>The primary nutrients for fertilizers are",
             #                                                                        " nitrogen (N), phosphorus (P), and Potassium (K)",
             #                                                                        ", which explains the choice of nutrients. </p>") ,
             #                                                                      placement = "right",
             #                                                                      trigger = "hover",
             #                                                                      options = list(container = "body")
             #                                                                    ),
             #                                                                    checkboxInput(inputId = "moreInfoSelectNutrient",
             #                                                                                    label = "Not sure how to select/unselect items?",
             #                                                                                    value = FALSE),
             #                                                                      conditionalPanel(
             #                                                                        condition = "input.moreInfoSelectNutrient == true",
             #                                                                        helpText("All nutrients are selected by default. To drop nutrients, click on the item(s)
             #                                                                                 above and press delete on your keyboard. You can also control which items are shown
             #                                                                                 on the visualization itself. Click the fertilizer name directly in the visualization legend. 
             #                                                                                 Double click to show only that fertilizer. Single click to drop that fertilizer from the plot 
             #                                                                                 or to add it back if it was previously dropped. ")
             #                                                                      ) #end of short conditional panel
             # 
             #                                                                      
             #                                                                    
             #                                                                    # radioButtons("index2", label = "Select what to show on y axis:",
             #                                                                    #              choices = list("Actual prices" = 1, "Indexed values" = 2),
             #                                                                    #              selected = 1
             #                                                                    # )
             #                                                                    
             #                                                                  )
             #                                                           ),
             #                                                           column(8,
             #                                                                  column(12,
             #                                                                         helpText("Currently under maintenance. Check back soon.")
             #                                                                         
             #                                                                #  plotlyOutput("plot2")
             #                                                           )#,
             #                                                         #   column(12,
             #                                                         #   fluidRow(
             #                                                         #     hr(style="margin-bottom:0;"),
             #                                                         #     helpText("Source: own elaboration based on Bloomberg", style = "font-size:12px;")
             #                                                         #   )
             #                                                         # )
             #                                                           )
             #                                                         
             #                                                         
             #                                         )))),# end of nutrient tab
             #                                
             #                                tabPanel("Input prices (natural gas)",
             #                                         fluidRow(column(12,
             #                                                         HTML('<br/>'),
             #                                                         
             #                                                         
             #                                                         
             #                                                         fluidRow(
             #                                                           column(4,
             #                                                                  wellPanel(
             #                                                                    # p(strong("How-to")),
             #                                                                    # h4("How-to"),
             #                                                                  #  HTML('<h5><b>Introduction</b></h5>'),
             #                                                                    helpText("Natural gas provides energy for fertilizer production, but it is also a key feedstock, particularly for nitrogenous fertilizer production."),
             # 
             #                                                                    radioButtons("buttonInputsOUtputs", "Select:",
             #                                                                                 choices = list("Natural gas prices only" = 1, "Natural gas with fertilizer prices" = 2),
             #                                                                                 selected = 1),
             #                                                                    #if 1
             #                                                                    conditionalPanel(
             #                                                                      condition = "input.buttonInputsOUtputs == 1",
             #                                                                  
             #                                                                    
             #                                                                    sliderInput("Datenatgas", "Date range",
             #                                                                                min = min(p_natgas_long$date),
             #                                                                                max = max(p_natgas_long$date),
             #                                                                                value = c(as.Date("2019-01-04", "%Y-%m-%d"), max(p_natgas_long$date)),
             #                                                                                timeFormat="%b %Y"
             #                                                                                # ticks = FALSE
             #                                                                    ),
             #                                                                    
             #                                                                    checkboxGroupInput(inputId = "Natgas_selected",
             #                                                                                       label = "Select natural gas markets:",
             #                                                                                       choices = c("Natural Gas - USA", "Natural Gas - EU"), 
             #                                                                                       selected = c("Natural Gas - USA", "Natural Gas - EU")
             #                                                                    ),
             #                                                                    radioButtons("index_natgas", 
             #                                                                                 label = tags$span("Select y axis variable",bsButton("infoPriceVsIndexNatGas", label = "", icon = icon("info"), style = "info", size = "extra-small") ),
             #                                                                                 choices = list("Prices (USD/mmBTU)" = 1, "Indexed values (2019=100)" = 2),
             #                                                                                 selected = 1
             #                                                                    ),
             #                                                                    bsPopover(
             #                                                                      id = "infoPriceVsIndexNatGas",
             #                                                                      title = "Prices vs indexed prices",
             #                                                                      content = paste0(
             #                                                                        "<p>Actual prices are monthly averages in USD per mmBTU.",
             #                                                                        " Indexed values are calculated using 2019 as a base",
             #                                                                        " (price in 2019 = 100).</p>") ,
             #                                                                      placement = "right",
             #                                                                      trigger = "hover",
             #                                                                      options = list(container = "body")
             #                                                                    ),
             #                                                                  ), #end of  conditional panel
             #                                                                  
             #                                                                  
             #                                                                  #if 2
             #                                                                  conditionalPanel(
             #                                                                    condition = "input.buttonInputsOUtputs == 2",
             #                                                                    
             #                                                                    sliderInput("dateInputOutput", "Date range",
             #                                                                                min = as.Date("2018-01-01", "%Y-%m-%d"),
             #                                                                                max = max(p_natgas_long$date),
             #                                                                                value = c(as.Date("2021-01-01", "%Y-%m-%d"), max(p_natgas_long$date)),
             #                                                                                timeFormat="%b %Y"
             #                                                                                # ticks = FALSE
             #                                                                    ),
             #                                                                    
             #                                                                    checkboxGroupInput("inputOutput_products", 
             #                                                                                 label = tags$span("Select items",
             #                                                                                                   #bsButton("infoPriceVsIndexNatGas", label = "", icon = icon("info"), style = "info", size = "extra-small") 
             #                                                                                                   ),
             #                                                                                 choices = c("Natural Gas - USA", "Natural Gas - EU", "N","P", "K", "NPK Index"), 
             #                                                                                 selected = c("Natural Gas - USA", "N", "NPK Index")
             #                                                                    ),
             #                                                                    
             #                                                                    HTML('<h5><b>Units</b></h5>'),
             #                                                                    helpText("All prices are indexed (2019=100) in order to make them comparable."),
             #                                                                    
             #                                                                    
             #                                                                    
             #                                                                    
             #                                                                  )#end of conditional panel 2
             #                                                                  )# end of well panel
             #                                                                  
             #                                                                  
             #                                                                  
             #                                                           ),
             #                                                           column(8,
             #                                                                  helpText("Currently under maintenance. Check back soon.")
             #                                                                  
             #                                                                  # conditionalPanel(
             #                                                                  #   condition = "input.buttonInputsOUtputs == 1",
             #                                                                  # 
             #                                                                  # plotlyOutput("plotnatgas")
             #                                                                  # ),
             #                                                                  # conditionalPanel(
             #                                                                  #   condition = "input.buttonInputsOUtputs == 2",
             #                                                                  #   
             #                                                                  #   plotlyOutput("plotnatgasFertilizer")
             #                                                                  # )
             #                                                                  
             #                                                                  
             #                                                                  
             #                                                                  
             #                                                                  
             #                                                           ),
             #                                                           # fluidRow(
             #                                                           #   hr(style="margin-bottom:0;"),
             #                                                           #   helpText("Source: NY Mercantile, ICE", style = "font-size:12px;")
             #                                                           # )
             #                                                         )
             #                                                         
             #                                         ))), #end of nat gas prices tab
             #                                
             #                                
             #                                
             #                              
             #                                
             #          ))
             #          
             #          ###
             # ),
             tabPanel("Prices", fluid = TRUE,
                      # titlePanel("Fertilizer Dashboard"),
                      
                      column(12,tabsetPanel(id="prices_tabs",
                                            tabPanel("Fertilizer prices",
                                                     
                                                     fluidRow(column(12,
                                                                     HTML('<br/>'),
                                                                     
                                                                     fluidRow(
                                                                       column(4,
                                                                              wellPanel(
                                                                                # p(strong("How-to")),
                                                                                # fluidRow(align="center",
                                                                                # actionButton("howToTabPrices", label = "How to use this tab",
                                                                                #          #    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                                                #              )
                                                                                # ),
                                                                                #  hr(),
                                                                                
                                                                                # HTML('<br/>'),
                                                                                # h4("How-to"),
                                                                                # helpText("Indicate the date range below or directly in the visualization by clicking and dragging the desired range."),
                                                                                # h4("Filter"),
                                                                                #  p(strong("Date")),
                                                                                #  helpText("Indicate the date range below or by clicking and dragging
                                                                                #           on the visualization"),
                                                                                sliderInput("Date_FertPrices", "Date range",
                                                                                            min = min(WB_fertilizer_prices$Date),
                                                                                            max = max(WB_fertilizer_prices$Date),
                                                                                            value = c(as.Date("2020-01-01", "%Y-%m-%d"), max(WB_fertilizer_prices$Date)),
                                                                                            timeFormat="%b %Y"
                                                                                            # ticks = FALSE
                                                                                ),
                                                                                selectInput(inputId = "Fert_selected",
                                                                                            label = tags$span("Select fertilizer type:",shinyBS::bsButton("productdescriptions", label = "", icon = icon("info"), style = "info", size = "small") ),
                                                                                            choices = unique(WB_fertilizer_prices$Commodity) , # Extract distinct values from Commodity column
                                                                                            selected = unique(WB_fertilizer_prices$Commodity),
                                                                                            multiple = TRUE,
                                                                                            selectize =TRUE
                                                                                ),
                                                                                bsPopover(
                                                                                  id = "productdescriptions",
                                                                                  title = "Description of fertilizer products",
                                                                                  content = paste0(
                                                                                    
                                                                                    "<b>DAP (diammonium phosphate)</b> : spot, f.o.b. US Gulf<br>",
                                                                                    "<b>Urea</b> Urea (Ukraine): prill spot f.o.b. Middle East (from March 2022); previously, f.o.b. Black Sea. <br>",
                                                                                    "<b>TSP (triple superphosphate)</b>: spot, import US Gulf <br>",
                                                                                    "<b>Potassium chloride (muriate of potash)</b>: Brazil CFR granular spot price (from January 2020); previously, f.o.b. Vancouver"
                                                                                  ) ,
                                                                                  placement = "right",
                                                                                  trigger = "hover",
                                                                                  options = list(container = "body")
                                                                                )
                                                                                # shinyBS::bsPopover(
                                                                                #   id = "productdescriptions",
                                                                                #   title = "Description of Fertilizer Products",
                                                                                #   content = paste0(
                                                                                #     "DAP (diammonium phosphate): spot, f.o.b. US Gulf\n",
                                                                                #     "Potassium chloride (muriate of potash): Brazil CFR granular spot price (from January 2020); previously, f.o.b. Vancouver\n",
                                                                                #     "TSP (triple superphosphate): spot, import US Gulf\n",
                                                                                #     "Urea (Ukraine): prill spot f.o.b. Middle East (from March 2022); previously, f.o.b. Black Sea."
                                                                                #   ),
                                                                                #   placement = "bottom",
                                                                                #   trigger = "click",
                                                                                #   options = list(container = "body")
                                                                                # )
                                                                                
                                                                                # hr(),
                                                                              )),
                                                                       column(8,
                                                                              plotlyOutput("plot1")
                                                                       )
                                                                     )
                                                     )
                                                     )
                                            ),
                                            tabPanel("Fertilizer input prices",
                                                     
                                                     fluidRow(column(12,
                                                                     HTML('<br/>'),
                                                                     
                                                                     fluidRow(
                                                                       column(4,
                                                                              wellPanel(
                                                                                # p(strong("How-to")),
                                                                                # fluidRow(align="center",
                                                                                # actionButton("howToTabPrices", label = "How to use this tab",
                                                                                #          #    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                                                #              )
                                                                                # ),
                                                                                #  hr(),
                                                                                
                                                                                # HTML('<br/>'),
                                                                                # h4("How-to"),
                                                                                # helpText("Indicate the date range below or directly in the visualization by clicking and dragging the desired range."),
                                                                                # h4("Filter"),
                                                                                #  p(strong("Date")),
                                                                                #  helpText("Indicate the date range below or by clicking and dragging
                                                                                #           on the visualization"),
                                                                                sliderInput("Date_NatGasPrices", "Date range",
                                                                                            min = min(WB_NatGas_prices$Date),
                                                                                            max = max(WB_NatGas_prices$Date),
                                                                                            value = c(as.Date("2020-01-01", "%Y-%m-%d"), max(WB_NatGas_prices$Date)),
                                                                                            timeFormat="%b %Y"
                                                                                            # ticks = FALSE
                                                                                )#,
                                                                                # selectInput(inputId = "Fert_selected",
                                                                                #             label = tags$span("Select fertilizer type:",bsButton("productdescriptions", label = "", icon = icon("info"), style = "info", size = "extra-small") ),
                                                                                #             choices = unique(WB_fertilizer_prices$Commodity) , # Extract distinct values from Commodity column
                                                                                #             selected = unique(WB_fertilizer_prices$Commodity),
                                                                                #             multiple = TRUE,
                                                                                #             selectize =TRUE
                                                                                # ) 
                                                                              )),
                                                                       column(8,
                                                                              plotlyOutput("plotNatGas")
                                                                       )
                                                                     )
                                                     )
                                                     )
                                            ),   
                                            tabPanel("Fertilizer and natural gas price relationship",
                                                     
                                                     fluidRow(column(12,
                                                                     HTML('<br/>'),
                                                                     
                                                                     fluidRow(
                                                                       column(4,
                                                                              wellPanel(
                                                                                # p(strong("How-to")),
                                                                                # fluidRow(align="center",
                                                                                # actionButton("howToTabPrices", label = "How to use this tab",
                                                                                #          #    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                                                #              )
                                                                                # ),
                                                                                #  hr(),
                                                                                
                                                                                # HTML('<br/>'),
                                                                                # h4("How-to"),
                                                                                # helpText("Indicate the date range below or directly in the visualization by clicking and dragging the desired range."),
                                                                                # h4("Filter"),
                                                                                #  p(strong("Date")),
                                                                                #  helpText("Indicate the date range below or by clicking and dragging
                                                                                #           on the visualization"),
                                                                                sliderInput("Date_NatGas_fert_Prices", "Date range",
                                                                                            min = min(WB_pricesIndexed$Date),
                                                                                            max = max(WB_pricesIndexed$Date),
                                                                                            value = c(as.Date("2000-01-01", "%Y-%m-%d"), max(WB_pricesIndexed$Date)),
                                                                                            timeFormat="%b %Y"
                                                                                            # ticks = FALSE
                                                                                ),
                                                                                selectInput(inputId = "FertNatGas_selected",
                                                                                            label = tags$span("Select items:",bsButton("productdescriptions_natGasFert", label = "", icon = icon("info"), style = "info", size = "small") ),
                                                                                            choices = unique(WB_pricesIndexed$Commodity) , # Extract distinct values from Commodity column
                                                                                            selected = unique(WB_pricesIndexed$Commodity),
                                                                                            multiple = TRUE,
                                                                                            selectize =TRUE
                                                                                ),
                                                                                bsPopover(
                                                                                  id = "productdescriptions_natGasFert",
                                                                                  title = "Description of fertilizer products",
                                                                                  content = paste0(
                                                                                    
                                                                                    "<b>DAP (diammonium phosphate)</b> : spot, f.o.b. US Gulf<br>",
                                                                                    "<b>Urea</b> Urea (Ukraine): prill spot f.o.b. Middle East (from March 2022); previously, f.o.b. Black Sea. <br>",
                                                                                    "<b>TSP (triple superphosphate)</b>: spot, import US Gulf <br>",
                                                                                    "<b>Potassium chloride (muriate of potash)</b>: Brazil CFR granular spot price (from January 2020); previously, f.o.b. Vancouver <br>",
                                                                                    "<b>Natural gas Europe</b>: from April 2015, Netherlands Title Transfer Facility (TTF); April 2010 to March 2015, average import border price and a spot price component, including UK; during June 2000 - March 2010 prices excludes UK. <br> ",
                                                                                    "<b>Natural gas United States</b>:  spot price at Henry Hub, Louisiana"
                                                                                    
                                                                                  ) ,
                                                                                  placement = "right",
                                                                                  trigger = "hover",
                                                                                  options = list(container = "body")
                                                                                )
                                                                              )),
                                                                       column(8,
                                                                              plotlyOutput("plotNatGasFert")
                                                                       )
                                                                     )
                                                     )
                                                     )
                                            ),                                              # tabPanel("Prices by fertilizer type",
                                            #          
                                            #          fluidRow(column(12,
                                            #                          HTML('<br/>'),
                                            # 
                                            #                          fluidRow(
                                            #                            column(4,
                                            #                                   wellPanel(
                                            #                                    # p(strong("How-to")),
                                            #                                     fluidRow(align="center",
                                            #                                     actionButton("howToTabPrices", label = "How to use this tab", 
                                            #                                              #    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                            #                                                  )
                                            #                                     ),
                                            #                                     hr(),
                                            #                                     
                                            #                                    # HTML('<br/>'),
                                            #                                     # h4("How-to"),
                                            #                                     # helpText("Indicate the date range below or directly in the visualization by clicking and dragging the desired range."),
                                            #                                     # h4("Filter"),
                                            #                                     #  p(strong("Date")),
                                            #                                     #  helpText("Indicate the date range below or by clicking and dragging
                                            #                                     #           on the visualization"),
                                            #                                     sliderInput("Date", "Date range",
                                            #                                                 min = min(p_fert_long$date),
                                            #                                                 max = max(p_fert_long$date),
                                            #                                                 value = c(as.Date("2019-01-04", "%Y-%m-%d"), max(p_fert_long$date)),
                                            #                                                 timeFormat="%b %Y"
                                            #                                                 # ticks = FALSE
                                            #                                     ),
                                            #                                    #hr(),
                                            #                                    
                                            #                                     
                                            #                                     # helpText("Use the sliders above to indicate the date range. 
                                            #                                     #          You can also control this and other features within the visualization
                                            #                                     #          itself."),
                                            #                                     
                                            #                             
                                            #                                     # checkboxGroupInput(inputId = "Fert_selected",
                                            #                                     # p(strong("Select:")),
                                            #                                     #helpText("Select whether to display average prices per product."),
                                            #                                     
                                            #                                     #info popup here: https://stackoverflow.com/questions/70679604/info-icon-next-to-label-of-a-selectinput-in-shiny
                                            #                                     #https://stackoverflow.com/questions/16449252/tooltip-on-shiny-r
                                            #                                
                                            #                                    tags$div(title=paste0(
                                            #                                          "By default, all fertilizer types are",
                                            #                                          " selected. To remove fertilizers, click",
                                            #                                           " the fertilizer and press delete on",
                                            #                                           " your keyboard. That dropped fertilizer",
                                            #                                           " will then appear in a dropdown list",
                                            #                                           " for reselection.",
                                            #                                           " Alternatively, click the",
                                            #                                           " fertilizer name directly in the ",
                                            #                                           " visualization legend. Double click to",
                                            #                                           " show only that fertilizer. Single click",
                                            #                                           " to drop that fertilizer from the plot",
                                            #                                           " or to add it back if it was previously",
                                            #                                           " dropped.") ,
                                            #                                             
                                            #                                     selectInput(inputId = "Fert_selected",
                                            #                                                 label = tags$span("Select fertilizer type:",bsButton("productdescriptions", label = "", icon = icon("info"), style = "info", size = "extra-small") ),
                                            #                                                                                       
                                            #                                                 choices =  c("Ammonia", "DAP",  "MAP",
                                            #                                                                 "MOP", "Phosphate Rock", "Urea"
                                            #                                                             ),
                                            #                                                 selected = c("Ammonia", "DAP",  "MAP",
                                            #                                                                "MOP", "Phosphate Rock", "Urea"
                                            #                                                             ),
                                            #                                                 
                                            #                                             
                                            #                                                 multiple = TRUE,
                                            #                                                 selectize =TRUE
                                            #                                                 #  width = "220px"
                                            #                                                 
                                            #                                     )#end of slider input
                                            #                                                 
                                            #                                     ), #end of div
                                            #                                    
                                            #                                    checkboxInput(inputId = "moreInfoSelectFertilizer",
                                            #                                                  label = "Not sure how to select/unselect items?",
                                            #                                                  value = FALSE),
                                            #                                    conditionalPanel(
                                            #                                      condition = "input.moreInfoSelectFertilizer == true",
                                            #                                      helpText("All fertilizers are selected by default. To drop nutrients, click on the item(s)
                                            #                                                  above and press delete on your keyboard. You can also control which items are shown
                                            #                                                  on the visualization itself. Click the fertilizer name directly in the visualization legend. 
                                            #                                                  Double click to show only that fertilizer. Single click to drop that fertilizer from the plot 
                                            #                                                  or to add it back if it was previously dropped. ")
                                            #                                    ), #end of short conditional panel
                                            #                                     
                                            #                                     # fluidRow(
                                            #                                     #   column(12,tags$span("Information on fertilizer types:",bsButton("productdescriptions", label = "", icon = icon("info"), style = "info", size = "extra-small") )
                                            #                                     #   )
                                            #                                     # 
                                            #                                     # ),
                                            #                                    #  HTML('<br/>'),
                                            #                                     
                                            #                                     # bsPopover(
                                            #                                     #   id = "howToFertilizerSelection",
                                            #                                     #   title = "How to select/remove fertilizer types",
                                            #                                     #   content = paste0(
                                            #                                     #     "<p>By default, all fertilizer types are",
                                            #                                     #     " selected. To remove fertilizers, click",
                                            #                                     #      " the fertilizer and press delete on",
                                            #                                     #      " your keyboard. That dropped fertilizer",
                                            #                                     #      " will then appear in a dropdown list",
                                            #                                     #      " for reselection. <br>",
                                            #                                     #     "<hr>",
                                            #                                     #      " Alternatively, click the",
                                            #                                     #      " fertilizer name directly in the ",
                                            #                                     #      " visualization legend. Double click to",
                                            #                                     #      " show only that fertilizer. Single click",
                                            #                                     #      " to drop that fertilizer from the plot", 
                                            #                                     #      " or to add it back if it was previously",
                                            #                                     #      " dropped. </p>") ,
                                            #                                     #   placement = "right",
                                            #                                     #   trigger = "hover",
                                            #                                     #   options = list(container = "body")
                                            #                                     # ),
                                            #                                     # 
                                            #                                     bsPopover(
                                            #                                       id = "productdescriptions",
                                            #                                       title = "Description of fertilizer products",
                                            #                                       content = paste0(
                                            #                                         "<p>Fertilizers are categorized by primary",
                                            #                                         " nutrient: nitrogen, phosphorus, and ",
                                            #                                         "potassium.<br>", 
                                            #                                         "<b>Nitrogenous</b> fertilizers include ",
                                            #                                         "ammonia and urea. <br>",
                                            #                                         "<b>Phosphorus-based</b> fertilizers include ",
                                            #                                         "monoammonium phosphate (MAP) and",
                                            #                                         " diammonium phosphate (DAP).<br>",
                                            #                                         "<b> Potassium-based</b> fertilizers include",
                                            #                                         " muriate of potash (MOP).</p>") ,
                                            #                                       placement = "right",
                                            #                                       trigger = "hover",
                                            #                                       options = list(container = "body")
                                            #                                     ),
                                            #                                    
                                            #                                   # hr(),
                                            #                                    
                                            #                                     # fluidRow(align="center",
                                            #                                     # actionButton("popupFPrice", label = "More information on products", icon("paper-plane"), 
                                            #                                     #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                            #                                     # ),
                                            #                                     # HTML('<br/>'),
                                            #                                     
                                            #                                     radioButtons("index", 
                                            #                                                  label = tags$span("Select y axis variable",bsButton("infoPriceVsIndex", label = "", icon = icon("info"), style = "info", size = "extra-small") ),
                                            #                                                  choices = list("Prices (USD/MT)" = 1, "Indexed values (2019=100)" = 2),
                                            #                                                  selected = 1
                                            #                                     ),
                                            #                                     
                                            #                                     # fluidRow(
                                            #                                     #   column(12,tags$span("Information on this choice",bsButton("infoPriceVsIndex", label = "", icon = icon("info"), style = "info", size = "extra-small") )
                                            #                                     #   )
                                            #                                     #   
                                            #                                     # ),
                                            #                                     
                                            #                                     bsPopover(
                                            #                                       id = "infoPriceVsIndex",
                                            #                                       title = "Prices vs indexed prices",
                                            #                                       content = paste0(
                                            #                                         "<p>Actual prices are those in spot markets in",
                                            #                                         " USD per metric ton. The price index is calculated",
                                            #                                         " based on a base period of 2019 (price in 2019 = 100).</p>") ,
                                            #                                       placement = "right",
                                            #                                       trigger = "hover",
                                            #                                       options = list(container = "body")
                                            #                                     ),
                                            #                                     
                                            #                                     # hr(style = "border-top: 1px solid #000000;"
                                            #                                     # ),
                                            #                                     # #checkboxInput
                                            #                                     # checkboxInput(inputId = "explainerIndexPrices",
                                            #                                     #               label = "More info about this choice",
                                            #                                     #               value = FALSE
                                            #                                     # ),
                                            #                                     # conditionalPanel(
                                            #                                     #   condition = "input.explainerIndexPrices == true",
                                            #                                     #   helpText("Actual prices are those in spot markets in USD per metric ton. The price index is calculated based on a base period of 2019 (price in 2019 = 100).")
                                            #                                     # ) #end of short conditional panel
                                            #                                     
                                            #                                   )
                                            #                            ),
                                            #                            column(8,
                                            #                                   #ggvisOutput("plot1")
                                            #                                   # plotOutput("plot1")
                                            #                                  # plotlyOutput("plot1")
                                            #                                  helpText("Currently under maintenance. Check back soon.")
                                            #                                  
                                            #                            )#,
                                            #                            # fluidRow(
                                            #                            #          hr(style="margin-bottom:0;"),
                                            #                            #          helpText("Source: own elaboration based on Bloomberg", style = "font-size:12px;")
                                            #                            # )
                                            #                            # column(8,
                                            #                            #        wellPanel("", 
                                            #                            #                  includeHTML("FertilizerNarratives.html"))
                                            #                            #        )
                                            #                          )
                                            #                          
                                            #                          ### end of tab parenth
                                            #          )
                                            #          # fluidRow(align="center",
                                            #          #   column(8,
                                            #          #          tags$span("Fertilizer product descriptions",bsButton("productDescriptionsOutput", label = "", icon = icon("info"), style = "info", size = "extra-small") ),
                                            #          #         
                                            #          #           bsPopover(
                                            #          #            id = "productDescriptionsOutput",
                                            #          #            title = "Description of fertilizer products",
                                            #          #            content = paste0(
                                            #          #              "<p>Fertilizers are categorized by primary",
                                            #          #              " nutrient: nitrogen, phosphorus, and ",
                                            #          #              "potassium.<br>", 
                                            #          #              "<b>Nitrogenous</b> fertilizers include ",
                                            #          #              "ammonia and urea. <br>",
                                            #          #              "<b>Phosphorus-based</b> fertilizers include ",
                                            #          #              "monoammonium phosphate (MAP) and",
                                            #          #              " diammonium phosphate (DAP).<br>",
                                            #          #              "<b> Potassium-based</b> fertilizers include",
                                            #          #              " muriate of potash (MOP).</p>") ,
                                            #          #            placement = "below",
                                            #          #            trigger = "hover",
                                            #          #            options = list(container = "body")
                                            #          #          )
                                            #          #          
                                            #          #   
                                            #          # ))
                                            #          
                                            #          )),
                                            
                                            ###########Tab on nutrient
                                            # tabPanel("Prices by nutrient",
                                            #          fluidRow(column(12,
                                            #                          HTML('<br/>'),
                                            # 
                                            #                          fluidRow(
                                            #                            column(4,
                                            #                                   wellPanel(
                                            #                                     # p(strong("How-to")),
                                            #                                   #  HTML('<h5><b>Introduction</b></h5>'),
                                            #                                     helpText("Instead of product prices, prices are shown by nutrient. 
                                            #                                              All prices are indexed (2019=100)."),
                                            #                                     # h4("Filter"),
                                            #                                     #  p(strong("Date")),
                                            #                                     #  helpText("Indicate the date range below or by clicking and dragging
                                            #                                     #           on the visualization"),
                                            #                                     sliderInput("Date2", "Date range",
                                            #                                                 min = min(p_nutrient_long$date),
                                            #                                                 max = max(p_nutrient_long$date),
                                            #                                                 value = c(as.Date("2019-01-04", "%Y-%m-%d"), max(p_nutrient_long$date)),
                                            #                                                 timeFormat="%b %Y"
                                            #                                                 # ticks = FALSE
                                            #                                     ),
                                            #                                     
                                            #                                     
                                            #                                     
                                            #                                     selectInput(inputId = "Nut_selected",
                                            #                                                 label = tags$span("Select nutrient:",bsButton("nutrientDescriptions", label = "", icon = icon("info"), style = "info", size = "extra-small") ),
                                            #                                                 choices = c("N", "P",  "K", "NPK Index"
                                            #                                                 ),
                                            #                                                 selected = c("N", "P",  "K", "NPK Index"
                                            #                                                 ),
                                            #                                                 
                                            #                                                 multiple = TRUE,
                                            #                                                 selectize =TRUE
                                            #                                                 #  width = "220px"
                                            #                                                 
                                            #                                     ),
                                            #                                     
                                            #                                     bsPopover(
                                            #                                       id = "nutrientDescriptions",
                                            #                                       title = "Description of nutrients",
                                            #                                       content = paste0(
                                            #                                         "<p>The primary nutrients for fertilizers are",
                                            #                                         " nitrogen (N), phosphorus (P), and Potassium (K)",
                                            #                                         ", which explains the choice of nutrients. </p>") ,
                                            #                                       placement = "right",
                                            #                                       trigger = "hover",
                                            #                                       options = list(container = "body")
                                            #                                     ),
                                            #                                     checkboxInput(inputId = "moreInfoSelectNutrient",
                                            #                                                     label = "Not sure how to select/unselect items?",
                                            #                                                     value = FALSE),
                                            #                                       conditionalPanel(
                                            #                                         condition = "input.moreInfoSelectNutrient == true",
                                            #                                         helpText("All nutrients are selected by default. To drop nutrients, click on the item(s)
                                            #                                                  above and press delete on your keyboard. You can also control which items are shown
                                            #                                                  on the visualization itself. Click the fertilizer name directly in the visualization legend. 
                                            #                                                  Double click to show only that fertilizer. Single click to drop that fertilizer from the plot 
                                            #                                                  or to add it back if it was previously dropped. ")
                                            #                                       ) #end of short conditional panel
                                            # 
                                            #                                       
                                            #                                     
                                            #                                     # radioButtons("index2", label = "Select what to show on y axis:",
                                            #                                     #              choices = list("Actual prices" = 1, "Indexed values" = 2),
                                            #                                     #              selected = 1
                                            #                                     # )
                                            #                                     
                                            #                                   )
                                            #                            ),
                                            #                            column(8,
                                            #                                   column(12,
                                            #                                   helpText("Currently under maintenance. Check back soon.")
                                            #                                  # plotlyOutput("plot2")
                                            #                            )#,
                                            #                          #   column(12,
                                            #                          #   fluidRow(
                                            #                          #     hr(style="margin-bottom:0;"),
                                            #                          #     helpText("Source: own elaboration based on Bloomberg", style = "font-size:12px;")
                                            #                          #   )
                                            #                          # )
                                            #                            )
                                            #                          
                                            #                          
                                            #          )))),# end of nutrient tab
                                            
                                            # tabPanel("Input prices (natural gas)",
                                            #          fluidRow(column(12,
                                            #                          HTML('<br/>'),
                                            #                          
                                            #                          
                                            #                          
                                            #                          fluidRow(
                                            #                            column(4,
                                            #                                   wellPanel(
                                            #                                     # p(strong("How-to")),
                                            #                                     # h4("How-to"),
                                            #                                   #  HTML('<h5><b>Introduction</b></h5>'),
                                            #                                     helpText("Natural gas provides energy for fertilizer production, but it is also a key feedstock, particularly for nitrogenous fertilizer production."),
                                            # 
                                            #                                     radioButtons("buttonInputsOUtputs", "Select:",
                                            #                                                  choices = list("Natural gas prices only" = 1, "Natural gas with fertilizer prices" = 2),
                                            #                                                  selected = 1),
                                            #                                     #if 1
                                            #                                     conditionalPanel(
                                            #                                       condition = "input.buttonInputsOUtputs == 1",
                                            #                                   
                                            #                                     
                                            #                                     sliderInput("Datenatgas", "Date range",
                                            #                                                 min = min(p_natgas_long$date),
                                            #                                                 max = max(p_natgas_long$date),
                                            #                                                 value = c(as.Date("2019-01-04", "%Y-%m-%d"), max(p_natgas_long$date)),
                                            #                                                 timeFormat="%b %Y"
                                            #                                                 # ticks = FALSE
                                            #                                     ),
                                            #                                     
                                            #                                     checkboxGroupInput(inputId = "Natgas_selected",
                                            #                                                        label = "Select natural gas markets:",
                                            #                                                        choices = c("Natural Gas - USA", "Natural Gas - EU"), 
                                            #                                                        selected = c("Natural Gas - USA", "Natural Gas - EU")
                                            #                                     ),
                                            #                                     radioButtons("index_natgas", 
                                            #                                                  label = tags$span("Select y axis variable",bsButton("infoPriceVsIndexNatGas", label = "", icon = icon("info"), style = "info", size = "extra-small") ),
                                            #                                                  choices = list("Prices (USD/mmBTU)" = 1, "Indexed values (2019=100)" = 2),
                                            #                                                  selected = 1
                                            #                                     ),
                                            #                                     bsPopover(
                                            #                                       id = "infoPriceVsIndexNatGas",
                                            #                                       title = "Prices vs indexed prices",
                                            #                                       content = paste0(
                                            #                                         "<p>Actual prices are monthly averages in USD per mmBTU.",
                                            #                                         " Indexed values are calculated using 2019 as a base",
                                            #                                         " (price in 2019 = 100).</p>") ,
                                            #                                       placement = "right",
                                            #                                       trigger = "hover",
                                            #                                       options = list(container = "body")
                                            #                                     ),
                                            #                                   ), #end of  conditional panel
                                            #                                   
                                            #                                   
                                            #                                   #if 2
                                            #                                   conditionalPanel(
                                            #                                     condition = "input.buttonInputsOUtputs == 2",
                                            #                                     
                                            #                                     sliderInput("dateInputOutput", "Date range",
                                            #                                                 min = as.Date("2018-01-01", "%Y-%m-%d"),
                                            #                                                 max = max(p_natgas_long$date),
                                            #                                                 value = c(as.Date("2021-01-01", "%Y-%m-%d"), max(p_natgas_long$date)),
                                            #                                                 timeFormat="%b %Y"
                                            #                                                 # ticks = FALSE
                                            #                                     ),
                                            #                                     
                                            #                                     checkboxGroupInput("inputOutput_products", 
                                            #                                                  label = tags$span("Select items",
                                            #                                                                    #bsButton("infoPriceVsIndexNatGas", label = "", icon = icon("info"), style = "info", size = "extra-small") 
                                            #                                                                    ),
                                            #                                                  choices = c("Natural Gas - USA", "Natural Gas - EU", "N","P", "K", "NPK Index"), 
                                            #                                                  selected = c("Natural Gas - USA", "N", "NPK Index")
                                            #                                     ),
                                            #                                     
                                            #                                     HTML('<h5><b>Units</b></h5>'),
                                            #                                     helpText("All prices are indexed (2019=100) in order to make them comparable."),
                                            #                                     
                                            #                                     
                                            #                                     
                                            #                                     
                                            #                                   )#end of conditional panel 2
                                            #                                   )# end of well panel
                                            #                                   
                                            #                                   
                                            #                                   
                                            #                            ),
                                            #                            column(8,
                                            #                                   # conditionalPanel(
                                            #                                   #   condition = "input.buttonInputsOUtputs == 1",
                                            #                                   # 
                                            #                                   # plotlyOutput("plotnatgas")
                                            #                                   # ),
                                            #                                   # conditionalPanel(
                                            #                                   #   condition = "input.buttonInputsOUtputs == 2",
                                            #                                   #   
                                            #                                   #   plotlyOutput("plotnatgasFertilizer")
                                            #                                   # )
                                            #                                   helpText("Currently under maintenance. Check back soon.")
                                            #                                   
                                            #                                   
                                            #                                   
                                            #                                   
                                            #                                   
                                            #                            ),
                                            #                            # fluidRow(
                                            #                            #   hr(style="margin-bottom:0;"),
                                            #                            #   helpText("Source: NY Mercantile, ICE", style = "font-size:12px;")
                                            #                            # )
                                            #                          )
                                            #                          
                                            #          ))), #end of nat gas prices tab
                                            
                                            
                                            
                                            
                                            
                      ))
                      
                      ###
             ),
             
             # #########START OF INPUT COSTS TAB
             # tabPanel("Input costs", fluid = TRUE,
             #          ####Section - start of nat gas price tab
             #          column(12,tabsetPanel(id="inputs_tabs",
             #                                
             #                                tabPanel("Natural gas prices",
             #                                         fluidRow(column(12,
             #                                                         
             #                                                         
             #                                                         fluidRow(
             #                                                           column(4,
             #                                                                  wellPanel(
             #                                                                    # p(strong("How-to")),
             #                                                                    # h4("How-to"),
             #                                                                    helpText("Natural gas is a key input and fuel source for fertilizer production. This tab 
             #                 shows natural gas prices (just to show you how this works; need to connect to the same data feeding the current dashboard)."),
             #                                                                    
             #                                                                    sliderInput("Datenatgas", "Date range",
             #                                                                                min = min(p_natgas_long$date),
             #                                                                                max = max(p_natgas_long$date),
             #                                                                                value = c(as.Date("2019-01-04", "%Y-%m-%d"), max(p_natgas_long$date)),
             #                                                                                timeFormat="%b %Y"
             #                                                                                # ticks = FALSE
             #                                                                    ),
             #                                                                    
             #                                                                    checkboxGroupInput(inputId = "Natgas_selected",
             #                                                                                 label = "Select natural gas markets:",
             #                                                                                 choices = c("Natural Gas - USA", "Natural Gas - EU"), 
             #                                                                                 selected = c("Natural Gas - USA", "Natural Gas - EU")
             #                                                                    ),
             #                                                                    radioButtons("index_natgas", label = "Select what to show on y axis:",
             #                                                                                 choices = list("Actual prices" = 1, "Indexed values" = 2),
             #                                                                                 selected = 1
             #                                                                    )
             #                                                                  )
             #                                                                  
             #                                                           ),
             #                                                           column(8,
             #                                                                  plotlyOutput("plotnatgas")
             #                                                           )
             #                                                         )
             #                                                         
             #                                         ))), #end of nat gas prices tab
             #                                
             #                                
             #                                ### Section - Start of nat gas compared to fert 
             #                                tabPanel("Natural gas and fertilizer price relationship",
             #                                         fluidRow(column(12,
             #                                                         helpText("We could consider showing plotting natural gas prices and fertilizer prices together to show the relationship (using indexed values). 
             #     Fertilizers shown could be the averages by fertilizer type.")
             #                                                         
             #                                         )))### Section - end of nat gas compared to fert
             #                                
             #          ))  #end of inputs tab
             #          
             #          
             # ),  ##########END OF INPUT COSTS TAB
             
             
             ##############START OF MARKET CHARACTERISITCS TAB
             # 
             # tabPanel("Market characteristics", fluid = TRUE,
             #          fluidRow(
             #            column(12,
             #                   wellPanel(
             #                     
             #                     "", 
             #                     align="center",
             #                     
             #                             includeHTML("FertilizerMarketCharacteristics.html"))
             #            ) #end of column 12
             #          ), #end of fluid row
             #          column(12,
             #                 tabsetPanel(id="MarketCharacteristics_tabs",
             #                                # tabPanel("Fertilizer trade",
             #                                #          fluidRow(column(12,
             #                                #                          HTML('<br/>'),
             #                                #                          wellPanel(
             #                                #                            align="center",
             #                                # 
             #                                #                           # helpText("Select from the following:"),  
             #                                #                            radioButtons(inputId = "fertilizerUseChoices",
             #                                #                                         label = "Select from the following:",
             #                                #                                         choices = list("Net imports" = 1, "Import dependence ratio" = 2,"Main exporters and importers" = 3), 
             #                                #                                         selected = 1,
             #                                #                                         inline=T
             #                                #                            )
             #                                #                            
             #                                #                          ) #end of wellPanel
             #                                #          )), #end of col 12 and end of fluidRow
             #                                #          conditionalPanel(
             #                                #            condition = "input.fertilizerUseChoices == 2",
             #                                #          fluidRow(column(12,
             #                                #                                          column(3, 
             #                                #                                                 wellPanel(
             #                                #                                                   # radioButtons(inputId = "selectionImports",
             #                                #                                                   #              label = "Select:",
             #                                #                                                   #              choices = list("Net imports" = 1, "Dependency ratio" = 2), 
             #                                #                                                   #              selected = 2
             #                                #                                                   # ),
             #                                #                                                   
             #                                #                                                     helpText("You are viewing the ratio
             #                                #                                            between imports and use."),
             #                                #                                                     radioButtons(inputId = "dep_nutrient",
             #                                #                                                                  label = "Select:",
             #                                #                                                                  choices = list("Nitrogenous" = 1, "Phosphate" = 3,"Potash" =2), 
             #                                #                                                                  selected = 1
             #                                #                                                     ),
             #                                #                                                     
             #                                #                                                     radioButtons(inputId = "impDepMapOrTable",
             #                                #                                                                  label = "Select:",
             #                                #                                                                  choices = list("Show as map" = 1, "Show as table" = 2), 
             #                                #                                                                  selected = 1
             #                                #                                                     ),
             #                                #                                                     
             #                                #                                                     hr(style = "border-top: 1px solid #000000;"
             #                                #                                                     ),
             #                                #                                                     checkboxInput(inputId = "impDepMoreInfo",
             #                                #                                                                   label = "Tab explaination",
             #                                #                                                                   value = FALSE
             #                                #                                                     ),
             #                                #                                                     conditionalPanel(
             #                                #                                                       condition = "input.impDepMoreInfo == true",
             #                                #                                                       helpText("The ratio ranges from 0, no imports, to 1, 
             #                                #                                            domestic use fully supplied by imports.")
             #                                #                                                       ) #end of short conditional panel
             #                                #                                                     
             #                                #                                                     
             #                                # 
             #                                #                                                   
             #                                #                                                   
             #                                #                                                 ) #end of wellpanel
             #                                #                                          ), #end of col3
             #                                #                                          column(9,
             #                                #                                                # conditionalPanel(
             #                                #                                                #   condition = "input.impDepMapOrTable == 1 && input.selectionImports == 2",
             #                                #                                                   plotlyOutput("map_impdep")
             #                                #                                                # )
             #                                #                                                 
             #                                #                                          ) #end col 9
             #                                #                                          
             #                                #                                          
             #                                # 
             #                                #                          
             #                                #                          
             #                                #                          )) #end of col 12 and fluidRow
             #                                #          ), #end of conditional panel
             #                                #          
             #                                #          conditionalPanel(
             #                                #            condition = "input.fertilizerUseChoices == 1",
             #                                #            fluidRow(column(12,
             #                                #          
             #                                #          
             #                                #          
             #                                # 
             #                                #                          column(3,
             #                                #                                 
             #                                #                                 wellPanel(
             #                                #                                   # p(strong("How-to")),
             #                                #                                   # h4("How-to"),
             #                                #                                   
             #                                #                                   
             #                                #                                   # checkboxGroupInput(inputId = "OnlyImporters",
             #                                #                                   radioButtons(inputId = "OnlyImporters",
             #                                #                                                label = "Show:",
             #                                #                                                choices = list("Only net importers" = 2, "Only net exporters" = 3,"Both" = 1), 
             #                                #                                                selected = 1
             #                                #                                   ),
             #                                #                                   radioButtons(inputId = "DateImports", 
             #                                #                                                label = "Year:",
             #                                #                                                choices = list("2020" = 2020, "2010" = 2010),
             #                                #                                                selected = 2020
             #                                #                                   ),
             #                                #                                   
             #                                #                                   
             #                                #                                   radioButtons(inputId = "CountriesAllImports",
             #                                #                                                label = "Countries:",
             #                                #                                                choices = list("All countries" = 1, "Selection of countries" = 2), 
             #                                #                                                selected = 1
             #                                #                                   ), 
             #                                #                                 ) #end of well panel
             #                                #                                 
             #                                #                          ), #end of col 3
             #                                #                          column(9,
             #                                #                                 plotlyOutput("plotimports")
             #                                #                          )
             #                                #                          
             #                                #                          
             #                                #          )#end of column 12
             #                                #          )#end of fluidRow
             #                                #          )#end of conditional panel
             #                                # ),#end of tabPanel,
             #                             
             #                                # tabPanel("Fertilizer Use",
             #                                #          fluidRow(
             #                                #            HTML('<br/>')
             #                                #          ),
             #                                # 
             #                                #          )#end of fluidRow
             #                                # ),#end of tabPanel import dependency
             #                                
             # 
             #                                
             #                                # tabPanel("Fertilizer use",
             #                                #          fluidRow(column(12,
             #                                #                          HTML('<br/>'),
             #                                #                          wellPanel(
             #                                #                            align="center",
             #                                #                            
             #                                #                            # helpText("Select from the following:"),  
             #                                #                            radioButtons(inputId = "fertilizerUseChoices",
             #                                #                                         label = "Select from the following:",
             #                                #                                         choices = list("Agricultural use (in MT)" = 1, "Application rates (kg/ha)" = 2,"Fertilizer use and yields" = 3), 
             #                                #                                         selected = 1,
             #                                #                                         inline=T
             #                                #                            )
             #                                #                            
             #                                #                          ) #end of wellPanel
             #                                #          )), #end of col 12 and end of fluidRow
             #                                #          fluidRow(column(12,
             #                                #                          column(3, 
             #                                #                                 wellPanel(
             #                                #                                   radioButtons(inputId = "useYear",
             #                                #                                                label = "Select year:",
             #                                #                                                choices = list("2020" = "2020", "2010" = "2010", "2000" = "2000"), 
             #                                #                                                selected = "2020"
             #                                #                                   ),
             #                                #                                   radioButtons(inputId = "useNutrient",
             #                                #                                                label = "Select nutrient:",
             #                                #                                                choices = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"), 
             #                                #                                                selected = "Nitrogenous"
             #                                #                                   )
             #                                #                                   
             #                                #                                 ) #end of wellPanel
             #                                #                          ), #column end 3
             #                                #                          column(9,
             #                                #                                 ggiraphOutput("useBubble")
             #                                #                                 # highchartOutput("demographicsDiversity",
             #                                #                                 #                 width = "700px", height = "2000px")
             #                                #                                 ) #end of col 9
             #                                #          ) #end of column 12
             #                                #          )#end of fluidRow
             #                                # ),#end of tabPanel fertilizer use
             #                             
             #                             # tabPanel("Fertilizer production",
             #                             #          fluidRow(column(12,
             #                             #                          
             #                             #          )) #end of col12 and fluid row
             #                             # ), #end of panel on production
             #                             #                          
             #                             #                          
             #                             # tabPanel("Country profiles",
             #                             #          fluidRow(column(12,
             #                             #                          helpText("This would mirror the current version")               
             #                             #                          
             #                             #          )
             #                             #          )#end of fluidRow
             #                             # ),#end of tabPanel country profiles
             #                                
             #                                
             #                                
             #                                # tabPanel("Ukraine-Russia war exposure",
             #                                #          fluidRow(column(12,
             #                                #                          helpText("In addition to this tab, there is the
             #                                #                      tab on impact from trade restrictions on the 
             #                                #                      current version. We could either (1) add this as a sub tab to the right or (2) create
             #                                #                      a new high-level tab s or something like that, under which this tab and the
             #                                #                      one on restrictions would go.") ,              
             #                                #                          
             #                                #                          
             #                                #          )
             #                                #          )#end of fluidRow
             #                                # )#end of tabPanel Uk Russ
             #                                
             #                                
             #                                
             #                                
             #                                
             #                                
             #          )) ###end of marketcharacteristics tabset panel (countaining subtabs)
             #          
             #          
             #          
             #          
             # ),  ############END OF MARKET CHARACTERISTICS TAB
             
             
             #######START OF TRADE TAB#############
             tabPanel("Trade", fluid = TRUE,
                      column(12,tabsetPanel(id="trade_tabs",
                                            
                                            tabPanel("Main exporters and importers",
                                                     fluidRow(
                                                       column(12,
                                                              HTML('<br/>'),
                                                              
                                                              
                                                              
                                                              column(4,
                                                                     wellPanel(
                                                                       helpText("Import and export quantities are shown in metric tons nutrient equivalent."),
                                                                       radioButtons(inputId = "impExpQuantity_nutrient",
                                                                                    label = "Select:",
                                                                                    choices = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"), 
                                                                                    selected = "Nitrogenous"
                                                                       ),
                                                              
                                                                       selectInput(inputId = "impExpQuantity_year", 
                                                                                   label = "Year:", 
                                                                                   choices = yearBasedOnDataAvail, 
                                                                                   selected =  max(yearBasedOnDataAvail)
                                                                                   # choices = list("2020" = "2020", "2010" = "2010", "2000" = "2000"), 
                                                                                   # selected = "2020"
                                                                       )
                                                                     ) #end wellPanel
                                                              ), #endcol 4
                                                             column(8,
                                                                    fluidRow(
                                                              column(6,
                                                                     # helpText(
                                                                     #   "Import quantity",
                                                                     #   align="center"
                                                                     # ),
                                                                     shinycssloaders::withSpinner(
                                                                     ggiraphOutput("bubble_impQuantity")
                                                                     )
                                                              ),
                                                              column(6,
                                                                     # helpText(
                                                                     #   "Export quantity",
                                                                     #   align="center"
                                                                     # ),
                                                                     shinycssloaders::withSpinner(
                                                                     ggiraphOutput("bubble_expQuantity")
                                                                     )
                                                              )
                                                                    ),#end of fluidRow
                                                              
                                                              

                                                             fluidRow(
                                                                column(12,
                                                                       hr(style="margin-bottom:0;"),
                                                                       helpText("Source: FAOSTAT (with adjustments)", style = "font-size:12px;")
                                                                    )
                                                             )
                                                            )#end of col8
                                                       ) #end col12
                                                     )#end fluidRow
                                            ), #end of tab
                                            
                                            
                                                  
                                                  # conditionalPanel(
                                                  #   condition = "input.fertilizerUseChoices == 1",
                                            
                                        tabPanel("Net imports",       
                                             fluidRow(column(12,
                                                             HTML('<br/>'),
                                                             
                                                                   
                                                                    column(4,
                                                                           
                                                                           wellPanel(
                                                                             # p(strong("How-to")),
                                                                             # h4("How-to"),
                                                                             
                                                                             
                                                                             # checkboxGroupInput(inputId = "OnlyImporters",
                                                                             
                                                                             
                                                                             # radioButtons(inputId = "onlyImporters",
                                                                             #              label = "Show:",
                                                                             #              choices = list("Only net importers" = 2, "Only net exporters" = 3,"Both" = 1), 
                                                                             #              selected = 1
                                                                             # ),
                                                                             
                                                                            
                                                                            
                                                                             helpText("Net imports are shown and are the total imports minus total exports of a country. "),
                                                                             # radioButtons(inputId = "netImportsOutputNutrient", 
                                                                             #              label = "Select nutrients shown:",
                                                                             #              choices = list("Show by nutrient" = 1,"Show all nutrients" = 2),
                                                                             #              selected = 1
                                                                             # ),
                                                                             # hr(),
                                                                             
                                                                             radioButtons(inputId = "netImportsOutput",
                                                                                          label = "Select countries shown:",
                                                                                          choices = list("Top 10 net importers/exporters" = 1, "All countries (map output)" = 2),
                                                                                          selected = 1
                                                                             ),
                                                                           #  hr(),
                                                                           
                                                                           radioButtons(inputId = "nutrientImports",
                                                                                        label = "Select:",
                                                                                        choices = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"),
                                                                                        selected = "Nitrogenous"
                                                                           ),
                                                                           selectInput(inputId = "dateImports", 
                                                                                       label = "Year:",
                                                                                       choices =  yearBasedOnDataAvail,
                                                                                       selected =  max(yearBasedOnDataAvail)
                                                                                       # choices = list("2020" = "2020", "2010" = "2010"),
                                                                                       # selected = "2020"
                                                                           ),
                                                                             
                                                                              
                                                                             # radioButtons(inputId = "CountriesAllImports",
                                                                             #              label = "Countries:",
                                                                             #              choices = list("All countries" = 1, "Selection of countries" = 2), 
                                                                             #              selected = 1
                                                                             # ),
                                                                             # 
                                                                             # radioButtons(inputId = "netImportsOrderBy",
                                                                             #              label = "Countries:",
                                                                             #              choices = list("Nitrogenous" = 1, "Phosphate" = 2, "Potash" = 3), 
                                                                             #              selected = 1
                                                                             # ),
                                                                             
                                                                             # radioButtons(inputId = "inputOrder",
                                                                             #              label = "Order top importers by nutrient:",
                                                                             #              choices = list("Nitrogenous" = 1, "Phosphate" = 2, "Potash" = 3), 
                                                                             #              selected = 1
                                                                             # )
                                                                             
                                                                           ) #end of well panel
                                                                           
                                                                    ), #end of col 3
                                                                  #  https://stackoverflow.com/questions/68917679/plotly-how-to-properly-limit-the-row-shown-with-a-vertical-scroller
                                                             
                                                                  
                                                                  conditionalPanel(
                                                                    condition = "input.netImportsOutput == 2",
                                                                  column(8, 
                                                                         shinycssloaders::withSpinner(
                                                                          plotlyOutput("mapNetImports")
                                                                           )
                                                                  )
                                                                  ),#end of conditional panel
                                                                  
                                                                  conditionalPanel(
                                                                    condition = "input.netImportsOutput == 1",
                                                                    column(8,
                                                                           fluidRow(
                                                                             column(12,
                                                                           #   (divstyle="overflow-y:scroll;height: 200px;"
                                                                          # helpText("kdiddidi"),
                                                                         #  (div(style='overflow-y:scroll;height: 500px;',
                                                                                # (div(style='width:600px;overflow-x: scroll;height:1000px;overflow-y: scroll;',
                                                                         shinycssloaders::withSpinner(
                                                                           
                                                                                plotlyOutput("plotImportsByNutrient") 
                                                                                #))
                                                                         )
                                                                             ) 
                                                                          ),#end fluid Row
                                                                          fluidRow(
                                                                            column(12,
                                                                                   hr(style="margin-bottom:0;"),
                                                                                   helpText("Source: FAOSTAT (with adjustments)", style = "font-size:12px;")
                                                                                   
                                                                                   )
                                                                            
                                                                          )
                                                                    )
                                                                  )#end of conditional panel
                                                                    
                                                                    
                                                    )#end of column 12
                                                    )#end of fluidRow
                                                  ),#end of tab
                      # conditionalPanel(
                      #   condition = "input.fertilizerUseChoices == 3",
                      tabPanel("Import dependency",
                               
                               # fluidRow(column(12,
                               #                 HTML('<br/>'),
                               #                 wellPanel(
                               #                   align="center",
                               #                   
                               #                   # helpText("Select from the following:"),  
                               #                   radioButtons(inputId = "fertilizerUseChoices",
                               #                                label = "Select from the following:",
                               #                                choices = list("Net imports" = 1, "Import dependence ratio" = 2,"Main exporters and importers" = 3), 
                               #                                selected = 1,
                               #                                inline=T
                               #                   )
                               #                   
                               #                 ) #end of wellPanel
                               # )), #end of col 12 and end of fluidRow
                               # conditionalPanel(
                               #   condition = "input.fertilizerUseChoices == 2",
                               fluidRow(column(12,
                                               HTML('<br/>'),
                                               
                                               column(3, 
                                                      wellPanel(
                                                        # radioButtons(inputId = "selectionImports",
                                                        #              label = "Select:",
                                                        #              choices = list("Net imports" = 1, "Dependency ratio" = 2), 
                                                        #              selected = 2
                                                        # ),
                                                        
                                                        helpText("Ratio between imports and agricultural use (0 no imports to 1 domestic use fully supplied by imports)."),
                                                        radioButtons(inputId = "dep_nutrient",
                                                                     label = "Select:",
                                                                     choices = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate","Potash" ="Potash"), 
                                                                     selected = "Nitrogenous"
                                                        ),
                                                        
                                                        # radioButtons(inputId = "impDepMapOrTable",
                                                        #              label = "Select:",
                                                        #              choices = list("Show as map" = 1, "Show as table" = 2), 
                                                        #              selected = 1
                                                        # ),
                                                        
                                                        selectInput(inputId = "impDepMapYear", 
                                                                     label = "Year:",
                                                                    choices = yearBasedOnImpDependencyDataAvail, 
                                                                    selected =  max(yearBasedOnImpDependencyDataAvail)                                                                    
                                                                     # choices = list("2020" = 2020, "2010" = 2010),
                                                                     # selected = 2020
                                                        ),
                                                        
                                                        # hr(style = "border-top: 1px solid #000000;"
                                                        # ),
                                                        # checkboxInput(inputId = "impDepMoreInfo",
                                                        #               label = "Tab explaination",
                                                        #               value = FALSE
                                                        # ),
                                                        # conditionalPanel(
                                                        #   condition = "input.impDepMoreInfo == true",
                                                        #   helpText("The ratio ranges from 0, no imports, to 1, 
                                                        #                                domestic use fully supplied by imports.")
                                                        # ) #end of short conditional panel
                                                        
                                                        
                                                        
                                                        
                                                        
                                                      ) #end of wellpanel
                                               ), #end of col3
                                               column(9,
                                                      fluidRow(
                                                        column(12,
                                                      # conditionalPanel(
                                                      #   condition = "input.impDepMapOrTable == 1 && input.selectionImports == 2",
                                                      shinycssloaders::withSpinner(
                                                      plotlyOutput("map_impdep")
                                                      )
                                                      # )
                                                      )#end col 12
                                               ),#end fluidRow
                                               fluidRow(
                                                 column(12,
                                                        hr(style=list("margin-bottom:0;","margin-top:0;")),
                                                        helpText("Source: Computation based on FAOSTAT", style = "font-size:12px;")
                                                        )
                                               )
                                                      
                                               ) #end col 9
                                               
                                               
                                               
                                               
                                               
                               )) #end of col 12 and fluidRow
                               
                      ) #end of tab
                      # ), #end of conditional panel

                      ))), #end of trade tab
             
             #######START OF use TAB#############
             tabPanel("Use", fluid = TRUE,
                      column(12,tabsetPanel(id="use_tabs",
                                            
                                            tabPanel("Use (in MT)",
                                                     
                      
             
                              
                               # fluidRow(column(12,
                               #                 HTML('<br/>'),
                               #                 wellPanel(
                               #                   align="center",
                               #                   
                               #                   # helpText("Select from the following:"),  
                               #                   radioButtons(inputId = "useChoices",
                               #                                label = "Select from the following:",
                               #                                choices = list("Agricultural use (in MT)" = 1, "Application rates (kg/ha)" = 2,"Fertilizer usage and yields" = 3), 
                               #                                selected = 1,
                               #                                inline=T
                               #                   )
                               #                   
                               #                 ) #end of wellPanel
                               # )), #end of col 12 and end of fluidRow
                      # conditionalPanel(
                      #   condition = "input.useChoices == 1",
                               fluidRow(column(12,
                                               HTML('<br/>'),
                                               
                                               column(4, 
                                                      wellPanel(
                                                        
                                                        helpText("Agricultural use of fertilizer in metric tons nutrient equivalent."),
                                                        selectInput(inputId = "useYear",
                                                                     label = "Select year:",
                                                                    choices = yearBasedOnUseDataAvail, 
                                                                    selected = max(yearBasedOnUseDataAvail)                                                                     # choices = list("2020" = "2020", "2010" = "2010", "2000" = "2000"), 
                                                                     # selected = "2020"
                                                        ),
                                                        radioButtons(inputId = "useNutrient",
                                                                     label = "Select nutrient:",
                                                                     choices = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"), 
                                                                     selected = "Nitrogenous"
                                                        )
                                                        
                                                      ) #end of wellPanel
                                               ), #column end 3
                                               column(8,
                                                      fluidRow(
                                                        column(12,
                                                      ggiraphOutput("useBubble")
                                                      # highchartOutput("demographicsDiversity",
                                                      #                 width = "700px", height = "2000px")
                                                        ) #end of column
                                                      ),#end of fluidRow
                                                      fluidRow(
                                                        column(12,
                                                              hr(),
                                                              helpText("Source: FAOSTAT")
                                                               )
                                                      )
                                                      ) #end of col 8
                               ) #end of column 12
                               )#end of fluidRow
                      ),
                      #end of subtab
                      # conditionalPanel(
                      #   condition = "input.useChoices == 2",
                      tabPanel("Application rates",
                               
                        fluidRow(column(12,
                                        HTML('<br/>'),
                                        
                                        column(3, 
                                               wellPanel(
                                                 helpText("Application rates are the total kilograms (kg) of nutrient by hectare (ha) of cropland."),
                                                 
                                                 selectInput(inputId = "yearApplicationRt",
                                                              label = "Select year:",
                                                             choices = yearBasedOnUseDataAvail, 
                                                             selected = max(yearBasedOnUseDataAvail)                                                              # choices = list("2020" = "2020", "2010" = "2010", "2000" = "2000"), 
                                                              # selected = "2020"
                                               
                                               ),
                                               radioButtons(inputId = "nutrientApplicationRt",
                                                            label = "Select nutrient:",
                                                            choices = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"), 
                                                            selected = "Nitrogenous"
                                               )
                                               
                                               )#end of wellPanel
                                        ), #column end 3
                                        column(9,
                                               fluidRow(
                                                 column(12,
                                               shinycssloaders::withSpinner(
                                                 
                                               plotlyOutput("applicationRateMap")
                                               )
                                               # highchartOutput("demographicsDiversity",
                                               #                 width = "700px", height = "2000px")
                                                 )
                                          
                                               ),#end of fluidRow
                                               fluidRow(
                                                 column(12,
                                                        hr(),
                                                        helpText("Source: computations based on FAOSTAT")
                                                        )
                                               )
                                                ) #end of col 9
                        ) #end of column 12
                        )#end of fluidRow
                      ), #end of subtab
                      
                      # conditionalPanel(
                      #   condition = "input.useChoices == 3",
                      tabPanel("Fertilizer usage and yields",
                        fluidRow(column(12,
                                        HTML('<br/>'),
                                        
                                        column(3, 
                                               wellPanel(
                                                 helpText("Relation between application rates and cereal yields"),
                                                 selectInput(inputId = "yearUseYields",
                                                              label = "Select year:",
                                                             choices = yearBasedOnUseDataAvailYields, 
                                                             selected = max(yearBasedOnUseDataAvailYields)
                                                             #Note maybe update these choices to years with overlap                                                              # choices = list("2020" = "2020", "2010" = "2010", "2000" = "2000"), 
                                                              # selected = "2020"
                                                              
                                                 ),
                                                 checkboxGroupInput(inputId = "nutrientUseYields",
                                                              label = "Select nutrient:",
                                                              choices = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"), 
                                                              selected = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash")
                                                 ),
                                                 
                                                 hr(style = "border-top: 1px solid #000000;"),
                                                 
                                                 radioButtons(inputId = "useYieldLine",
                                                              label = "Output shown",
                                                              choices = list("Include trendline" = 1, "No trendline" = 2), 
                                                              selected = 2
                                                 )
                                                 
                                                 
                                                 
                                                 
                                                 
                                               )#end of wellPanel
                                        ), #column end 3
                                        conditionalPanel(
                                          condition = "input.useYieldLine == 1",
                                          column(9,
                                                 fluidRow(
                                                   column(12,
                                                 shinycssloaders::withSpinner(
                                                   
                                                 plotlyOutput("useYields")
                                                 )
                                                   )
                                          ), #end fluidRow
                                          fluidRow(
                                            column(12,
                                                   hr(),
                                                   helpText("Source: computation based on FAOSTAT (fertilizer use, cropland, and yields)")
                                                   )
                                            
                                          )
                                          )
                                          ),
                                        
                                        conditionalPanel(
                                          condition = "input.useYieldLine == 2",
                                          column(9,
                                                 fluidRow(
                                                   column(12,
                                                 shinycssloaders::withSpinner(
                                                   
                                                 plotlyOutput("useYieldsNoLine")
                                                 )
                                                 )
                                                 ),#end of fluidRow
                                                 fluidRow(
                                                   column(12,
                                                          hr(),
                                                          helpText("Source: computation based on FAOSTAT (fertilizer use, cropland, and yields)")
                                                          )
                                                 )
                                                 )#end of col 9
                                        ),
                                        # column(9,
                                        #        plotlyOutput("useYields")
                                        #        # highchartOutput("demographicsDiversity",
                                        #        #                 width = "700px", height = "2000px")
                                        # ) #end of col 9
                        ) #end of column 12
                        )#end of fluidRow
                      ) #end of subtab

             ))), #end of use tab
             
             #########start of production tab###########
             
             tabPanel("Production", fluid = TRUE,
                      fluidRow(column(12,
                                      HTML('<br/>'),
                                      
                                      column(3, 
                                             wellPanel(

                                               helpText("This tab shows fertilizer production in metric tons by country."),
                                               # hr(style = "border-top: 1px solid #000000;"),

                                               selectInput(inputId = "productionYear",
                                                            label = "Select year:",
                                                           choices = yearBasedOnPRoductionDataAvail, 
                                                           selected = max(yearBasedOnPRoductionDataAvail)                                                            # choices = list("2020" = "2020", "2010" = "2010", "2000" = "2000"), 
                                                            # selected = "2020"
                                               ),
                                               radioButtons(inputId = "productionNutrient",
                                                            label = "Select nutrient:",
                                                            choices = list("Nitrogenous" = "Nitrogenous", "Phosphate" = "Phosphate", "Potash" = "Potash"), 
                                                            selected = "Nitrogenous"
                                               ),
                                               hr(style = "border-top: 1px solid #000000;"),
                                               radioButtons(inputId = "productionMapOrBubble",
                                                            label = "Output shown:",
                                                            choices = list("Map" = 1, "Bubble chart" = 2),
                                                            selected = 2
                                               )
                                               
                                             ) #end of wellPanel
                                      ), #column end 3
                                      conditionalPanel(
                                        condition = "input.productionMapOrBubble == 2",
                                        column(9,
                                               fluidRow(
                                                 column(12,
                                               ggiraphOutput("productionBubble")
                                               )
                                        ),#fluidRow
                                        fluidRow(
                                          column(12,
                                                 hr(),
                                                 helpText("Source: FAOSTAT")
                                                 )
                                        )
                                        )#col9
                                        ),#end of conditional panel
                                      conditionalPanel(
                                        condition = "input.productionMapOrBubble == 1",
                                        column(9,
                                               fluidRow(
                                                 column(12,
                                               
                                              # helpText("Coming soon")
                                               #ggiraphOutput("productionBubble")
                                              shinycssloaders::withSpinner(
                                                
                                              plotlyOutput("productionMap")
                                              )
                                        )
                                        ),#fluidRow
                                        fluidRow(
                                          column(12,
                                                 hr(),
                                                 helpText("Source: FAOSTAT")
                                          )
                                        )
                                        
                                        
                                        )#col9
                                      ),

                      ) #end of column 12
                      )#end of fluidRow
             )#, #end of production tab
             
             # tabPanel("Global Market Overview", fluid = TRUE,
             #          fluidRow(column(12,
             #                          
             #                     
             #                                                   HTML('<br/>'),
             #                                                   wellPanel(
             #                                                     
             #                                                     
             #                                                     fluidRow(
             #                                                       
             #                                                       column(12, 
             #                                                              
             #                                                              # tags$div(
             #                                                              # "Monthly assessments of fertilizer market developments 
             #                                                              #          were prepared for the",
             #                                                              # tags$a(href="https://www.amis-outlook.org/amis-monitoring#.ZC1FFPZByM8", 
             #                                                              #        "AMIS Market Monitor")
             #                                                              # ),
             #                                                              #https://community.rstudio.com/t/hyperlink-portion-of-text-in-shiny-server-text-block/67328/2
             #                                                              tags$h3(""),
             #                                                              tags$div(
             #                                                                "Monthly assessments of fertilizer market developments were prepared for the ",
             #                                                                tags$a(href="https://www.amis-outlook.org/amis-monitoring#.ZC1FFPZByM8", 
             #                                                                       "AMIS Market Monitor", target="_blank")
             #                                                              )
             #                                                              ),
             #                                                              
             #                                                              
             #                                                                                                     # helpText("Monthly assessments of fertilizer market developments 
             #                                                              #          were prepared for the AMIS Market Monitor")),
             # 
             #                                                              column(12,
             #                                                              selectInput("marketOverviewMonth", h3(""), 
             #                                                                          choices = list("Aug 2024" = 29, "Jun. 2024" = 28,"May 2024" = 27, "Apr. 2024" = 26, "Mar. 2024" = 25,"Feb. 2024" = 24,"Jan. 2024" = 23, "Nov. 2023" = 22,"Oct. 2023" = 21, "Sept. 2023" = 20, "Aug. 2023" = 19,"Jun. 2023" =18, "May 2023"=17, "Apr. 2023"=16, "Mar. 2023"=15,"Feb. 2023" = 1, "Jan. 2023"= 2, "Nov. 2022" =  4,
             #                                                                                         "Oct. 2022" =  5, "Sept. 2022" = 6, "Aug. 2022" = 7, "June 2022" = 9, 
             #                                                                                         "May 2022" = 10, "Apr. 2022" = 11, "Mar. 2022" = 12, "Feb. 2022" =  13, "Jan. 2022" = 14), 
             #                                                                          selected = 29)),
             #                                                       
             #                                                       hr(),
             #                                                     ),
             #                                                     fluidRow(
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 1",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesFeb23.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 2",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesJan23.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 4",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesNov22.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 5",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesOct22.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 6",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesSept22.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 7",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesAug22.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                     
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 9",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesJune22.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 10",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesMay22.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 11",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesApr22.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 12",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesMar22.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 13",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesFeb22.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 14",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesJan22.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 15",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesMar23.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 16",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesApr23.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 17",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesMay23.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 18",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesJune23.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 19",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesAug23.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 20",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesSept23.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 21",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesOct23.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 22",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesNov23.html")
             #                                                         )
             #                                                       ),
             #                                                       
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 23",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesJan24.html")
             #                                                         )
             #                                                       ),
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 24",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesFeb24.html")
             #                                                         )
             #                                                       ),                                                                   
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 25",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesMar24.html")
             #                                                         )
             #                                                       ),
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 26",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesApr24.html")
             #                                                         )
             #                                                       ),
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 27",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesMay24.html")
             #                                                         )
             #                                                       ),
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 28",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarrativesJun24.html")
             #                                                         )
             #                                                       ),
             #                                                       conditionalPanel(
             #                                                         condition = "input.marketOverviewMonth == 29",
             #                                                         column(6,
             #                                                                includeHTML("FertilizerNarratives_2024_08.html")
             #                                                         )
             #                                                       ),
             #                                                    
             #                                                       # column(6,
             #                                                       #        plotlyOutput("plotMonth")
             #                                                       # ), 
             #                                                       # fluidRow(
             #                                                       #        helpText("(For more prices, visit the Prices tab.)")
             #                                                       #        )
             #                                                       
             #                                                  
             #                                                       
             #                                                     )
             #                                                     
             #                                                     
             #                                                     
             #                                                   ),#end of wellpanel
             #                          helpText("Last update: September 10, 2024 / Next update: October 10, 2024")
             #                          
             #                                                   
             #                                                   # fluidRow(
             #                                                   #   column(12,
             #                                                   #          plotOutput("plot1"))
             #                                                   # )
             #                                                   # column(6,
             #                                                   #        #ggvisOutput("plot1")
             #                                                   #        # plotOutput("plot1")
             #                                                   #        plotlyOutput("plot3")
             #                                                   # )
             #                                                   
             #                                                   
             #                                   # end narrative tab
             #                          ##
             #                          
             #          ))
             # )
             
             
             
             
             
             
             #useShinyalert()
  )
)





server <- function(input, output) {
  
  
  
  shinyalert(
    title = "Welcome to the Fertilizer Dashboard for the Asia and the Pacific Region",
    text = "The top bar of this dashboard allows you to explore the various aspects of fertilizer
    markets: (global) fertilizer prices, fertilizer trade, fertilizer use, and fertilizer production. For a monthly 
    summary of the latest market developments (global), click on the market overview tab.",
    size = "m", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
   # type = "",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
   # timer = 0,
    imageUrl = "",
   # animation = FALSE
  )
  
  # observeEvent(input$link_to_tabpanel_prices, {
  #   updateTabItems(session, "panels", "Prices")
  # })
  
  #Shiny alert on fertilizer product description
  observeEvent(input$howToTabPrices, {
  shinyalert(html = TRUE, 
             text = paste0(
               "<p><b>Inputs (left column)</b> <br>",
               " Adjust the left column inputs to adjust", 
               " the visualization to the right. Date range",
               " is adjusted using the sliders or on the",
               " visualization itself. Select/drop fertilizers",
               " (all fertilizer products are selected by default)",
               " and indicate whether you would rather see the", 
               " actual prices or the indexed values (2019=100).",
               " The information icons throughout provide more context", 
               " how-tos.<br>",
               "<hr>",
               " <b> Outputs (right column) </b> <br>",
               " The visualization on the right", 
               " changes based on your selections. Note that you can", 
               " adjust what is shown on the visualization itself.",
               " To do this, click the",
               " fertilizer name directly in the ",
               " visualization legend. Double click to",
               " show only that fertilizer. Single click",
               " to drop that fertilizer from the plot", 
               " or to add it back if it was previously",
               " dropped. You can also zoom in and out by",
               " clicking and dragging on the chart.</p>"),
             #text =includeHTML("FertilizerNarratives.html", ),
             closeOnEsc = TRUE,
             closeOnClickOutside = TRUE,
             size = "m"
  )
  })
  # includeHTML("FertilizerNarratives.html")
  
  
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
        validate(need(input$Fert_selected, "Please select a fertilizer type"))
        
        x <- p_fert_long %>%
          filter(Product %in% input$Fert_selected) %>%
          filter(variable == "PriceMT") %>%
        # filter(grepl('Average', name)) %>%
          ggplot(aes(x=date, y=value, colour = Product)) +
          geom_line(aes(name="Fertilizer")) +
          scale_color_manual_fertizerProducts()+
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
         # layout(xaxis = list(rangeslider = list()))
        
        
        
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
        
        validate(need(input$Fert_selected, "Please select a fertilizer type"))
        
        
        x <- p_fert_long %>%
          filter(Product %in% input$Fert_selected) %>%
          filter(variable=="Index") %>%
          ggplot(aes(x=date, y=value, colour = Product)) +
          geom_line(aes(name="Fertilizer")) +
          scale_color_manual_fertizerProducts() +
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
  
  
  ######### WB PRICES ##############
  ####### version WB prices Nov 24 ####################
  output$plot1 <- renderPlotly({
    WB_fertilizer_prices %>% 
      filter(Date >= min(input$Date_FertPrices) & Date <= max(input$Date_FertPrices)) %>%
      #filter(Date >= min(input$Date_FertPrices) & Date<= max(input$Date_FertPrices)) %>%
      filter(Commodity %in% input$Fert_selected) %>%
      plot_ly(x = ~Date, 
              y = ~Value, 
              color = ~Commodity, 
              type = 'scatter', 
              mode = 'lines') %>%
      layout(title = "Fertilizer Prices Over Time",
             xaxis = list(title = ""),
             yaxis = list(title = WB_fertilizer_prices %>% select(Unit) %>% slice(1) %>% pull()),  # Set y-axis label dynamically
             # yaxis = list(title = "Value"),
             legend = list(title = list(text = "Fertilizer")),
             annotations = list(
               x = 1,  
               y = -.1,
               xref = "paper",
               yref = "paper",
               showarrow = FALSE,
               text = "Source: World Bank Pink Sheet",
               xanchor = "right",
               yanchor = "auto",
               font = list(size = 8, color = "gray")
             )
             
      ) 
    
  })
  
  
  ####### version WB nat gas prices Nov 24 ####################
  output$plotNatGas <- renderPlotly({
    WB_NatGas_prices %>% 
      filter(Date >= min(input$Date_NatGasPrices) & Date <= max(input$Date_NatGasPrices)) %>%
      #filter(Date >= min(input$Date_FertPrices) & Date<= max(input$Date_FertPrices)) %>%
      #filter(Commodity %in% input$Fert_selected) %>%
      plot_ly(x = ~Date, 
              y = ~Value, 
              color = ~Commodity, 
              type = 'scatter', 
              mode = 'lines') %>%
      layout(title = "Natural Gas Prices Over Time",
             xaxis = list(title = ""),
             yaxis = list(title = WB_NatGas_prices %>% select(Unit) %>% slice(1) %>% pull()),  # Set y-axis label dynamically
             # yaxis = list(title = "Value"),
             legend = list(title = list(text = "Natural Gas Market")),
             annotations = list(
               x = 1,  
               y = -.1,
               xref = "paper",
               yref = "paper",
               showarrow = FALSE,
               text = "Source: World Bank Pink Sheet",
               xanchor = "right",
               yanchor = "auto",
               font = list(size = 8, color = "gray")
             )
      )
    
  })
  
  ####### version WB nat gas and fert prices together Nov 24 ####################
  output$plotNatGasFert <- renderPlotly({
    
    plot_ly() %>%
      add_trace(data =   WB_pricesIndexed %>% 
                  filter(Date >= min(input$Date_NatGas_fert_Prices) & Date <= max(input$Date_NatGas_fert_Prices)) %>%
                  filter(Commodity %in% input$FertNatGas_selected) %>%
                  filter(Commodity %in% c("DAP", "TSP", "Urea", "Potassium chloride")), 
                x = ~Date, 
                y = ~Value, 
                color = ~Commodity, 
                type = 'scatter', 
                mode = 'lines'#, 
                #name = "Fertilizers"
      ) %>%
      # Overlay layer with natural gas prices in black
      add_trace(data = WB_pricesIndexed %>%
                  filter(Date >= min(input$Date_NatGas_fert_Prices) & Date <= max(input$Date_NatGas_fert_Prices)) %>%
                  filter(Commodity %in% input$FertNatGas_selected) %>%
                  filter(Commodity %in% c("Natural gas, US"#,"Natural gas, Europe" 
                  )),
                x = ~Date,
                y = ~Value,
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'black'),  # Set line color to black for natural gas
                name = "Natural gas, US"
      ) %>%
      add_trace(data = WB_pricesIndexed %>%
                  filter(Date >= min(input$Date_NatGas_fert_Prices) & Date <= max(input$Date_NatGas_fert_Prices)) %>%
                  filter(Commodity %in% input$FertNatGas_selected) %>%
                  filter(Commodity %in% c("Natural gas, Europe" #,"Natural gas, Europe"
                  )),
                x = ~Date,
                y = ~Value,
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'black', dash = 'dot'),  # Set line color to black and make it dotted
                name = "Natural gas, EU"
      ) %>%
      layout(title = "Indexed Fertilizer and Natural Gas Prices",
             xaxis = list(title = ""),
             yaxis = list(title = WB_pricesIndexed %>% select(Unit) %>% slice(1) %>% pull()),  # Set y-axis label dynamically
             # yaxis = list(title = "Value"),
             # legend = list(title = list(text = "Natural Gas Market"))
             annotations = list(
               x = 1,  
               y = -.1,
               xref = "paper",
               yref = "paper",
               showarrow = FALSE,
               text = "Source: World Bank Pink Sheet",
               xanchor = "right",
               yanchor = "auto",
               font = list(size = 8, color = "gray")
               
             )
      )
    
  })
  
  ############Nutrient - indexed/not indexed##########
  


  output$plot2 <- renderPlotly({
    
    validate(need(input$Nut_selected, "Please select a nutrient"))
    
    
    x_nutrient <- p_nutrient_long %>%
      filter(Product %in% input$Nut_selected) %>%
      #filter(variable == "PriceMT") %>%
      ggplot(aes(x=date, y=value, colour = Product)) +
      geom_line(aes(name="Nutrient")) +
      scale_color_manual_fertizerNutrientCategories()+
      xlim(min(input$Date2), max(input$Date2)) +
      labs(x = "",y="Price (indexed values 2019=100)") 
    
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
    
    validate(need(input$Natgas_selected, "Please select a natural gas market"))
    
    
    x_natgas <- p_natgas_long %>%
      filter(Product %in% input$Natgas_selected) %>%
      filter(variable=="USDpermmBTU") %>%
      ggplot(aes(x=date, y=value, colour = Product)) +
      geom_line(aes(name="Natural_Gas_Prices")) +
      scale_color_manual_natGas()+
      xlim(min(input$Datenatgas), max(input$Datenatgas)) +
      labs(x = "",y="Price (monthly average; USD per mmBTU)") 
    
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
        
        validate(need(input$Natgas_selected, "Please select a natural gas market"))
        
        
        x_natgas <- p_natgas_long %>%
          filter(Product == input$Natgas_selected) %>%
          filter(variable=="Index") %>%
          ggplot(aes(x=date, y=value, colour = Product)) +
          geom_line(aes(name="Natural_Gas_Prices")) +
          scale_color_manual_natGas()+
          xlim(min(input$Datenatgas), max(input$Datenatgas)) +
          labs(x = "",y="Price (indexed values 2019=100)") 
        
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
  
  
  #if user selects to compare input/output prices
  observeEvent(input$buttonInputsOUtputs,{
    if (input$buttonInputsOUtputs == 2) {
      
      
      output$plotnatgasFertilizer <- renderPlotly({
        
        validate(need(input$inputOutput_products, "Please select a nutrient/natural gas market"))
        
        
        natGasInputOutputPrices <- prices %>%
          filter(variable =="Index") %>%
          filter(Product %in% input$inputOutput_products) %>%

          ggplot(aes(x=date, y=value, colour = Product)) +
          geom_line(aes(name=Product)) +
          scale_color_manual_natGasFertilizer()+
          xlim(min(input$dateInputOutput), max(input$dateInputOutput)) +
          labs(x = "",y="Indexed value (2019 = 100)")
        
        l <- list(
          font = list(
            # family = "sans-serif",
            size = 10,
            color = "#000"
          )
        )
        
        ggplotly(natGasInputOutputPrices, dynamicTicks = T) %>%
          layout(legend = l) %>%
          layout(legend=list(title=list(text='')))%>%
          layout(legend = list(orientation = 'h'))
        
      })
      
      
      
    } #end of if
    
  })#end of observe event
  
  
  #############ENd of nat gas tab
  ##start of market overview
 
      #############
    dateMarketOverview <- reactive({
      
      
      req(input$marketOverviewMonth)
      if (input$marketOverviewMonth==1 ){
      today <- as.Date("2023-03-01", "%Y-%m-%d")
      }
      if (input$marketOverviewMonth==2 ){
        today <- as.Date("2023-02-01", "%Y-%m-%d")
      }
      if (input$marketOverviewMonth==4 ){
        today <- as.Date("2022-12-01", "%Y-%m-%d")
      }
      else {
        today<-as.Date("2022-12-01", "%Y-%m-%d")
    
        
        
        
      }
    })
      
      #######################end 
  
  output$plotMonth <- renderPlotly({
    
  #  validate(need(input$Fert_selected, "Please select a fertilizer type"))
    req(input$marketOverviewMonth)
    if (input$marketOverviewMonth==1 ){
      maxDate <- as.Date("2023-03-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
    }
    if (input$marketOverviewMonth==2 ){
      maxDate <- as.Date("2023-02-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    if (input$marketOverviewMonth==4 ){
      maxDate <- as.Date("2022-12-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    
    if (input$marketOverviewMonth==5 ){
      maxDate <- as.Date("2022-11-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    if (input$marketOverviewMonth==6 ){
      maxDate <- as.Date("2022-10-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    if (input$marketOverviewMonth==7 ){
      maxDate <- as.Date("2022-09-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    if (input$marketOverviewMonth==8 ){
      maxDate <- as.Date("2022-08-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    if (input$marketOverviewMonth==9 ){
      maxDate <- as.Date("2022-07-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    if (input$marketOverviewMonth==10 ){
      maxDate <- as.Date("2022-06-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    if (input$marketOverviewMonth==11 ){
      maxDate <- as.Date("2022-05-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    if (input$marketOverviewMonth==12 ){
      maxDate <- as.Date("2022-04-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    if (input$marketOverviewMonth==13 ){
      maxDate <- as.Date("2022-03-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    if (input$marketOverviewMonth==14 ){
      maxDate <- as.Date("2022-02-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    if (input$marketOverviewMonth==15 ){
      maxDate <- as.Date("2023-04-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    
    if (input$marketOverviewMonth==16 ){
      maxDate <- as.Date("2023-05-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    
    if (input$marketOverviewMonth==17 ){
      maxDate <- as.Date("2023-06-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    
    if (input$marketOverviewMonth==18 ){
      maxDate <- as.Date("2023-07-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    
    if (input$marketOverviewMonth==19 ){
      maxDate <- as.Date("2023-09-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    
    if (input$marketOverviewMonth==20 ){
      maxDate <- as.Date("2023-10-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    
    if (input$marketOverviewMonth==21 ){
      maxDate <- as.Date("2023-11-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    
    if (input$marketOverviewMonth==22 ){
      maxDate <- as.Date("2023-12-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    
    if (input$marketOverviewMonth==23 ){
      maxDate <- as.Date("2024-02-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    
    if (input$marketOverviewMonth==24 ){
      maxDate <- as.Date("2024-03-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    
    if (input$marketOverviewMonth==25 ){
      maxDate <- as.Date("2024-04-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
    }
      
      if (input$marketOverviewMonth==26 ){
        maxDate <- as.Date("2024-05-01", "%Y-%m-%d")
        minDate <- maxDate %m+% months(-3)
      }
        
        if (input$marketOverviewMonth==27 ){
          maxDate <- as.Date("2024-06-01", "%Y-%m-%d")
          minDate <- maxDate %m+% months(-3)
        }
          
          if (input$marketOverviewMonth==28 ){
            maxDate <- as.Date("2024-07-01", "%Y-%m-%d")
            minDate <- maxDate %m+% months(-3)
      
          }
    if (input$marketOverviewMonth==29 ){
      maxDate <- as.Date("2024-10-01", "%Y-%m-%d")
      minDate <- maxDate %m+% months(-3)
      
    }
    else {
      
    }
      
    priceNarrative <- p_fert_long %>%
     # filter(Product %in% input$Fert_selected) %>%
      filter(variable == "PriceMT") %>%
     # filter(date > (maxDate %m+% months(-3)) ) %>%
      ggplot(aes(x=date, y=value, colour = Product)) +
      geom_line(aes(name="Fertilizer")) +
      scale_color_manual_fertizerProducts() +
      xlim(min(minDate), max(maxDate)) +
      labs(x = "",y="Price MT")  +
      theme(legend.position="bottom", plot.margin=unit(c(1,1,1,1),"cm") )
      
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
    
    
    ggplotly(priceNarrative, dynamicTicks = T) %>%
     # layout(legend = l) %>%
      layout(legend=list(title=list(text='')))%>%
      layout(legend = list(orientation = 'h'))
    
    # layout(list(x = 0.08, y = 0.97)
    #layout(showlegend = FALSE)
    #  layout(legend = "dfdfd")
    # 
    
    #legend(x, "lopleft")
    
    
    
    
    
    
  })
  
  ##end of market overview
  
  ##########SECTION: Beginning of net imports
  
  #https://community.plotly.com/t/add-scrolling-options-to-plots/9493/5
  # 
  # netImportsReactive <- reactive({
  #   
  #   if(inputOrder ==1) {
  #     netImports <- usebroad %>%    
  #       group_by(Year, Nutrient) %>%
  #       # arrange(desc(Year)) %>%
  #       arrange(desc(value))
  #       
  #     
  #   }
  #   
  #})
  # netImportsN <- reactive({
  #     usebroad %>%
  #       filter(variable == "Net Imports") %>%    
  #       filter(Year==input$dateImports) %>%
  #       #  filter(Nutrient == input$nutrientImports) %>%
  #       filter(Nutrient == "Nitrogenous") %>%
  #       # filter(Year==2020)%>%
  #       filter(`Country_Name` != "World")%>%
  #       filter(`Country_Name` != "European Union") %>%
  #       mutate(Net = case_when(
  #         value > 0  ~ "Net importer",
  #         value < 0 ~ "Net exporter",
  #         value ==0 ~"Balanced"
  #         #   value == 0 ~ "Balanced",
  #         # .default = 999
  #       )) %>%
  #       group_by(Year, Nutrient) %>%
  #       # arrange(desc(Year)) %>%
  #      arrange(desc(value))
  #   #, .by_group = TRUE)
  #     
  #     
  #    #  slice(1:15)
  #     
  #  
  #   
  # })
  # 
  # netImportsP <- reactive({
  #   usebroad %>%
  #     filter(variable == "Net Imports") %>%
  #     filter(Nutrient == "Phosphate") %>%
  #     filter(Year==input$dateImports) %>%
  #     filter(`Country_Name` != "World")%>%
  #     filter(`Country_Name` != "European Union") 
  # 
  # 
  #     # # filter(Nutrient == "Nitrogenous") %>%
  #     # # filter(Year==2020)%>%
  #     # filter(`Country_Name` != "World")%>%
  #     # filter(`Country_Name` != "European Union") %>%
  #     # mutate(Net = case_when(
  #     #   value > 0  ~ "Net importer",
  #     #   value < 0 ~ "Net exporter",
  #     #   value ==0 ~"Balanced"
  #     #   #   value == 0 ~ "Balanced",
  #     #   # .default = 999
  #     # )) %>%
  #     # group_by(Year, Nutrient) %>%
  #     # # arrange(desc(Year)) %>%
  #     # arrange(desc(value), .by_group = TRUE) %>%
  #     # #slice(1:15)
  # })
  # 
  # netImportsK <- reactive({
  #   usebroad %>%
  #     filter(variable == "Net Imports")  %>%
  #     filter(Nutrient == "Potash") %>%
  #     filter(Year==input$dateImports) %>%
  #     filter(`Country_Name` != "World")%>%
  #     filter(`Country_Name` != "European Union") 
  #     # # filter(Nutrient == "Nitrogenous") %>%
  #     # # filter(Year==2020)%>%
  #     # filter(`Country_Name` != "World")%>%
  #     # filter(`Country_Name` != "European Union") %>%
  #     # mutate(Net = case_when(
  #     #   value > 0  ~ "Net importer",
  #     #   value < 0 ~ "Net exporter",
  #     #   value ==0 ~"Balanced"
  #     #   #   value == 0 ~ "Balanced",
  #     #   # .default = 999
  #     # )) %>%
  #     # group_by(Year, Nutrient) %>%
  #     # # arrange(desc(Year)) %>%
  #     # arrange(desc(value), .by_group = TRUE) %>%
  #     # filter(Year==2020) %>%
  #     # filter(Nutrient == "Potash")
  #     # #slice(1:15)
  # })
  
  
  

  # netImportsJoined <- reactive({
  #   netImportsNJoined <- netImportsN()
  #   netImportsPJoined <- netImportsP()
  #   netImportsKJoined <- netImportsK()
  #
  #   netImportsNJoined <- netImportsNJoined %>%
  #     left_join(netImportsPJoined,.)%>%
  #     left_join(netImportsKJoined,.)

  #})
    
  
  ###
  
  # netImports <- reactive({
  #   usebroad %>%
  #     filter(variable == "Net Imports") %>%
  #     filter(Nutrient == input$nutrientImports) %>%
  #     filter(Year==input$dateImports)%>%
  #     filter(`Country_Name` != "World")%>%
  #     filter(`Country_Name` != "European Union") %>%
  #     mutate(Net = case_when(
  #       value > 0  ~ "Net importer",
  #       value < 0 ~ "Net exporter",
  #       value ==0 ~"Balanced" ))
  #       #   value == 0 ~ "Balanced",
  #       # .default = 999
  # 
  # })
  
  ##### end old version


    
    
    
    # add in 
    
    # observeEvent(input$inputOrder,{
    #   
    #   if (input$inputOrder == 1){
    #     netImportsN_plotly <- netImportsN() %>%
    #       group_by(Year, Nutrient) %>%
    #       # arrange(desc(Year)) %>%
    #       arrange(desc(value))
    #     netImportsP_plotly <- netImportsP()
    #     netImportsK_plotly <- netImportsK()
    #   }
    #   
    #   
    #   if (input$inputOrder == 2){
    #     netImportsP_plotly <- netImportsP() %>%
    #       group_by(Year, Nutrient) %>%
    #       # arrange(desc(Year)) %>%
    #       arrange(desc(value))
    #     netImportsN_plotly <- netImportsN()
    #     netImportsK_plotly <- netImportsK()
    #   }
    #   
    #   if (input$inputOrder == 3){
    #     netImportsK_plotly <- netImportsK() %>%
    #       group_by(Year, Nutrient) %>%
    #       # arrange(desc(Year)) %>%
    #       arrange(desc(value))
    #     netImportsP_plotly <- netImportsP()
    #     netImportsN_plotly <- netImportsN()
    #   }
#   output$plotImportsAllNutrients <- renderPlotly({
#     
#       
#  netImportsN_plotly <- netImportsN()
#  netImportsP_plotly <- netImportsP()
#  netImportsK_plotly <- netImportsK()
#         
#         
#   #N
#   plotImportsN <-   plot_ly(netImportsN_plotly,
#           y = ~Country_Name,
#           x = ~value ,
#           type = "bar",
#        #   tooltip = value, Country_Code,
#           height = 3000
#   )
#   #%>% layout(yaxis = list(categoryorder = "total descending"))
#   # %>%
#   #   layout(
#   #     title = "Plot",
#   #    # autosize = FALSE,
#   #     showlegend = FALSE
#   #   ) %>%
#   #   layout(yaxis = list(categoryorder = "total ascending"))
#   #
#   # P
#   plotImportsP <-   plot_ly(netImportsP_plotly,
#                                       y = ~Country_Name,
#                                       x = ~value ,
#                                       type = "bar",
#                                       height = 3000
#   )
#    # layout(yaxis = list(categoryorder = "total ascending"))
# 
#   #K
#   plotImportsK <-   plot_ly(netImportsK_plotly,
#                                       y = ~Country_Name,
#                                       x = ~value ,
#                                       type = "bar",
#                                       height = 3000
#   )
#     #layout(yaxis = list(categoryorder = "total ascending"))
# 
# 
#   plotImportsCombined <- subplot(plotImportsN, plotImportsP, plotImportsK, nrows = 1, shareY = TRUE, shareX = TRUE) %>%
#     layout(
#       title = paste("Net imports", input$dateImports),
#     #   autosize = FALSE,
#       showlegend = FALSE
#     )%>%
#     layout(yaxis = list(categoryorder = "total ascending", title =""), 
#            xaxis = list(title ="Metric tons")
#            )
# 
#    # layout(title = 'Side By Side Subplots')
# #  plotImportsCombined <- ggplotly(plotImportsCombined, width = 600, height = 600 )
#   
#   })
    
    
    # output$plotimports <- renderPlotly({
    #   
    #   
    #   netImports_gg <- netImports() 
    #   netImports_gg <- netImports_gg%>%
    #     ggplot(aes(x=`Country_Name`, y=`value`, label=`value`)) + 
    #     geom_bar(stat='identity', aes(fill=`Net`), width=.1)  +
    #     scale_fill_manual(name="", 
    #                       labels = c("Net exporter", "Net importer"), 
    #                       values = c("Net exporter"="#00ba38", "Net importer"="#f8766d")) + 
    #     labs(x = "",y="Amount in tons") +
    #     facet_grid(.~`Nutrient`,scales="free")+
    #   # geom_text(aes(label = round(`Imports`, 1),
    #   #           size = 1)
    #   #           )+
    #   
    #   #labs(subtitle="Normalised mileage from 'mtcars'", 
    #   #       title= "Diverging Bars") + 
    #   coord_flip() 
    #   
    #   
    #   ggplotly(netImports_gg,
    #            height = 300 + 2*nrow(netImports_gg)
    #            )
    #   #%>%
    #   # layout(legend = l) %>%
    #   # layout(legend=list(title=list(text='')))%>%
    #   # layout(legend = list(orientation = 'h'))
    #   
    # }) #end of renderplotly
    
    #####end old version
    
    #### if user selects show single nutrients
  
  netImportsByNutrient <- reactive({

    #req(input$netImportsOutput)
  #  if (input$netImportsOutput==1 ){
      usebroad %>%
        filter(variable == "Net Imports") %>%
        filter(Year==input$dateImports) %>%
        filter(Nutrient == input$nutrientImports) %>%
        #filter(Nutrient == "Nitrogenous") %>%
        # filter(Year==2020)%>%
        filter(`Country_Name` != "World")%>%
        filter(`Country_Name` != "European Union") %>%
        mutate(Net = case_when(
          value > 0  ~ "Net importer",
          value < 0 ~ "Net exporter",
          value ==0 ~"Balanced"
          #   value == 0 ~ "Balanced",
          # .default = 999
        )) %>%
        group_by(Year, Nutrient) %>%
        # arrange(desc(Year)) %>%
       arrange(desc(value), .by_group = TRUE)%>%
        slice(1:10, (n()-10):n())

   # }
    # else {
    #   usebroad %>%
    #     filter(variable == "Net Imports") %>%
    #     filter(Year==input$dateImports) %>%
    #     #  filter(Nutrient == input$nutrientImports) %>%
    #     filter(Nutrient == input$nutrientImports) %>%
    #     # filter(Year==2020)%>%
    #     filter(`Country_Name` != "World")%>%
    #     filter(`Country_Name` != "European Union") %>%
    #     mutate(Net = case_when(
    #       value > 0  ~ "Net importer",
    #       value < 0 ~ "Net exporter",
    #       value ==0 ~"Balanced"
    #       #   value == 0 ~ "Balanced",
    #       # .default = 999
    #     ))
    # 
    # 
    # 
    # }
  })
  
  netImportWorld <- reactive({
   # netImportFiltered <- netImportsByNutrient() %>%
    usebroad %>%
      filter(variable == "Net Imports") %>%    
      filter(Year==input$dateImports) %>%
      # filter(Nutrient == "Nitrogenous") %>%
      # filter(Year==2020)%>%
       filter(Nutrient == input$nutrientImports) %>%
      # filter(Nutrient == input$nutrientImports) %>%
      # filter(Year==2020)%>%
      filter(`Country_Name` != "World")%>%
      filter(`Country_Name` != "European Union")%>%
      left_join(world_forDepRatio,.) %>%
      mutate(text_intermediate = paste(NAME_ENGL, "\n",value,  "\n", "net imports in MT") ) %>%
      mutate(text = ifelse(is.na(value), "No data available for this country", text_intermediate)) 
    
    ###
    
  })
  
  
  output$plotImportsByNutrient <- renderPlotly({
    
    netImportsByNutrientPlotly <- netImportsByNutrient()

    
    
    plot_ly(netImportsByNutrientPlotly,
                              y = ~Country_Name,
                              x = ~value ,
            color = ~Net,
                              type = "bar"
                             # height = 4000
    ) %>%
    layout(yaxis = list(categoryorder = "total ascending", title = ""),
           xaxis = list(title ="Metric tons"),
           title = paste(input$nutrientImports, "fertilizers net imports -",input$dateImports )
    )
    # %>%
    #   layout(
    #     title = "Plot",
    #    # autosize = FALSE,
    #     showlegend = FALSE
    #   ) %>%
    #   layout(yaxis = list(categoryorder = "total ascending"))
    #
    
  })
    
  
  
  #
  
  #Map - all 
  
  output$mapNetImports <- renderPlotly({
    
    netImports_gg <- netImportWorld() %>%
      ggplot() +
      geom_sf(aes(fill = value, text = text), show.legend = TRUE) +
      #  scale_fill_viridis_b()+
      coord_sf(crs = "EPSG:4326",  # Assuming latitude and longitude coordinates
               xlim = c(30, 180),   # Adjusted xlim for Asia-Pacific region (longitude)
               ylim = c(-50, 60),   # Adjusted ylim for Asia-Pacific region (latitude)
               expand = FALSE)+
      #coord_sf(crs = "ESRI:54030", xlim = c(-11000804, 12909125), ylim = c(-2485711, 6071856), expand = FALSE) +
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
      title = paste("Net imports -", input$dateImports),
      # caption = gisco_attributions()
    ) +
      labs(fill = "MT") +
    #change legend https://r-charts.com/spatial/choropleth-map-ggplot2/
    #scale_fill_viridis_c(option = "B") +
    #diverging colorscale
    #https://colorspace.r-forge.r-project.org/reference/scale_colour_continuous_divergingx.html  ; http://sahirbhatnagar.com/EPIB607/color-basics.html
    scale_fill_continuous_divergingx(palette = "Earth",
                                     rev = TRUE,
                                     mid = 0
                                     # ,
                                     # guide = guide_colorbar(
                                     #   direction = "horizontal",
                                     #   label.position = "bottom",
                                     #   title.position = "top"
                                     #   )
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
         #legend.position='bottom'
    ) 
    # guides(fill = guide_legend(title = "Unit: 1000", title.position = "bottom", title.theme =element_text(size = 10, face = "bold",colour = "gray70",angle = 0)))
    
    netImportsMap_plotly <- netImports_gg %>%
      ggplotly(tooltip = "text") %>%
      style(hoveron = "fill") %>%
      config(scrollZoom = TRUE)
    #  plotly_build()  
    
  })

  #############ENd of net imports
  
  
  
  #######START of import dep map##############
  
  observeEvent(input$dep_nutrient,{
    # importdep_world <- importdep_world %>%
    #   filter(nutrient == input$dep_nutrient)


    if (input$dep_nutrient == "Nitrogenous"){
      importdep_world <- importdep_world %>%    
        mutate_if(is.numeric, ~round(., 2)) %>%
        filter(Nutrient == "Nitrogenous") %>%
        filter(Year == input$impDepMapYear) %>%
        left_join(world_forDepRatio,.)  %>%
        mutate(text_intermediate = paste(Country_Name, "\n", "Dependency ratio", value ) ) %>%
        mutate(text = ifelse(is.na(value), "No data available for this country", text_intermediate)) 
        
    }
    if (input$dep_nutrient == "Phosphate"){
      importdep_world <- importdep_world %>%    
        mutate_if(is.numeric, ~round(., 2)) %>%
        filter(Nutrient == "Phosphate") %>%
        filter(Year == input$impDepMapYear) %>%
        left_join(world_forDepRatio,.)%>%
        mutate(text_intermediate = paste(Country_Name, "\n", "Dependency ratio", value ) ) %>%
        mutate(text = ifelse(is.na(value), "No data available for this country", text_intermediate)) 
    }
    if (input$dep_nutrient == "Potash"){
      importdep_world <- importdep_world %>%
        mutate_if(is.numeric, ~round(., 2)) %>%
        filter(Nutrient == "Potash") %>%
        filter(Year == input$impDepMapYear) %>%
        left_join(world_forDepRatio,.)%>%
        mutate(text_intermediate = paste(Country_Name, "\n", "Dependency ratio", value ) ) %>%
        mutate(text = ifelse(is.na(value), "No data available for this country", text_intermediate)) 
    }

  output$map_impdep <- renderPlotly({
    
  importdep_world_gg <- importdep_world %>%
    ggplot() +
    geom_sf(aes(fill = value, text = text, color = Country_Name), show.legend = T) +
  # scale_fill_viridis_b("")+
    scale_fill_gradient(low = "#e7e1ef", high = "#dd1c77") +
    coord_sf(crs = "EPSG:4326",  # Assuming latitude and longitude coordinates
             xlim = c(30, 180),   # Adjusted xlim for Asia-Pacific region (longitude)
             ylim = c(-50, 60),   # Adjusted ylim for Asia-Pacific region (latitude)
             expand = FALSE)+
    # coord_sf(crs = "ESRI:54030", #xlim = c(-11000804, 12909125), ylim = c(-2485711, 6071856)
    #          xlim = c(3000, 10909125),  # Adjusted xlim for Asia-Pacific region
    #          ylim = c(-4985711, 6071856),  expand = FALSE) +
    # scale_fill_continuous_divergingx(palette = "Earth",
    #                                  rev = TRUE
    #                                  # ,
    #                                  # guide = guide_colorbar(
    #                                  #   direction = "horizontal",
    #                                  #   label.position = "bottom",
    #                                  #   title.position = "top"
    #                                  #   )
    # ) +
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
    # labs(
    #  # title = "Import DEP",
    #   caption = gisco_attributions()
    # ) +
  labs(title=paste(input$dep_nutrient, "import dependency ratio"), color = "Legend", fill = "Ratio")+
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
  
  # importdep_world_plotly <- importdep_world_gg %>%
  #   ggplotly(tooltip = "text") %>%
  #  style(hoveron = "fill") 
  #  plotly_build()  
  
  importdep_world_plotly <- ggplotly(importdep_world_gg, tooltip = "text") %>%
    config(scrollZoom = TRUE) %>%
    layout(title = list(text = paste(input$dep_nutrient, " import dependency ratio -",input$impDepMapYear,
                                            '<br>',
                                            '<sup>',
                                            'Ratio between imports and agricultural use',"</sup>")))
  
  
  importdep_world_plotly %>%
    style(
      hoveron = "fills",
      # override the color mapping
      line.color = toRGB("gray64")  
      # don't apply these style rules to the first trace, which is the background graticule/grid
      # traces = seq.int(2, length(productionMap_gg$NAME_ENGL))
      #traces = seq.int(2, length(productionMap_gg$Country_Name))
    )%>%
    hide_legend()
         
#  ggplotly(importdep_world) #  importdep_world <- plot_ly()
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
  
  ##
 
  ##
  
  })  #end of observe event
  
  })
  
  ### end of imp dep map
  
  
##########Start of imp/exp quantity bubble chart##############
  
  #Import 
  impQuant <- reactive({
    usebroad %>%
      filter(`variable` == "Import Quantity") %>%
      filter(Nutrient == input$impExpQuantity_nutrient) %>%
      filter(`Year`==input$impExpQuantity_year)%>%
      filter(`Country_Name` != "World")%>%
      filter(`Country_Name` != "European Union") %>%
      filter(value !=0)%>%
      mutate(Country_Code_Big = case_when(ranks<16~Country_Code))%>%
      mutate(valueComma = scales::comma(value))
  })
  
  packingImport <- reactive({
    impQuant <- impQuant()
    circleProgressiveLayout(impQuant$value, sizetype='area')
    
  })
  
  impQuant_2 <- reactive({
    cbind(impQuant(), packingImport())%>%
      mutate(text = paste(Country_Name, "\n", valueComma, "\n", "Metric Tons Nutrient Equivalent") )
    
 
  })
  
  dat.gg_imp <- reactive({
    circleLayoutVertices(packingImport (), npoints=50)
  })
  
  
  plotImportBubble<- function() {
    dat.gg_imp <- dat.gg_imp()
    impQuant_2 <- impQuant_2()
    ggplot() +
      geom_polygon_interactive(data = dat.gg_imp(), aes(x, y, group = id, fill=id,
                                                    tooltip = impQuant_2$text[id],
                                                    data_id = id), colour = "black", alpha = 0.6) +
     # labs(title="Main importers") +
      labs(title=paste("Import quantity"),
           subtitle = paste(input$impExpQuantity_nutrient, " fertilizer - ",input$impExpQuantity_year ))+
      
      geom_text(data = impQuant_2(), aes(x, y, label = Country_Code_Big), size=4, color="black") +
      theme_void() +
      theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm"),
            plot.title = element_text(hjust = 0.5, size = 22),
            plot.subtitle =  element_text(hjust = 0.5, size = 15)
      )
    
    
  }
  
  
  output$bubble_impQuantity <- renderggiraph({
    gg_imp <- plotImportBubble () 
    ggiraph(code = print(gg_imp), selection_type = "none")
    
    

    
  })
  
  #Export 
  expQuant <- reactive({
    usebroad %>%
      filter(`variable` == "Export Quantity") %>%
      filter(Nutrient == input$impExpQuantity_nutrient) %>%
      filter(`Year`==input$impExpQuantity_year)%>%
      filter(`Country_Name` != "World")%>%
      filter(`Country_Name` != "European Union") %>%
      mutate(Country_Code_Big = case_when(ranks<16~Country_Code)) %>%
      filter(value !=0)%>%
      mutate(valueComma = scales::comma(value))
  })
  
  packingExport <- reactive({
    expQuant <- expQuant()
    circleProgressiveLayout(expQuant$value, sizetype='area')
    
  })
  
  expQuant_2 <- reactive({
    cbind(expQuant(), packingExport())%>%
      mutate(text = paste(Country_Name, "\n", valueComma, "\n", "Metric Tons Nutrient Equivalent") )
  })
  
  dat.gg_exp <- reactive({
    circleLayoutVertices(packingExport (), npoints=50)
  })
  
  
  plotExportBubble<- function() {
    dat.gg_exp <- dat.gg_exp()
    expQuant_2 <- expQuant_2()
    ggplot() +
      geom_polygon_interactive(data = dat.gg_exp(), aes(x, y, group = id, fill=id,
                                                        tooltip = expQuant_2$text[id],
                                                        data_id = id), colour = "black", alpha = 0.6) +
      #labs(title="Main exporters") +
      labs(title=paste("Export quantity"),
           subtitle = paste(input$impExpQuantity_nutrient, " fertilizer - ",input$impExpQuantity_year ))+
      scale_fill_viridis() +
      
      geom_text(data = expQuant_2(), aes(x, y, label = Country_Code_Big), size=4, color="black") +
      theme_void() +
      theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm"),
            plot.title = element_text(hjust = 0.5, size = 22),
            plot.subtitle =  element_text(hjust = 0.5, size = 15)
            
      )
  }
  
  
  output$bubble_expQuantity <- renderggiraph({
    gg_exp <- plotExportBubble () 
    ggiraph(code = print(gg_exp), selection_type = "none")
    
    
  })
  
###########START OF USE MAJOR TAB################
  
  
####Start of use Bubble chart###


  use <- reactive({
  usebroad %>%
    filter(`variable` == "Agricultural Use") %>%
      # filter(`Product` == "Potash") %>%
    #  filter(`Year`==2020)%>%
    #filter(`Product` == input$useNutrient) %>%
    #filter(`Year` == input$useYear)%>%
     filter(Nutrient == input$useNutrient) %>%
      filter(`Year`==input$useYear)%>%
    filter(`Country_Name` != "World")%>%
    filter(`Country_Name` != "European Union") %>%
    filter(value !=0)%>%
    mutate(Country_Code_Big = case_when(ranks<16~Country_Code))%>%
      mutate(valueComma = scales::comma(value))
  })
  
  packing <- reactive({
    use <- use()
    circleProgressiveLayout(use$value, sizetype='area')
    
  })
  
  use_2 <- reactive({
    cbind(use(), packing())%>%
    mutate(text = paste(Country_Name, "\n", valueComma, "\n", "Metric Tons Nutrient Equivalent") )
  })
    
  dat.gg <- reactive({
    circleLayoutVertices(packing (), npoints=50)
  })

  
  plotBubble<- function() {
    data.gg <- dat.gg()
    use_2 <- use_2()
    ggplot() +
      geom_polygon_interactive(data = dat.gg(), aes(x, y, group = id, fill=id,
                                                  tooltip = use_2$text[id],
                                                  data_id = id), colour = "black", alpha = 0.6) +
      scale_fill_viridis() +
      labs(title=paste("Agricultural use in MT nutrient equivalent"),
           subtitle = paste(input$useNutrient, " fertilizer - ",input$useYear ))+
      geom_text(data = use_2(), aes(x, y, label = Country_Code_Big), size=4, color="black") +
      theme_void() +
      theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
            
)
    
  }
  
  
output$useBubble <- renderggiraph({
  gg <- plotBubble () 
  ggiraph(code = print(gg), selection_type = "none")


})

#######START of AVG App rate map##############


appRate_filtered <- reactive({
  ####
  appRate <- application %>%
    mutate_if(is.numeric, ~round(., 2)) %>%
    filter(variable == "Application rate") %>%
    filter(Country_Name  %notin% c("World","European Union")) %>%
    #select(-1) %>%
    dplyr::rename(ISO3_CODE= "Country_Code") %>%
   # select(7,2,9,3,5,1,6) %>%
    filter(`Country_Name` != "World")%>%
    filter(`Country_Name` != "European Union") %>%
    filter(`Nutrient` %in% input$nutrientApplicationRt) %>%
    filter(`Year` ==input$yearApplicationRt) %>%
    mutate(text_intermediate = paste(Country_Name, "\n", value, "\n", "Application Kg per Ha") ) %>%
    mutate(text = ifelse(is.na(value), "No data available for this country", text_intermediate) )})

appRateWorld <- reactive({
  appRate_filtered <- appRate_filtered()
  
  
  
  
  appRateWorld <- appRate_filtered %>%
    left_join(world,.)
  ###

    })


  output$applicationRateMap <- renderPlotly({
    

    appRate_gg <- appRateWorld() %>%
      ggplot() +
      geom_sf(aes(fill = value, text = text), color = NA, show.legend = T) +
        scale_fill_viridis_b()+
      #coord_sf(crs = "ESRI:54030", xlim = c(-11000804, 12909125), ylim = c(-2485711, 6071856), expand = FALSE) +
      coord_sf(crs = "EPSG:4326",  # Assuming latitude and longitude coordinates
               xlim = c(30, 180),   # Adjusted xlim for Asia-Pacific region (longitude)
               ylim = c(-50, 60),   # Adjusted ylim for Asia-Pacific region (latitude)
               expand = FALSE)+
      labs(title=paste("Average application rate"),
           subtitle = paste("dkdk"))+
         #  color = "Legend", fill = "Kg/ha")+
    theme(plot.caption = element_text(face = "italic"),
          #panel.background = element_blank(),
          plot.background = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank() 
         # legend.position = "bottom",
    ) 
  #  guides(fill = guide_legend(title = "Unit: 1000", title.position = "bottom", title.theme =element_text(size = 10, face = "bold",colour = "gray70",angle = 0)))
    
    appRate_plotly <- appRate_gg %>%
      ggplotly(tooltip = "text") %>%
      config(scrollZoom = TRUE) %>%
      style(hoveron = "fill") %>%
      layout(title = list(text = paste0('Average application rate',
                               '<br>',
                               '<sup>',
                               input$nutrientApplicationRt,' fertilizer - ',input$yearApplicationRt,'</sup>')))
 

  #    layout(legend = list(orientation = "h", x = 0.4, y = -0.2))

  
})

### end of imp dep map
  
  
  
###start of app rate and yields####
  
  appRateYield_filtered <- reactive({
    application %>%
      filter(variable == "Application rate") %>%
      filter(`Nutrient` == input$nutrientUseYields) %>%
      filter(`Year` ==input$yearUseYields) %>%
      filter(`Country_Name` != "World")%>%
      filter(`Country_Name` != "European Union") %>%
     filter(`Yield, Cereals` <10) %>%
      filter(`Yield, Cereals` >=0) %>%
      mutate(text = paste(Country_Name, "\n", value, "\n", `Yield, Cereals`) ) %>%
      dplyr::rename(`Application rate`= value)
    
  })
  
  
  output$useYields <- renderPlotly({
    
    useYields_gg <- appRateYield_filtered() %>%
    ggplot(aes(x=`Application rate`, y=`Yield, Cereals`, color = Nutrient), text = Country_Name 
           #text = paste("Country:", Country_Name)
             ) +
    #  geom_point(size = 1.5, shape=1) + 
      #https://plotly.com/ggplot2/hover-text-and-formatting/
      geom_point(aes(text=Country_Name), size = 1.5, shape = 1)+ 
      labs(title=paste("Relation between application rate and cereal yields"),
           subtitle = paste(input$nutrientUseYields, " fertilizer -",input$yearUseYields ))+
       stat_smooth(aes(),method = "lm", formula = y ~ poly(x,2), size = .5) +     # stat_smooth(method = "lm", formula = y ~ x^2, size = 1) +
     # scale_fill_fertilizerCategories() + #global
      scale_color_manual_fertizerCategories()+
     # scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
      # stat_smooth(aes(),method = "lm", formula = y ~ x + I(x^2), size = .5) +     # stat_smooth(method = "lm", formula = y ~ x^2, size = 1) +
      # stat_smooth(method = "lm", formula = y ~ x^2, size = 1) +
      labs(
        #title = "Relation between individual application rate and cereal yields", 
        x = "Kg applied per Ha", y = "Yield, Cereals, Tons per Ha") 
      # geom_smooth()
    
   ggplotly(useYields_gg)
   
   
   ##
   
   # output$useYieldsWOConfInt <- renderPlotly({
   #   
   #   useYields_gg <- appRateYield_filtered() %>%
   #     ggplot(aes(x=`Application rate`, y=`Yield, Cereals`, color = Nutrient), text = Country_Name 
   #            #text = paste("Country:", Country_Name)
   #     ) +
   #     #  geom_point(size = 1.5, shape=1) + 
   #     #https://plotly.com/ggplot2/hover-text-and-formatting/
   #     geom_point(aes(text=Country_Name), size = 1.5, shape = 1)+ 
   #     stat_smooth(aes(),method = "lm", formula = y ~ x + I(x^2), size = .5, se=FALSE) +     # stat_smooth(method = "lm", formula = y ~ x^2, size = 1) +
   #     # stat_smooth(method = "lm", formula = y ~ x^2, size = 1) +
   #     labs(
   #       #title = "Relation between individual application rate and cereal yields", 
   #       x = "Kg applied per Ha", y = "Yield, Cereals, Tons per Ha") 
   #   # geom_smooth()
   #   
   #   ggplotly(useYields_gg)
   # 

 
    
 
  
  })
  
  output$useYieldsNoLine <- renderPlotly({
    
    useYields_gg <- appRateYield_filtered() %>%
      ggplot(aes(x=`Application rate`, y=`Yield, Cereals`, color = Nutrient)
             #text = paste("Country:", Country_Name)
      ) +
      #  geom_point(size = 1.5, shape=1) + 
      #https://plotly.com/ggplot2/hover-text-and-formatting/
      geom_point(aes(text=Country_Name), size = 1.5, shape = 1)+ 
     # stat_smooth(aes(),method = "lm", formula = y ~ x + I(x^2), size = .5) +     # stat_smooth(method = "lm", formula = y ~ x^2, size = 1) +
      # stat_smooth(method = "lm", formula = y ~ x^2, size = 1) +
      labs(
        #title = "Relation between individual application rate and cereal yields", 
        x = "Kg applied per Ha", y = "Yield, Cereals, Tons per Ha") 
    # geom_smooth()
    
    ggplotly(useYields_gg)
    
    
    ##
    
    # output$useYieldsWOConfInt <- renderPlotly({
    #   
    #   useYields_gg <- appRateYield_filtered() %>%
    #     ggplot(aes(x=`Application rate`, y=`Yield, Cereals`, color = Nutrient), text = Country_Name 
    #            #text = paste("Country:", Country_Name)
    #     ) +
    #     #  geom_point(size = 1.5, shape=1) + 
    #     #https://plotly.com/ggplot2/hover-text-and-formatting/
    #     geom_point(aes(text=Country_Name), size = 1.5, shape = 1)+ 
    #     stat_smooth(aes(),method = "lm", formula = y ~ x + I(x^2), size = .5, se=FALSE) +     # stat_smooth(method = "lm", formula = y ~ x^2, size = 1) +
    #     # stat_smooth(method = "lm", formula = y ~ x^2, size = 1) +
    #     labs(
    #       #title = "Relation between individual application rate and cereal yields", 
    #       x = "Kg applied per Ha", y = "Yield, Cereals, Tons per Ha") 
    #   # geom_smooth()
    #   
    #   ggplotly(useYields_gg)
    # 
    
    
    
    
    
  })
  
  
  
###End of app rate and yields####
  
#######start of production bubble##########
  ####Start of use Bubble chart###
  
  
  production <- reactive({
    usebroad %>%
      filter(`variable` == "Production") %>%
      # filter(`Product` == "Potash") %>%
      #  filter(`Year`==2020)%>%
      #filter(`Product` == input$useNutrient) %>%
      #filter(`Year` == input$useYear)%>%
      filter(Nutrient == input$productionNutrient) %>%
      filter(`Year`==input$productionYear)%>%
      filter(`Country_Name` != "World")%>%
      filter(`Country_Name` != "European Union") %>%
      filter(value !=0)%>%
      mutate(Country_Code_Big = case_when(ranks<14~Country_Code)) %>%
      mutate(valueComma = scales::comma(value))
  })
  
  packingProduction <- reactive({
    production <- production()
    circleProgressiveLayout(production$value, sizetype='area')
    
  })
  
  production_2 <- reactive({
    cbind(production(), packingProduction())%>%
      mutate(text = paste(Country_Name, "\n", valueComma, "\n", "Metric Tons Nutrient Equivalent") )
  })
  
  dat.ggProduction <- reactive({
    circleLayoutVertices(packingProduction (), npoints=50)
  })
  
  
  plotBubbleProduction<- function() {
    data.ggProduction <- dat.ggProduction()
    production_2 <- production_2()
    ggplot() +
      geom_polygon_interactive(data = dat.ggProduction(), aes(x, y, group = id, fill=id,
                                                    tooltip = production_2$text[id],
                                                    data_id = id), colour = "black", alpha = 0.6) +
      labs(title=paste(input$productionNutrient,"fertilizer production in metric tons"), 
           subtitle = paste(input$productionYear))+
      scale_fill_viridis() +
      geom_text(data = production_2(), aes(x, y, label = Country_Code_Big), size=4, color="black") +
      theme_void() +
      theme(legend.position="none",
            plot.margin=unit(c(0,0,0,0),"cm"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
            
            )
  }
  
  
  output$productionBubble <- renderggiraph({
    ggProduction <- plotBubbleProduction () 
    ggiraph(code = print(ggProduction), selection_type = "none")
    
    
  })
  
###########end of production bubble########
  
#####Start production map#######

    productionMapFiltered <- reactive({
      production_tobefiltered %>%
      #filter(`variable` == "Production") %>%
      # filter(`Product` == "Potash") %>%
      #  filter(`Year`==2020)%>%
      #filter(`Product` == input$useNutrient) %>%
      #filter(`Year` == input$useYear)%>%
      filter(`Nutrient` == input$productionNutrient) %>%
      filter(`Year`==input$productionYear)%>%
        left_join(world,.)  %>%
        mutate(valueComma = scales::comma(value))%>%
          #mutate(value = ifelse(value ==0, 99999999, value) ) %>%
      # mutate(text = value)
                #paste(ISO3_CODE, value, "\n", "Metric Tons Nutrient Equivalent"))
        mutate(text_intermediate = paste(Country_Name, "\n", valueComma, "\n", "Metric Tons Nutrient Equivalent") ) %>%
          mutate(text = ifelse(is.na(value), "No data available for this country", text_intermediate) )
    #    mutate(text_intermediate = paste(ISO3_CODE, "Metric Tons Nutrient Equivalent") )
        # mutate(text = paste(ISO3_CODE))
        
       # unite("text", Country_Name, value, remove = FALSE)
       # mutate(text_intermediate = paste(Country_Name,  "Metric Tons Nutrient Equivalent"))
       # mutate(text_intermediate = paste(Country_Name, "\n", value, "\n", "Metric Tons Nutrient Equivalent") )
     #   mutate(text = ifelse(is.na(value), "No data available for this country", text_intermediate) ) 

      
   #   
      #%>%
               

      #filter(`Country_Name` != "World")%>%
    #  filter(`Country_Name` != "European Union") #%>%
     # filter(value !=0)%>%
     # mutate(Country_Code_Big = case_when(ranks<16~Country_Code))
  })
#https://stackoverflow.com/questions/71628944/custom-tooltip-hovertext-for-choropleth-created-with-plotly-via-r
  output$productionMap <- renderPlotly({
    
    productionMap_gg <- productionMapFiltered() %>%
    ggplot() +
      geom_sf(aes(fill = value, text = text, color = NAME_ENGL))+
              #, color = NA, show.legend = TRUE) +
      
   #   scale_colour_gradient()   +   #  scale_fill_viridis_b()+
      #coord_sf(crs = "ESRI:54030", xlim = c(-11000804, 12909125), ylim = c(-2485711, 6071856), expand = FALSE) +
      coord_sf(crs = "EPSG:4326",  # Assuming latitude and longitude coordinates
               xlim = c(30, 180),   # Adjusted xlim for Asia-Pacific region (longitude)
               ylim = c(-50, 60),   # Adjusted ylim for Asia-Pacific region (latitude)
               expand = FALSE)+
      scale_fill_continuous_divergingx(palette = "Earth",
                                       rev = TRUE
                                       # ,
                                       # guide = guide_colorbar(
                                       #   direction = "horizontal",
                                       #   label.position = "bottom",
                                       #   title.position = "top"
                                       #   )
      ) +
      labs(title=paste("Fertilizer production -",input$productionYear), color = "Legend", fill = "Production in MT")+
      
     # theme_minimal() +
   
      
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
    
# 
    productionMap_gg <- ggplotly(productionMap_gg, tooltip = "text")%>%
      config(scrollZoom = TRUE) 
    
    productionMap_gg %>%
      style(
        hoveron = "fills",
        # override the color mapping
        line.color = toRGB("gray64")   
        # don't apply these style rules to the first trace, which is the background graticule/grid
       # traces = seq.int(2, length(productionMap_gg$NAME_ENGL))
       #traces = seq.int(2, length(productionMap_gg$Country_Name))
      )%>%
      hide_legend()
    
    # ggplotly(productionMap_gg, tooltip = "text") %>% 
    #   style(hoverlabel = list(bgcolor = "white"),hoveron = "fill")
    
    
   # productionMap_plotly <- ggplotly(productionMap_gg, tooltip = "text") %>%
   #   style(hoveron = "text")
    
   # 
   # productionMap_plotly %>%
   #  # ggplotly(tooltip = "text") %>%
   #   style(
   #     hoveron = "fills",
   #     # override the color mapping
   #     line.color = toRGB("gray40"),
   #     # don't apply these style rules to the first trace, which is the background graticule/grid
   #     traces = seq.int(2, length(productionMap_plotly$x$data))
   #   )

     
     
   
    
  })

### end of imp dep map

  
#####end production map#########


  
  
} # end of server   


# Run the application 
shinyApp(ui = ui, server = server)


#deployApp(appName = "FertilizerDashboard13Jan22_AsiaPacific")