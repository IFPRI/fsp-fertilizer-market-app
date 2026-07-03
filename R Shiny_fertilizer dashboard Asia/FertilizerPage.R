install.packages("readxl")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("zoo")
install.packages("ggplot2")

library(readxl)
library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)
library(ggplot2)

setwd("C:/Users/BRICE/Dropbox (IFPRI)/FertilizerPageApp")

prices <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 8)
prices <- prices %>%
  mutate(date=as.Date(date, "%Y-%m-%d"))

#product prices 
p_fert_long <- prices %>%
  filter(Product %in% c("Ammonia", "DAP", "MAP", "MOP",
                        "Phosphate Rock", "Urea"))

p_fert <- read_excel("Fertilizers all v2.xlsx") 
save(p_fert, file = "p_fert.RData")

#bring in imports data as well 
imports <- read_excel("NetImports.xlsx") 
save(imports, file = "imports.RData")

#

p_fert <- p_fert %>%
  rowwise()%>%
  mutate(urea_avg = mean(c(`Urea Gulf NOLA (gran)`, `Urea Mediterranean`), na.rm = TRUE),
         dap_avg = mean(c(`DAP US Gulf Nola`,`DAP Baltic`), na.rm = TRUE),
         ammonia_avg = mean(c(`Ammonia Middle East`, `Ammonia Western Europe`, `Ammonia US Gulf NOLA`), na.rm = TRUE),
         potash_avg = mean(c(`Potash Baltic Standard`, `Potash Gulf NOLA` ), na.rm = TRUE)
         )
  #select(c("Urea Gulf NOLA (gran)","Urea Gulf Prill","Urea Mediterranean")) %>%
 # mutate(Urea_avg = rowMeans(select(.,"Urea Gulf NOLA (gran)","Urea Gulf Prill","Urea Mediterranean")))

p_fert_long <- pivot_longer(p_fert, cols = 2:21)

p_fert_long <-p_fert_long %>%
  mutate(Date=as.Date(Date, "%Y-%m-%d"),
         value=as.numeric(value))

p_fert_long %>%
  filter(name!="Natural Gas BNGC") %>%
  ggplot( aes(x=Date, y=value, colour = name)) +
  geom_line()


###########



#############Imports
ggplot(imports, aes(x=`Country`, y=`Imports`, label=`Imports`)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, 
                   x = `Country`, 
                   yend = `Imports`, 
                   xend = `Country`), 
               color = "black") +
  geom_text(color="white", size=2) +
  labs(title="Net imports", 
       subtitle="") + 
  ylim(-8000000, 8000000) +
  coord_flip()


## different kind
imports_nit <- imports %>%
  filter(`Nutrient` == "Potash")

ggplot(imports_nit, aes(x=`Country`, y=`Imports`, label=`Net`)) + 
  geom_bar(stat='identity', aes(fill=`Net`), width=.1)  +
    scale_fill_manual(name="Net", 
                      labels = c("Net exporter", "Net importer"), 
                      values = c("Net exporter"="#00ba38", "Net importer"="#f8766d")) + 
  #labs(subtitle="Normalised mileage from 'mtcars'", 
#       title= "Diverging Bars") + 
  coord_flip()




############################
#trying importing data from googlesheets

library(googlesheets4)
prices <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 8)
products <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 7, col_types = "?????inc??" )

appRate <- products %>%
  filter(variable == "Application rate") %>%
  select(-1)



t1 <- prices %>%
  tabyl(Product)


############



#############world map data##############
url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="Country-Codes-A-C"]') %>%
  html_table()
iso_codes <- iso_codes[[1]][, -1]
iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")
head(iso_codes)

# reading in import dep. data
importdep <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 10)
importdep <- importdep %>%
  rename("ISO3"= ISO3_CODE)
# next step
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)

#importdep['ISO3'] <- iso_codes$ISO3[match(importdep$Country.Name, iso_codes$Country)]
world_data["ISO3"] <- iso_codes$ISO3[match(world_data$region, iso_codes$Country)]

world_data_na <- world_data %>%
  filter(is.na(ISO3))

world_data_na %>% tabyl(region)
world_data['N'] <- importdep$N[match(world_data$ISO3, importdep$ISO3)]
world_data['K2O'] <- importdep$K2O[match(world_data$ISO3, importdep$ISO3)]
world_data['P2O5'] <- importdep$P2O5[match(world_data$ISO3, importdep$ISO3)]

#world_data <- world_data[!is.na(world_data$ISO3), ]


worldMaps <- function(importdep, world_data){
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.text = element_text(size = 14),
                       axis.title = element_text(size = 14),
                       strip.text = element_text(size = 14),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  # Select only the data that the user has selected to view
  plotdf <- importdep[importdep$K20 == indicator ,]
  plotdf <- plotdf[!is.na(plotdf$ISO3), ]
  
  library(RColorBrewer)
  library(ggiraph)
  
  g <- 
    
    ggplot() + 
    geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = N, 
                                 tooltip = sprintf("%s<br/>%s", ISO3, N))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
    scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) #+ 
    # labs(fill = data_type, color = data_type, title = NULL, x = NULL, y = NULL, caption = capt) + 
    #my_theme()
  
  return(g)
  
  ##
  ggplot() + 
    geom_polygon_interactive(data = subset(world_data, lat >= -60 & lat <= 90), color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = N, group = group, 
                                 tooltip = sprintf("%s<br/>%s", ISO3, N))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white')# + 
   # labs(fill = N, color = N, title = NULL, x = NULL, y = NULL, caption = capt)  
   # my_theme()
  
  ##
  
world_data_na <- world_data %>%
  filter(ISO3==is.na)
    tabyl(ISO3)
  
}



##############TRY WITH https://stackoverflow.com/questions/27338512/color-countries-on-world-map-based-on-iso3-codes-in-r-using-ggplot ###############

#from this https://cran.r-project.org/web/packages/igoR/vignettes/mapping.html

if("countrycode" %in% installed.packages()[,1] == FALSE){
  install.packages("countrycode")}
if("giscoR" %in% installed.packages()[,1] == FALSE){
  install.packages("giscoR")}
if("igoR" %in% installed.packages()[,1] == FALSE){
  install.packages("igoR")}

library(igoR)

# Helper packages
library(dplyr)
library(ggplot2)
library(countrycode)

# Geospatial packages
library(giscoR)


world <- gisco_get_countries(year = "2010")

# reading in import dep. data
importdep <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 10)
importdep_world <- importdep %>%
  left_join(world,.) %>%
  select(NAME_ENGL, K2O, N, P2O5, geometry)%>%
  pivot_longer(cols = c(K2O, N, P2O5), 
               names_to = "nutrient",
               values_to = "value")

#plotting it
importdep_world <- importdep_world  %>%
  filter(nutrient == "N") 

importdep_world %>%
ggplot() +
  geom_sf(aes(fill = value), color = 'black', show.legend = TRUE) +
  scale_fill_viridis_b()+
  coord_sf(crs = "ESRI:54030") +
  # facet_wrap(vars(year),
  #            ncol = 1,
  #            strip.position = "left"
  # ) +
   #scale_fill_manual(
   #  values = "#74A9CF",
   #   na.value = "#E0E0E0"
   # ) +
  theme_void() +
  labs(
    title = "Import DEP",
    caption = gisco_attributions()
  ) +
  theme(plot.caption = element_text(face = "italic")) 
 # guides(fill = guide_legend(title = "Unit: 1000", title.position = "bottom", title.theme =element_text(size = 10, face = "bold",colour = "gray70",angle = 0)))

#########################Map of application rates###############

products <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 7, col_types = "?????inc??" )

appRate <- products %>%
  filter(variable == "Application rate") %>%
  select(-1) %>%
  rename(ISO3_CODE= "Country_Code") %>%
  select(7,2,9,3,5,1,6)


# appRateWorld<-world %>%
#   left_join(appRate,.) %>%
#   select(-(8:11))

`%notin%` <- Negate(`%in%`)
appRateWorld <- appRate %>%
  left_join(world,.) %>%
  filter(Country_Name  %notin% c("World","European Union"))

# %>%
#   select(-1,-2,-4,-5)
 # filter(NAME_ENGL!= "ANTARCTICA") 

################end of map of application rates###############

####use yields scatter plot#######

useYields <- appRate %>%
  ggplot(aes(x=value, y=`Yield, Cereals`, color = Nutrient)) + 
  geom_point(size = 1.5, shape=1) + 
  labs(title = "Relation between individual application rate and cereal yields", x = "Kg applied per Ha", y = "Yield, Cereals, Tons per Ha")

useYields

################use yields map#################


#######Fertilizer use
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

usebroad <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 5)
use <- usebroad %>%
  filter(variable == "Agricultural Use") %>%
  filter(Product =="Potash") %>%
  filter(Year==2020)%>% 
  filter(Country_Name != "World")%>% 
  filter(Country_Name != "European Union") %>%
  filter(value !=0)%>%
  mutate(Country_Code_Big = case_when(ranks<10~Country_Code))
 # mutate(text = dpaste("name: ",data$group, "\n", "value:", data$value, "\n", "You can add a story here!"))




# Generate the layout
packing <- circleProgressiveLayout(use$value, sizetype='area')
data <- cbind(use, packing) %>%
  mutate(text = paste(Country_Name, "\n", value) )
dat.gg <- circleLayoutVertices(packing, npoints=50)


# Make the plot with a few differences compared to the static version:
p <- data %>%
  ggplot() + 
  geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, 
                                              tooltip = data$text[id], 
                                              data_id = id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, label = Country_Code_Big), size=4, color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) 


widg <- ggiraph(ggobj = p)
widg
#  coord_equal()
  
ggplotly(p)
  widg <- ggiraph(ggobj = p, width_svg = 10, height_svg = 10)



####VERSION ONLINE#############

# Create data
data <- data.frame(group=paste("Group_", sample(letters, 70, replace=T), sample(letters, 70, replace=T), sample(letters, 70, replace=T), sep="" ), value=sample(seq(1,70),70)) 

# Add a column with the text you want to display for each bubble:
data$text <- paste("name: ",data$group, "\n", "value:", data$value, "\n", "You can add a story here!")

# Generate the layout
packing <- circleProgressiveLayout(data$value, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot with a few differences compared to the static version:
p <- ggplot() + 
  geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, tooltip = data$text[id], data_id = id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, label = gsub("Group_", "", group)), size=2, color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  coord_equal()

# Turn it interactive
widg <- ggiraph(ggobj = p, width_svg = 7, height_svg = 7)



#########Bubble importquant

#Import 
impQuant <- usebroad %>%
    filter(`variable` == "Import Quantity") %>%
    filter(`Product` == "Potash") %>%
    filter(`Year`==2020)%>%
    filter(`Country_Name` != "World")%>%
    filter(`Country_Name` != "European Union") %>%
    filter(value !=0)%>%
    mutate(Country_Code_Big = case_when(ranks<16~Country_Code))

# Generate the layout
packing <- circleProgressiveLayout(impQuant$value, sizetype='area')
data <- cbind(impQuant, packing) %>%
  mutate(text = paste(Country_Name, "\n", value) )
dat.gg <- circleLayoutVertices(packing, npoints=50)


p <- data %>%
  ggplot() + 
geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id,
                                                      tooltip = data$text[id],
                                                      data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = data, aes(x, y, label = Country_Code_Big), size=4, color="black") +
    theme_void() +
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") )



widg <- ggiraph(ggobj = p)
widg

library(janitor
        )

t1 <- impQuant %>%
  filter(Year==2020) %>%
  tabyl(Product)



###################Production map and bubble chart############
##bubble
#######Fertilizer production
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

world <- gisco_get_countries(year = "2010")

usebroad <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 5)
production <- usebroad %>%
  filter(variable == "Production") %>%
 # filter(Product =="Potash") %>%
  #filter(Year==2020)%>% 
  filter(Country_Name != "World")%>% 
  filter(Country_Name != "European Union") %>%
 # filter(value !=0)%>%
 # mutate(Country_Code_Big = case_when(ranks<10~Country_Code)) %>%
  rename(ISO3_CODE= "Country_Code") 

`%notin%` <- Negate(`%in%`)
productionWorld <- production %>%
  left_join(world,.) %>%
  filter(Country_Name  %notin% c("World","European Union")) %>%
  select(-1,-2,-5,-14,-15) %>%
  rename(Country_Name = "NAME_ENGL")
  
  
# mutate(text = dpaste("name: ",data$group, "\n", "value:", data$value, "\n", "You can add a story here!"))

##clearning for the world


appRate <- products %>%
  filter(variable == "Application rate") %>%
  select(-1) %>%
  rename(ISO3_CODE= "Country_Code") %>%
  select(7,2,9,3,5,1,6)


# appRateWorld<-world %>%
#   left_join(appRate,.) %>%
#   select(-(8:11))

`%notin%` <- Negate(`%in%`)
appRateWorld <- appRate %>%
  left_join(world,.) %>%
  filter(Country_Name  %notin% c("World","European Union"))



# Generate the layout
packing <- circleProgressiveLayout(production$value, sizetype='area')
data <- cbind(production, packing) %>%
  mutate(text = paste(Country_Name, "\n", value) )
dat.gg <- circleLayoutVertices(packing, npoints=50)


# Make the plot with a few differences compared to the static version:
p <- data %>%
  ggplot() + 
  geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, 
                                              tooltip = data$text[id], 
                                              data_id = id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, label = Country_Code_Big), size=4, color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) 


widg <- ggiraph(ggobj = p)
widg

############end production map and bubble chart##########
  
  
##############Dep Ratio linked to R data not google sheet########

load(file="./fertilizer_use.RData" )
importdep <- fertilizer.use %>%
  filter(variable =="DependencyRatio")

#world map data (new version)
world <- gisco_get_countries(year = "2010") %>%
  rename("Country_Code"= ISO3_CODE)


importdep_world <- importdep %>%
left_join(world,.) %>%
  # select(NAME_ENGL, K2O, N, P2O5, geometry)%>%
  # pivot_longer(cols = c(K2O, N, P2O5), 
  #              names_to = "nutrient",
  #              values_to = "value") %>%
  filter(NAME_ENGL!= "ANTARCTICA") 



#########db connect##########
library(RMySQL)

# connect to the database


mysqlconnection = dbConnect(RMySQL::MySQL(),
                            dbname=Sys.getenv('DB_NAME'),
                            host=Sys.getenv('DB_HOST'),
                            port=3306,
                            user=Sys.getenv('DB_USER'),
                            password=Sys.getenv('DB_PASSWORD'))

dbListTables(mysqlconnection) # displays the tables available in this database.


#result_fert = dbSendQuery(mysqlconnection, "select * from DAVID_BLOOMBERG_REQUESTS_FERTILIZERS") # write query to access the records from a particular table.
result_fertPrices = dbSendQuery(mysqlconnection, "select * from FERTILIZERDASHBOARDPRICES") # write query to access the records from a particular table.


fertilizerPrices = fetch(result_fertPrices, n=-1)

###########

products <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 7, col_types = "?????inc??" )
save(products, file = "products.RData") 
load(file= "./products.RData"   )

load(file="./fertilizer_use.RData" )

##########net imports viz######
netImports <- fertilizer.use %>%
  filter(variable == "Net Imports") %>%
 # filter(Nutrient == "Nitrogenous") %>%
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
  arrange(desc(value), .by_group = TRUE) %>%
  filter(Year==2020) %>%
  filter(Nutrient == "Nitrogenous") %>%
  slice(1:15)
  
###works
plot_ly(netImports,
        y = ~Country_Code,
        x = ~value ,
        type = "bar",
        height = 800 + 10*nrow(netImports)
) %>%
  layout(
    title = "Plot",
    autosize = FALSE,
    showlegend = FALSE
  ) %>%
  layout(yaxis = list(categoryorder = "total ascending"))

###


  
  netImports_gg <- netImports %>%
    ggplot(aes(x=`Country_Name`, y=`value`, label=`value`)) + 
    geom_bar(stat='identity', aes(fill=`Net`), width=.1)  +
    scale_fill_manual(name="", 
                      labels = c("Net exporter", "Net importer"), 
                      values = c("Net exporter"="#00ba38", "Net importer"="#f8766d")) + 
    labs(x = "",y="Amount in tons") +
      facet_grid(.~`Nutrient`,scales="free")
    # geom_text(aes(label = round(`Imports`, 1),
    #           size = 1)
    #           )+
    
    #labs(subtitle="Normalised mileage from 'mtcars'", 
    #       title= "Diverging Bars") + 
    coord_flip() 
  
  
    
               
             fixedrange = TRUE)  
  # layout(legend = l) %>%
  # layout(legend=list(title=list(text='')))%>%
  # layout(legend = list(orientation = 'h'))
  
  
  plot_ly(
    d,
    y = ~ Category,
    x = ~ Value,
    type = "bar",
    height = 500 + 10*nrow(d)
  ) %>%
    layout(
      title = "Plot",
      autosize = FALSE,
      showlegend = FALSE
    ) 
  
  plot_ly(
    d2,
    y = ~ Category,
    x = ~ Value,
    type = "bar",
    height = 500 + 10*nrow(d2)
  ) %>%
    layout(
      title = "Plot",
      autosize = FALSE,
      showlegend = FALSE
    ) 
  
  
  
  ###########combining fertilizer prices and input prices
  library(RMySQL)
  
  load(file="./FertilizerDashboard13Jan22/connection.RData")
  
  mysqlconnection = dbConnect(RMySQL::MySQL(),
                              dbname= connection$dbname,
                              host=connection$host,
                              port=connection$port,
                              user=connection$user,
                              password=connection$password
  )
  
  # close db connection after function call exits
  on.exit(dbDisconnect(mysqlconnection))
  
  result_fertPrices <- dbSendQuery(mysqlconnection, "select * from FERTILIZERDASHBOARDPRICES") # write query to access the records from a particular table.
  prices <- fetch(result_fertPrices, n=-1)
  
  prices <- prices %>%
    mutate(Product = replace(Product, Product ==  "Natural Gas - USD", "Natural Gas - EU"))
    mutate(Product = str_replace(Product, "Natural Gas - EU", "Natural Gas - USD"))

  pricesIndex <- prices %>%
    filter(Variable =="Index") %>%
    filter(Product %in% c("N", "P", "K", "NPK Index", "Natural Gas - EU", "Natural Gas - USA"))

  
  
  ############Fixing problem in the world maps
  
  library(giscoR)
  library(dplyr)
  library(ggplot2)
  library(countrycode)
  library(googlesheets4)
  library(tidyverse)
  
  load(file="./fertilizer_use.RData" )
  load(file= "./products.RData"   )
  
  world <- gisco_get_countries(year = "2010")
  
  # reading in import dep. data
  importdep <- range_read("https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8/edit?usp=sharing", sheet = 10)
  importdep_world <- importdep %>%
    full_join(world,., by ="ISO3_CODE") %>%
    select(NAME_ENGL, K2O, N, P2O5, geometry)%>%
    pivot_longer(cols = c(K2O, N, P2O5), 
                 names_to = "nutrient",
                 values_to = "value")
  
  #plotting it
  importdep_world <- importdep_world  %>%
    filter(nutrient == "N") 
  
  importdep_world %>%
    ggplot() +
    geom_sf(aes(fill = value), color = 'black', show.legend = TRUE) +
    scale_fill_viridis_b()+
    coord_sf(crs = "ESRI:54030") +
    # facet_wrap(vars(year),
    #            ncol = 1,
    #            strip.position = "left"
    # ) +
    #scale_fill_manual(
    #  values = "#74A9CF",
    #   na.value = "#E0E0E0"
    # ) +
    theme_void() +
    labs(
      title = "Import DEP",
      caption = gisco_attributions()
    ) +
    theme(plot.caption = element_text(face = "italic")) 
  # guides(fill = guide_legend(title = "Unit: 1000", title.position = "bottom", title.theme =element_text(size = 10, face = "bold",colour = "gray70",angle = 0)))
  
  ##########
  `%notin%` <- Negate(`%in%`)
  
  appRate <- products %>%
    filter(variable == "Application rate") %>%
    filter(Country_Name  %notin% c("World","European Union")) %>%
    select(-1) %>%
    rename(ISO3_CODE= "Country_Code") %>%
    select(7,2,9,3,5,1,6) 
  
  appRateWorld <- appRate %>%
    left_join(world,.)
  
  
  
  #### world map for net imports
  netImports <- fertilizer.use %>%
    filter(variable == "Net Imports") %>%
    filter(Nutrient == "Nitrogenous") %>%
    filter(Year==2020)%>%
    filter(`Country_Name` != "World")%>%
    filter(`Country_Name` != "European Union") %>%
    mutate(Net = case_when(
      value > 0  ~ "Net importer",
      value < 0 ~ "Net exporter",
      value ==0 ~"Balanced"
      #   value == 0 ~ "Balanced",
      # .default = 999
    ))
  
  worldNetImports <- world %>%
    rename("Country_Code" = ISO3_CODE)
  netImportsWorld <- netImports %>%
    left_join(worldNetImports,.)
  
  netImportsWorld_gg <- netImportsWorld %>%
    ggplot() +
    geom_sf(aes(fill = value, text = value), color = NA, show.legend = TRUE) +
    #  scale_fill_viridis_b()+
    coord_sf(crs = "ESRI:54030", xlim = c(-11000804, 12909125), ylim = c(-2485711, 6071856), expand = FALSE) +

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
  
  netImportsMap_plotly <- netImportsWorld_gg %>%
    ggplotly(tooltip = "text") %>%
    style(hoveron = "fill") 
  #  plotly_build()  
  
#############join use and world
  
  production <- fertilizer.use %>%
    filter(variable == "Production") 
    
    worldTransformed <- world %>%
    rename("Country_Code" = ISO3_CODE)
  
  productionWorld <- production %>%
    left_join(worldTransformed,.) %>%
    filter(Country_Code == "NAM")%>%
   mutate(text_intermediate = paste(Country_Name, "\n", value, "\n", "Metric Tons Nutrient Equivalent") ) %>%
  mutate(text = ifelse(is.na(value), "No data available for this country", text_intermediate) ) 
  
  
  
  
  
  #3#scatter
  useYields_gg <- products %>%
      filter(variable == "Application rate") %>%
     # filter(`Nutrient` == input$nutrientUseYields) %>%
      filter(`Year` ==2010) %>%
      filter(`Country_Name` != "World")%>%
      filter(`Country_Name` != "European Union") %>%
      filter(`Yield, Cereals` <10)

  
  

    
    p<- useYields_gg  %>%
      ggplot(aes(x=value, y=`Yield, Cereals`,color = Nutrient),  text = paste("Country:", Country_Name)) +
      geom_point()
     # geom_point(size = 1.5, shape=1) + 
      # stat_smooth(method = "lm", formula = y ~ x^2, size = 1) +
      # labs(
      #   #title = "Relation between individual application rate and cereal yields", 
      #   x = "Kg applied per Ha", y = "Yield, Cereals, Tons per Ha") 
    p
    
      p+ geom_smooth(method='lm', formula= y~x)

      
      ####################
      
      
      # # HTML(' <a href="https://www.ifpri.org/spotlight/food-fertilizers-and-nutrition-rising-prices-and-global-food-security"><img src="IFPRIPolicySeminar.png" title="Example Image Link" width="20" height="10" /></a> '),
      # tags$a(
      #   href="https://www.ifpri.org/spotlight/food-fertilizers-and-nutrition-rising-prices-and-global-food-security",
      #   tags$img(src="IFPRIPolicySeminar.png",
      #            title="Policy Seminar",
      #            width="600",
      #            height="400")
      # )
      # tags$div(
      #   HTML(' <a href="https://www.ifpri.org/spotlight/food-fertilizers-and-nutrition-rising-prices-and-global-food-security"><img src="IFPRIPolicySeminar.png" title="Example Image Link" width="200" height="150" /></a> ')
      #  # img(src='IFPRIPolicySeminar.png', align = "right"),
      
      