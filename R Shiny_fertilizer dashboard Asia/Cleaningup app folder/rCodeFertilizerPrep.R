#Fertilizer code
#Date: 17Mar23
#Flow: from DAVID_BLOOMBERG_REQUESTS_FERTILIZERS -> this script -> database -> R shiny tools


#####Loading packages#####
#connecting
library(RMySQL)
#####Connection#####

load(file="./connection.RData")

mysqlconnection = dbConnect(RMySQL::MySQL(),
                            dbname= connection$dbname,
                            host=connection$host,
                            port=connection$port,
                            user=connection$user,
                            password=connection$password
)

# close db connection after function call exits
#on.exit(dbDisconnect(mysqlconnection))

#####Loading data#####

result_fertData <- dbSendQuery(mysqlconnection, "select * from DAVID_BLOOMBERG_REQUESTS_FERTILIZERS") # write query to access the records from a particular table.
fertilizer.price <- fetch(result_fertData, n=-1)

#get tickers
# url.fertilizer="https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8"
# ticker.description<-read_sheet(url.fertilizer, sheet="Ticker_List")
# save(ticker.description, file = "ticker_description.RData")
load(file="./ticker_description.RData")


#####David#####
rm(list=ls())
library(openxlsx)
library(vroom)
library(tidyverse)
library(ggplot2) 
library(googlesheets4)

### Useful for tracker and generic graphs
start.year=2019

# ### load credential to access google sheet
# gs4_auth("d.laborde@cgxchange.org")
# ### Google sheet address
# url.fertilizer="https://docs.google.com/spreadsheets/d/1lXiUYRLjQD_SNohh1uXLFvpryfBcNU_NwamBS1CfVT8"
# 
# ### Simplified external inputs from TRACKER_FAO_processing.R
# load("sourcesR/FAOSTATProcessed.Rdata", verbose=TRUE)

### Functions & Definitions ###
list.months<-c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec")
labeled.months<-c(1:12)
names(labeled.months)<-list.months

fct.corr.prices<-function(base.df) {data.frame(var1 = row.names(base.df%>%
                                                                  select(-DATE)%>%cor()),
                                               base.df%>%select(-DATE)%>%cor())%>%tibble()%>%
    gather(var2,correlation_value,-var1)%>%mutate(correlation_value=round(correlation_value,2))%>%
    ggplot(aes(x=var1, y=var2, fill=correlation_value)) +
    geom_tile(color = "black")+
    geom_text(aes(var1, var2, label = correlation_value), color = "black", size = 4) +
    
    scale_fill_gradient2(low = "white", high = "darkred", mid = "red", 
                         midpoint = 0.65, limit = c(0.3,1), space = "Lab",
                         name="Correlation") +
    theme_minimal() + # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    labs(x="",y="")
}
fn.tag.iso3<-function(desc){
  case_when( grepl("U.S. Gulf",desc) ~ "USA",
             grepl("Cornbelt",desc) ~ "USA",
             grepl("US Gulf",desc) ~ "USA",
             grepl("Western US",desc) ~ "USA",
             grepl("Canada",desc) ~ "CAN",
             grepl("Baltic",desc) ~ "RUS",
             grepl("Black Sea",desc) ~ "RUS",
             grepl("SE Asia",desc) ~ "IDN",
             grepl("Indonesia",desc) ~ "IDN",
             grepl("Brazil",desc) ~ "BRA",
             grepl("Egypt",desc) ~ "EGY",
             grepl("Middle East",desc) ~ "SAU",
             grepl("Europe",desc) ~ "EUR",
             grepl("enelux",desc) ~ "EUR",
             grepl("erman",desc) ~ "EUR",
             grepl("India",desc) ~ "IND",
             grepl("China",desc) ~ "CHN",
             grepl("Isra",desc) ~ "ISR",
             grepl("orocco",desc) ~ "MAR",
             grepl("North Africa",desc) ~ "MAR",
             grepl("Caribbean",desc) ~ "TTO"
  )
}


#### Load Fertistat by product ####
fertiPstat%>%select(Element,`Element Code`)%>%distinct()
fertiPstat%>%dplyr::filter(Year==2019 & `Element Code` == 5910)%>%
  group_by(Item, `Item Code`)%>%summarize(tonnes=sum(Value,na.rm=TRUE))%>%
  ungroup()%>%
  arrange(desc(tonnes))%>%print(n=30)

fertiPstat%>%dplyr::filter(Year==2019 & `Element Code` == 5910 & `Item Code`==4001)%>%
  group_by(Area)%>%summarize(tonnes=sum(Value,na.rm=TRUE))%>%
  ungroup()%>%
  arrange(desc(tonnes))%>%print(n=30)

pstat.prod<-fertiPstat%>%select(Item,`Item Code`)%>%distinct()%>%
  mutate(Product =case_when ( `Item Code` %in% c(4007,4003) ~ "Ammonia",
                              `Item Code` %in% c(4022) ~ "DAP",
                              `Item Code` %in% c(4023) ~ "MAP",
                              `Item Code` %in% c(4001) ~ "Urea",
                              `Item Code` %in% c(4016) ~ "MOP",
                              `Item Code` %in% c(4011) ~ "Phosphate Rock",
                              TRUE ~ "Others"))

ferti.X.products<-fertiPstat%>%dplyr::filter(Year %in% c(2017:2019) & 
                                               `Element Code` %in% c(5910,5922) 
)%>%
  mutate(`Area Code`=as.character(`Area Code`))%>%
  left_join(pstat.prod)%>%
  left_join(country.label%>%select(ISO3_CODE, FAOST_CODE),by=c(`Area Code`="FAOST_CODE"))%>%
  # manual consolidation for gulf countries
  mutate( ISO3_CODE = ifelse( ISO3_CODE=="QAT","SAU" ,ISO3_CODE))%>%
  group_by(ISO3_CODE, Element,Product)%>%
  summarize(Value=sum( Value/3 , na.rm=TRUE))%>%
  ungroup()%>%spread(Element,Value)
ferti.X.products.world<-fertiPstat%>%dplyr::filter(Year %in% c(2017:2019) & 
                                                     `Element Code` %in% c(5910,5922) 
)%>%
  left_join(pstat.prod)%>%
  group_by(Element,Product)%>%
  summarize(Value=sum( Value/3 , na.rm=TRUE))%>%
  ungroup()%>%spread(Element,Value)%>%
  mutate(share=`Export Value`/sum(`Export Value`,na.rm=TRUE))

#keep below##########################
#### Load EXRate data ####
library(tidyquant)

exr.ticker = c("CNY=X","EUR=X","CAD=X")

exr <- tq_get(exr.ticker,
              from = "2015-01-01",
              to = Sys.Date()-3,
              get = "stock.prices")%>%
  select(symbol, date, adjusted)%>%
  spread(symbol,adjusted)

exr.f<-exr%>%fill()

#### Read Bloomberg Tickers and Prices ####

bloomberg.upload<-fertilizer.price%>%
  mutate(Ticker=substr(type, 1, 14),
         Year=substr(date,1,4),
         Month=substr(date,6,7),
         Day=substr(date,9,10)    )%>%
  mutate(date=as.Date(date, "%Y-%m-%d")) %>%
  
  left_join(exr.f)%>%
  mutate(value.adj=case_when(grepl("CAD",unit) ~ value/`CAD=X`,
                             grepl("CNY",unit) ~ value/`CNY=X`,
                             grepl("EUR",unit) ~ value/`EUR=X`,
                             grepl("st",unit) ~ value/0.907185,
                             TRUE ~ value))
# check contents - Imports
table(bloomberg.upload$unit)
table(bloomberg.upload$type)
table(bloomberg.upload$Ticker)
table(ticker.description$Product)

### Selection ###

selected.N<-c("GCFPURMG Index","GCFPURID Index","GCFPURGB Index",
              "GCFPAMNB Index","GCFPAMME Index","GCFPURCB Index")
selected.DAPMAP<-c("GCFPDABA Index", "GCFPDAMO Index","GCFPDANO Index",
                   "GCFPMABA Index", "GCFPMANO Index")
selected.P<-c("GCFPPHNA Index")

#Potash
selected.K<-c("GCFPGMW Index","GCFPPOCE Index") #,"GCFPPOIS Index") ISR: appears to be quaterly

### Analysis  and selection done in TRACLER_Prices_Analysis.R - not needed here

### Consolidated
all.selected <-c(selected.N,selected.P,selected.K,selected.DAPMAP, "")

summary.prices.desc<-ticker.description%>%dplyr::filter( Ticker %in% all.selected )%>%
  arrange(Nutrient,Product)%>%
  mutate(ISO3_CODE=fn.tag.iso3(Description))%>%
  select(Ticker, Description, Product, Nutrient, ISO3_CODE)%>%
  left_join(ferti.X.products)%>%
  rename(XQU=`Export Quantity`, XVU=`Export Value`)%>%
  left_join(ferti.X.products.world)%>%
  rename(XQ=`Export Quantity`, XV=`Export Value`)

summary.prices.desc%>%print(n=20)

clean.0<-bloomberg.upload%>%dplyr::filter(Ticker %in% all.selected & Year >=start.year)%>%
  left_join(summary.prices.desc)%>%select(Ticker, date, Year, Month, Day, ISO3_CODE, Product, Nutrient, value.adj, XVU)%>%
  na.omit()

index.0<-clean.0%>%dplyr::filter(Year==2019)%>%group_by(Ticker)%>%
  summarize( Index2019=mean(value.adj, na.rm=TRUE))

index.0%>%dplyr::filter( Ticker %in% c(all.selected,"TZTU2 Comdty"))

clean.cpt<-clean.0%>%select(Product,date)%>%
  group_by(Product,date)%>%
  summarize(cpt=n())%>%ungroup()%>%
  group_by(Product)%>%mutate(maxi=max(cpt, na.rm=TRUE))%>%
  ungroup()%>%dplyr::filter( maxi!=cpt )%>%mutate(missing=1)

clean.1<-clean.0%>%
  left_join(clean.cpt)%>%
  left_join(index.0)%>%
  mutate(missing=coalesce(missing,0))%>%
  dplyr::filter(missing !=1)%>%
  mutate( value.norm=value.adj/Index2019*100 )

clean.1%>%select(Ticker, Product)%>%distinct()%>%
  group_by(Product)%>%
  summarise( dl=n())

# Compute average
product.avg<-clean.1%>%
  group_by(Nutrient,Product,date, Year, Month, Day)%>%
  summarize(Index.w=weighted.mean(value.norm, XVU, na.rm=TRUE),
            Index=mean(value.norm, na.rm=TRUE),
            Prices.w=weighted.mean(value.adj, XVU, na.rm=TRUE),
            Prices=mean(value.adj, na.rm=TRUE)
  )%>%ungroup()

nutrient.avg<-product.avg%>%left_join(ferti.X.products.world)%>%
  group_by(Nutrient,date, Year, Month, Day)%>%
  summarize(Index.w=weighted.mean(Index.w, `Export Value`, na.rm=TRUE),
            Index=mean(Index, na.rm=TRUE)
            
  )%>%ungroup()%>%
  mutate(Product=Nutrient)


npk.avg<-nutrient.avg%>%left_join(ferti.X.products.world%>%left_join(product.avg%>%
                                                                       select(Nutrient,Product)%>%
                                                                       distinct())%>%
                                    group_by(Nutrient)%>%
                                    summarize(EV=sum(`Export Value`, na.rm=TRUE))%>%
                                    ungroup())%>%
  group_by(date, Year, Month, Day)%>%
  summarize(Index.w=weighted.mean(Index.w, EV, na.rm=TRUE),
            Index=mean(Index, na.rm=TRUE)
  )%>%ungroup()%>%
  mutate(Product="NPK Index")




#### Load Natural gas prices  ####
### US ->> Should we include in Bloomberg ticker
ngp.us <- tq_get("NG=F",
                 from = "2010-01-01",
                 to = Sys.Date()-3,
                 get = "stock.prices")%>%
  select(symbol, date, adjusted)%>%
  spread(symbol,adjusted)%>%
  mutate(Product="Natural Gas - USA",
         variable="USDpermmBTU")%>%
  mutate(Year=substr(date, 1,4), Month=substr(date,6,7), Day=substr(date,9,10))%>%
  mutate(value=`NG=F`)%>%select(-`NG=F`)

### NGP EU Prices
ngp.eu<-bloomberg.upload%>%dplyr::filter(grepl("TZTX2",Ticker))%>%
  select(date,Year,Month,Day,value.adj)%>%
  mutate(variable="USDpermmBTU",
         Product="Natural Gas - EU",
         # Mwh to mmBTU
         value=value.adj/3.412142)%>%select(-value.adj)


#### Combined NGP

ngp.all<-rbind(
  ngp.eu,
  ngp.us,
  rbind(ngp.eu,ngp.us)%>%left_join(rbind(ngp.eu,ngp.us)%>%dplyr::filter(Year==2019)%>%
                                     group_by(Product)%>%summarize(REF2019=mean(value,na.rm=TRUE)))%>%
    mutate(variable="Index",
           value=value/REF2019*100)%>%select(-REF2019)
)%>%mutate(category="FertilizerPrices",
           cdate=as.Date(paste0(Year,"-",Month,"-",15)))
unique(ngp.all$Variable)
#check evolution NGP
fig.natural.gas<- ggplot(data=ngp.all%>%dplyr::filter(date>=as.Date("2021/01/01") 
                                                      & grepl("BTU",variable)), 
                         aes(x=date, y=value, color=Product, group=Product))+
  geom_point()+
  geom_line()+
  labs(y="USD por mmBTU", x="Date")+
  scale_color_brewer(palette="Set2")+
  theme_light()+
  labs(title="Evolution of natural gas prices",
       caption="Dutch TTF natural gas contract for the EU, and U.S. Henry hub spot prices.")
fig.natural.gas
ggsave("output/Fig_PriceNG.png", device="png",fig.natural.gas,width = 6, height = 4)


ggplot(data=ngp.all%>%dplyr::filter(date>=as.Date("2021/01/01") & grepl("Index",variable)), aes(x=date, y=value, color=Product))+
  geom_point()

### Exports to GoogleSheet ###

consolidated.prices<-rbind(
  product.avg%>%select(-Index,-Nutrient, -Prices)%>%gather(Variable, value,-c(Product, date, Year, Month, Day)),
  nutrient.avg%>%select(-Nutrient, -Index)%>%gather(Variable, value,-c(Product, date, Year, Month, Day)), 
  npk.avg%>%select(-Index)%>%gather(Variable, value,-c(Product, date, Year, Month, Day)) )%>%
  mutate(Variable=case_when(grepl("Price",Variable) ~ "PriceMT", 
                            grepl("Index",Variable) ~ "Index",
                            TRUE ~ Variable))%>%
  rename(variable=Variable)%>%
  rbind(ngp.all%>%select(-cdate,-category))%>%
  mutate( category="FertilizerPrices",
          date=as.Date(date),
          cdate=as.character(date))
table(consolidated.prices$Product, consolidated.prices$variable)

fertilizer.prices<-consolidated.prices%>%
  mutate(cdate=as.Date(paste0(Year,"-",Month,"-",15)))%>%
  rbind(ngp.all)%>%
  mutate(Year=as.double(Year))

max(fertilizer.prices$cdate)
print(paste0("Number of rows for prices: ", nrow(fertilizer.prices)))
write_sheet(fertilizer.prices,ss=url.fertilizer,sheet="Prices")


#### Output graphs and summary analysis after processing for comparison and validation
library(ggplot2)
product.avg%>%dplyr::filter(date> as.Date("2021-01-01"))%>%
  select(date,Index.w,Index,Product)%>%
  gather(Variab,value,-c(Product, date))%>%
  ggplot( aes(x=date, y=value, colour=Variab))+
  facet_wrap(~ Product)+
  geom_point()
ggsave(filename="WghtOrNotProduct.png", device="png")

nutrient.avg%>%dplyr::filter(date> as.Date("2021-01-01"))%>%
  select(date,Index.w,Index,Product)%>%
  gather(Variab,value,-c(Product, date))%>%
  ggplot( aes(x=date, y=value, colour=Variab))+
  facet_wrap(~ Product)+
  geom_point()
ggsave(filename="WghtOrNotNutri.png", device="png")

unique(consolidated.prices$Product)
unique(consolidated.prices$variable)
cat1<-c("N","P","K","NPK Index")
cat2<-c("Ammonia","Urea","MOP","DAP","MAP","Phosphate Rock")

fig.prices.nutri<-consolidated.prices%>%
  dplyr::filter( variable =="Index" & Product %in% cat1)%>%
  ggplot(aes(x=date, y=value, color=Product))+
  geom_line()+
  labs(y="Index 100 - average 2019", x="Date")+
  theme_light()
fig.prices.nutri
consolidated.prices%>%dplyr::filter( variable =="Index" & Product %in% cat1)%>%write.csv(file="output/Fig_PriceNutri.csv",
                                                                                         row.names=FALSE)
consolidated.prices%>%dplyr::filter(Product=="Urea" & grepl("MT",variable))%>%write.csv(file="output/Fig_Urea.csv",
                                                                                        row.names=FALSE)

fig.prices.prod<-consolidated.prices%>%
  dplyr::filter( variable =="PriceMT" & Product %in% cat2)%>%
  ggplot(aes(x=date, y=value, color=Product))+
  geom_line()+
  labs(y="USD per metric tons", x="Date")+
  scale_color_brewer(palette="Set2")+
  theme_light()
fig.prices.prod

consolidated.prices%>%group_by(Product)%>%slice_max(n=1,order_by=date      )
ggsave("output/Fig_PriceNutri.png", device="png",fig.prices.nutri,width = 6, height = 4)
ggsave("output/Fig_PriceProd.png", device="png",fig.prices.prod,width = 6, height = 4)

#### Urea NG prices ####

urea.subselect<-c("GCFPURGB Index","GCFPURME Index","GCFPURMG Index","GCFPURMP Index")
bloomberg.upload%>%select(type, tickerName )%>%distinct()%>%dplyr::filter(grepl("rea",type))
coef.technique=55
urea.comparison<-ngp.all%>%select(date,cdate,variable,Product,value)%>%
  dplyr::filter( grepl("BTU",variable))%>%
  spread(Product,value)%>%na.omit()%>%select(-variable)%>%
  left_join(bloomberg.upload%>%dplyr::filter(tickerName %in% urea.subselect)%>%
              select(date,value.adj, type)%>%spread(type,value.adj))%>%na.omit()%>%
  mutate(`USA`=`GCFPURGB Index - Urea US Gulf NOLA Granular prices`/`Natural Gas - USA`/coef.technique,
         `European Union`=`GCFPURMP Index - Mediterranean Urea CFR Prill Spot Price`/`Natural Gas - EU`/coef.technique)
colnames(urea.comparison)

fig.urea.gas<-urea.comparison%>%
  select(date,cdate,USA,`European Union`)%>%
  gather(variable,value,-c(date,cdate))%>%
  ggplot(aes(x=date, y=value, color=variable))+
  geom_line()+
  labs(y="Price ratio", x="Date")+
  scale_color_brewer(palette="Set2")+
  theme_light()+
  labs(title="Price ratio between urea and natural gas",
       subtitle="Ratio computed assuming 55 million BTU of natural gas per metric ton of urea")
fig.urea.gas
ggsave("output/Fig_PriceUreaNG.png", device="png",fig.urea.gas,width = 6, height = 4)

