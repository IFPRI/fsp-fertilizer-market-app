#prices R code from David

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

ticker.description<-read_sheet(url.fertilizer, sheet="Ticker_List")
bloomberg.upload<-read_sheet(url.fertilizer, sheet="Bloomberg_upload")%>%
  mutate(Ticker=substr(type, 1, 14),
         Year=substr(date,1,4),
         Month=substr(date,6,7),
         Day=substr(date,9,10)    )%>%
  left_join(exr.f)%>%
  mutate(value.adj=case_when(grepl("CAD",unit) ~ value/`CAD=X`,
                             grepl("CNY",unit) ~ value/`CNY=X`,
                             grepl("EUR",unit) ~ value/`EUR=X`,
                             grepl("st",unit) ~ value/0.907185,
                             TRUE ~ value))