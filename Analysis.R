#********************
#
# empty the workspace
#
#********************
rm(list=ls())

#*******************
#
# set directory path
#
#*******************
CODE.dir.1="C:/Users/JinCheol Choi/Desktop/R/Functions/"
CODE.dir.2="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/"
Data.dir.1="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/Data/"

source(paste0(CODE.dir.1, "Functions.R"))

lapply(c("data.table",
         # "rvest",
         # "stringr",
         "dplyr",
         "magrittr",
         # "RSelenium",
         
         # "profvis",
         # "ggplot2",
         # "readr", # readr::parse_number
         # "reshape2"
         "tidyquant"
         ), # melt() and dcast()
       checkpackages)


#*****************
#
# Import US_Movers
#
#*****************
US_Movers=fread(paste0(Data.dir.1, "US_Movers.csv"))

#
US_Movers=US_Movers[!Ticker%in%c("TPGY U", "MTL PR", "MTBN"), ]
US_Movers[, Date:=as.Date(Date, format="%m/%d/%Y")]
US_Movers=US_Movers[Date<today(), ]


#
TQ_GET=function(x, Ticker, Date){
  return(tq_get(x[Ticker],
                from=as.Date(x[Date])) %>% 
           as.data.frame)
}

#
system.time({
  US_Movers_Stock_Prices=US_Movers %>% 
    apply(1,
          TQ_GET,
          Ticker="Ticker",
          Date="Date")
})


# combine all data frames into one
US_Movers_Stock_Prices=
  do.call(rbind, US_Movers_Stock_Prices) %>% 
  as.data.table()
US_Movers_Stock_Prices=unique(US_Movers_Stock_Prices)
# colnames(US_Movers_Stock_Prices)=c("Symbol", "Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

#
US_Movers_Stock_Prices[, .SD[date==min(date)], by=symbol]
US_Movers_Stock_Prices[, .SD[date==max(date)], by=symbol]

# calculate price change in percentage on the date compared to the first date
US_Movers_Stock_Prices[, `:=`(`price_change(%)`=(close-close[date==min(date)])/close*100,
                              date_gap=date-min(date)),
                       by=symbol]

# add Type column
US_Movers_Stock_Prices %<>% 
  left_join(US_Movers, by=c(symbol="Ticker", date="Date")) %>% 
  as.data.table
# consider only the first type for those that appear as both "Gainer" and "Loser"
US_Movers_Stock_Prices[, Type:=Type[date==min(date)], by=symbol]



################################################
US_Movers_Stock_Prices[Type=="Gainer" &
                         date_gap==1, `price_change(%)`] %>% 
  summary
US_Movers_Stock_Prices[Type=="Loser" &
                         date_gap==1, `price_change(%)`] %>% 
  summary

US_Movers_Stock_Prices[Type=="Loser" &
                         date_gap==1, ]
US_Movers_Stock_Prices[Type=="Loser" &
                         date_gap==5, ]

US_Movers_Stock_Prices[symbol=="MCEP"]

