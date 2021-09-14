#********************
#
# empty the workspace
#
#********************
rm(list=ls())


#***********
#
# parameters
#
#******************
# working directory
#******************
working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/" # desktop
data.dir="E:/Stock_Data/" # upper folder that has a folder storing stock data
# working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/" # laptop
# data.dir="C:/Users/jchoi02/Desktop/Data/" # upper folder that has a folder storing stock data


#****************
# data parameters
#****************
Symbols=c("MNQ")


#*****************
#
# preliminary step
#
#*******************
# load functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))

# import libraries
for(pack in c("IBrokers",
              "TTR",
              "data.table",
              "dplyr",
              "DescTools")){ # candle chart
  lapply(pack, checkpackages)
}

# import data
Get_Data(Symbols=list("MNQ"),
         Data_Dir=data.dir,
         BarSize=60*15,
         Convert_Tz=T,
         Filter=T)

# bar data
# SPY
BarData=MNQ

#************
# grid search
#************
# Simple_BBands_1_Long_PctB=c(0.25)
# Simple_BBands_2_Short_PctB=c(0.7)
# Stop_Order=c(1000000, 10, seq(20, 200, by=20))
# Profit_Order=c(5, seq(10, 200, by=5))


# Simple_BBands_1_Long_PctB=seq(0.05, 0.95, by=0.05)
# Simple_BBands_2_Short_PctB=seq(0.05, 0.95, by=0.05)
# Stop_Order=c(1000000)
# Profit_Order=c(120)

Simple_BBands_1_Long_PctB=0.15
Simple_BBands_2_Short_PctB=0.65
Stop_Order=c(100)
Profit_Order=c(15)


Simple_BBands_1_Long_PctB=seq(0.05, 0.4, by=0.05)
Simple_BBands_2_Short_PctB=seq(0.6, 0.95, by=0.05)
Stop_Order=c(1000, 5, seq(10, 100, by=10))
Profit_Order=c(5, seq(10, 50, by=5))

Params=data.table(
  expand.grid(Simple_BBands_1_Long_PctB,
              Simple_BBands_2_Short_PctB,
              Stop_Order,
              Profit_Order),
  NA
)

colnames(Params)=c("Simple_BBands_1_Long_PctB",
                   "Simple_BBands_2_Short_PctB",
                   "Stop_Order",
                   "Profit_Order",
                   "Net_Profit")

Net_Profit=c()
for(i in 1:nrow(Params)){
  if(Params[i, Simple_BBands_1_Long_PctB]==0 &
     Params[i, Simple_BBands_2_Short_PctB]==1){
    Params$Net_Profit[i]=0
    next
  }
  
  # import strategies
  source(paste0(working.dir, "Strategies.R"))
  
  #*********************
  #
  # simulation algorithm
  #
  #***********************************************
  # all strategies saved in the global environment
  Strategies=ls()[sapply(ls(), function(x) any(class(get(x))=='Strategy'))]
  
  # run Backtesting
  T1=system.time({
    Sim_Results=Live_Trading_Imitator(BarData=BarData,
                                      Strategy=get(Strategies[which(Strategies=="Test_Strategy")]))
  })
  
  # save results
  assign(paste0("Setting_", i),
         list(T1,
              Sim_Results))
  
  # save net profits
  Params$Net_Profit[i]=get(paste0("Setting_", i))[[2]]$Net_Profit
  
  # print the progress
  print(paste0(i, " / ", nrow(Params), " (", round(i/nrow(Params)*100, 2), "%)"))
  
  if(i%%500==0){
    save.image("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/Futures_2021-08-17_Trend.Rdata")
  }
}

#Sim_Results$Ind_Profit[, .SD, .SDcols=c("Time", "Cum_Profit")] %>% plot(type='o')
Params[, Row:=1:nrow(Params)]
Temp=Params[!is.na(Net_Profit), ]


Temp=Params[Stop_Order<=10000 &
              Stop_Order>Profit_Order &
              Stop_Order<100&
              Profit_Order<100, ]

Temp=Params[Stop_Order<=10000 &
              Stop_Order<Profit_Order, ]

Temp=Params[Stop_Order<1000, ]

i=Temp[Net_Profit==sort(Temp$Net_Profit, decreasing=T)[1], Row]
Temp[order(Net_Profit, decreasing=T), ] %>% head(30)

Params[Row>=(i-10) &
         Row<=(i+10), ]
Params[i, ]

Temp[Stop_Order==0, Net_Profit] %>% summary
get(paste0("Setting_", i))[[2]]$Net_Profit
#get(paste0("Setting_", i))[[2]]$Ind_Profit[, .SD, .SDcols=c("Date", "Cum_Profit")] %>% plot(type='o')
get(paste0("Setting_", i))[[2]]$Ind_Profit[, .SD, .SDcols=c("Date", "Daily_Cum_Profit")] %>% plot(type='o')
unique(get(paste0("Setting_", i))[[2]]$Ind_Profit[, .SD, .SDcols=c("Date", "Daily_Profit")])
unique(get(paste0("Setting_", i))[[2]]$Ind_Profit[, .SD, .SDcols=c("Date", "Daily_Profit")]) %>% plot(type="o")



Params[Stop_Order==10 & Profit_Order==100, ]
Params$Net_Profit[i]=get(paste0("Setting_", i))[[2]]$Net_Profit

Non_NA_Params=Params[Stop_Order<=10000, ]
Non_NA_Params[which.max(Net_Profit), ]
Non_NA_Params$Net_Profit %>% plot

Non_NA_Params[, c("Stop_Order", "Net_Profit")] %>% plot
Non_NA_Params[, c("Profit_Order", "Net_Profit")] %>% plot

7211.2    # 0.5
# 0.75
# 10
# 120


#**************
# save and load
#**************
#save.image("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/Futures_2021-08-14_Trend.Rdata")
#load("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/Futures_2021-08-17_Trend.Rdata")

#***********************
# visualize in bar chart
#***********************
# convert xts.Collapsed_BarData
library(quantmod)
BarData[, Volume:=abs(Net_Volume)]
xts.Collapsed_BarData=as.xts.data.table(BarData[, -1])
chartSeries(xts.Collapsed_BarData[1:1000, ],
            name=Symbols,
            theme="white")



lm(Net_Profit~Simple_BBands_1_Long_PctB+
     Simple_BBands_2_Short_PctB+
     Stop_Order+
     Profit_Order, data=Params[!(is.na(Net_Profit)|
                                   Net_Profit==-Inf), ]) %>% summary
Params[!(is.na(Net_Profit)|
           Net_Profit==-Inf),
       .SD,
       .SDcols=c("Stop_Order", "Net_Profit")] %>% plot
Params[!(is.na(Net_Profit)|
           Net_Profit==-Inf),
       .SD,
       .SDcols=c("Profit_Order", "Net_Profit")] %>% plot
Params[!(is.na(Net_Profit)|
           Net_Profit==-Inf),
       .SD,
       .SDcols=c("Simple_BBands_1_Long_PctB", "Net_Profit")] %>% plot
Params[!(is.na(Net_Profit)|
           Net_Profit==-Inf),
       .SD,
       .SDcols=c("Simple_BBands_2_Short_PctB", "Net_Profit")] %>% plot

Params[!(is.na(Net_Profit)|
           Net_Profit==-Inf)&
         Stop_Order==1000,
       .SD,
       .SDcols=c("Simple_BBands_2_Short_PctB", "Net_Profit")] %>% plot

Params[!(is.na(Net_Profit)|
           Net_Profit==-Inf)&
         Stop_Order==5,
       .SD,
       .SDcols=c("Profit_Order", "Net_Profit")] %>% plot


Params[!(is.na(Net_Profit)|
           Net_Profit==-Inf)&
         Stop_Order==5 &
         Profit_Order==35,
       .SD,
       .SDcols=c("Net_Profit")]$Net_Profit %>% hist







