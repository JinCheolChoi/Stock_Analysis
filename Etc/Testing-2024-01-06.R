# i=1241
# 4812.58
# 2022-05-05 06:04:00
# 2022-05-05 08:32:00
# take into account summer time

#********************
rm(list=ls())


#***********
#
# parameters
#
#******************
# working directory
#******************
Device="laptop" # "laptop" or "desktop"

if(Device=="desktop"){
  # desktop
  working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/"
  data.dir="E:/Stock_Data/" # upper folder that has a folder storing stock data
  rdata.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/"
  
  source(paste0("C:/Users/JinCheol Choi/Desktop/R/Functions/Functions.R"))
}else if(Device=="laptop"){
  # laptop
  working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/"
  data.dir="C:/Users/jchoi02/Desktop/Data/" # upper folder that has a folder storing stock data
  rdata.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis_Daily_Data/Rdata/"
  
  source(paste0("C:/Users/jchoi02/Desktop/R/Functions/Functions.R"))
}


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
              "DescTools", # candle chart
              
              "RcppRoll",
              "Rcpp",
              "RcppArmadillo",
              "bench",
              "tseries")){ 
  lapply(pack, checkpackages)
}

# bar size
BarSize="1min"

# import data
BarData=fread(paste0(data.dir, BarSize, "/", Symbols, "/", Symbols, ".csv"))
BarData[, Time:=as.POSIXct(format(as.POSIXct(Time), tz="America/Los_Angeles"),
                           tz="America/Los_Angeles")]

# BarData[, Time:=as.POSIXct(format(as.POSIXct(Time), tz="America/Los_Angeles"), tz="America/Los_Angeles")]

# the number of sub-datasets splited from the original data set
k=15

# regular market time to analyze
Market_Open_Time="05:30:00"
Market_Close_Time="14:00:00"

# Live_Trading
Live_Trading=FALSE

Market_Time=c(1, 2, 3)
Simple_BBands_1_Long_PctB=seq(0.1, 0.3, by=0.05)
Simple_BBands_1_Short_PctB=seq(0.7, 0.9, by=0.05)
Simple_BBands_2_Long_PctB=seq(0.1, 0.3, by=0.05)
Simple_BBands_2_Short_PctB=seq(0.7, 0.9, by=0.05)
Open_Long_Consec_Times=c(4)
Open_Short_Consec_Times=c(4)
Multiplier=100
Reverse=c(TRUE, FALSE)
# Market_Time=2
# Simple_BBands_1_Long_PctB=0.3
# Simple_BBands_1_Short_PctB=0.7
# Simple_BBands_2_Long_PctB=0.25
# Simple_BBands_2_Short_PctB=0.75
# Open_Long_Consec_Times=4
# Open_Short_Consec_Times=4
# Multiplier=100
# Reverse=TRUE
Params=data.table(
  expand.grid(
    Market_Time,
    Simple_BBands_1_Long_PctB,
    Simple_BBands_1_Short_PctB,
    Simple_BBands_2_Long_PctB,
    Simple_BBands_2_Short_PctB,
    Open_Long_Consec_Times,
    Open_Short_Consec_Times,
    Multiplier,
    Reverse
  )
)
Tuning_Parameters=c(
  "Market_Time",
  "Simple_BBands_1_Long_PctB",
  "Simple_BBands_1_Short_PctB",
  "Simple_BBands_2_Long_PctB",
  "Simple_BBands_2_Short_PctB",
  "Open_Long_Consec_Times",
  "Open_Short_Consec_Times",
  "Multiplier",
  "Reverse"
)
colnames(Params)=Tuning_Parameters

#
BarData_Temp=BarData
Trading_Dates=unique(as.Date(BarData_Temp$Time))

i=1241

# import strategies
source(paste0(working.dir, "Strategies.R"))

#*********************
#
# simulation algorithm
#
#***********************************************
# all strategies saved in the global environment
Strategies=ls()[sapply(ls(), function(x) any(class(get(x))=='Strategy'))]

Strategy_Name=Strategies

# common parameters
source(paste0(working.dir, "/Common_Parameters.R"))

All_Results_Temp=Run_Backtesting(Market_Time=Market_Time,
                                 BarData=BarData,
                                 Trading_Dates=Trading_Dates,
                                 Strategy=get(Strategies[which(Strategies==Strategy_Name)]),
                                 Working_Dir=working.dir)

Orders_Transmitted_Temp=do.call(rbind,
                                c(do.call(rbind,
                                          All_Results_Temp$Results)[, "Orders_Transmitted"],
                                  fill=TRUE))

Ind_Profit_Temp=Balance_Calculator(Orders_Transmitted_Temp)$Ind_Profit


Ind_Profit_Temp$Cum_Profit %>% plot



# Time series
BarData[, RSI_Est:=RSI(Close, 10)]
BarData[, RSI_Est_Shift:=shift(RSI_Est, 1)]

Intvl=1:10000
Pred_Intvl=(1:500)+tail(Intvl, 1)
Test=auto.arima(y=BarData[Intvl, Close],
                xreg=BarData[Intvl, RSI_Est_Shift])
# Test %>%
#   forecast(h=10, xreg=BarData[Pred_Intvl, RSI_Est]) %>%
#   autoplot()
# 
# 
# # p-values
# (1-pnorm(abs(Test$coef)/sqrt(diag(Test$var.coef))))*2
# 
# # MSE
# accuracy(Test)
# 
# # observed data
# plot(BarData[Intvl, Close], type='o')
# # predicted data
# lines(Test$fitted, col="red", type="o")


# forecasted data
plot(forecast(Test,
              h=length(Pred_Intvl),
              xreg=BarData[Pred_Intvl, RSI_Est_Shift]),
     col='red',
     type='l')

# observed data
lines(x=(length(Intvl)+1):(length(Intvl)+length(Pred_Intvl)),
      BarData[Pred_Intvl, Close], type='l', col='red')



# observed data
plot(x=1:length(Pred_Intvl),
     BarData[Pred_Intvl, Close], type='l', col='red')


# forecasted data
lines(x=1:length(Pred_Intvl),
      y=forecast(Test,
                 h=10,
                 xreg=BarData[Pred_Intvl, RSI_Est_Shift])$mean,
      col='blue',
      type='l')



