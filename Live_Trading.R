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
#***********
# working directory
working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/" # desktop
#working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/" # laptop

# account
# margin account="U4524665"
# paper trading account="DU2656942"
Account_Code="DU2656942"

# port
Port=7497 # tws : 7497, IB gateway : 4002

# BarSize
BarSize=12

#*****************
#
# preliminary step
#
#*******************
# required functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))
source(paste0(working.dir, "/Echos/Echo_Live_Trading.R"))

# import packages
for(Package in
    c("IBrokers",
      "TTR",
      "data.table",
      "dplyr",
      "DescTools")){ # candle chart
  checkpackages(Package)
}


#***********************
#
# live trading algorithm
#
#***********************
# contract info
contract=twsFuture("MNQ", "GLOBEX", "202106")
# connect to TWS
tws=twsConnect(port=Port)
# twsDisconnect(tws) # disconnect from TWS
# reqCurrentTime(tws)
# serverVersion(tws)
#
BarData=c()
# BarData5Secs=c()
while(TRUE){
  # connect to tws
  while(!isConnected(tws)){
    tws=twsConnect(port=Port)
  }
  
  # request realtime bar data
  # output : BarData
  ReqRealTimeBars(BarSize)
  
  # candle chart
  #Candle_Chart(BarData)
  
  # determine an action
  
  # place an order

}


z=0
T_1=system.time({
  for(i in 1:10000){
    System_Break()
    Daily_Hist_Data_Save(Force=F)
    z=z+1
  }
})
T_2=system.time({
  for(i in 1:10000){
    System_Break()
    z=z+1
  }
})
