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
#working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/" # desktop
working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/" # laptop

# account
# margin account="U4524665"
# paper trading account="DU2656942"
Account_Code="DU2656942"

# port
Port=7497 # tws : 7497, IB gateway : 4002


#*****************
#
# preliminary step
#
#*******************
# required functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))
source(paste0(working.dir, "/Echos/Echo_Daily_Hist_Data_Save.R"))

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

# execute a daily save of 5 second bar data afterwards
Daily_Hist_Data_Save(Force=T) #



