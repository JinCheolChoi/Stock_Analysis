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
log.dir=working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/" # desktop
#working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/" # laptop
data.dir="E:/Stock_Data/" # upper folder that has a folder storing stock data


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
source(paste0(working.dir, "Echos/Echo_Daily_Hist_Data_Save.R"))

# import packages
for(Package in
    c("IBrokers",
      "TTR",
      "data.table",
      "dplyr",
      "DescTools")){ # candle chart
  checkpackages(Package)
}


#***************
#
# set parameters
#
#***************
# Future
Futures=c("MNQ", "MES", "M2K")

# Equity
Equities=c("ARKK", "ARKF", "ARKX", "QQQ", "SPY", "IWM")

# progressbar
pb=txtProgressBar(min=0, max=length(Futures)+length(Equities), style=3)
i=0


#***************
#
# set parameters
#
#***************
# Futures
tws=twsConnect(port=Port)
for(Future in Futures){
  i=i+1
  # connect to tws
  while(!isConnected(tws)){
    tws=twsConnect(port=Port)
  }
  contract=twsFuture(Future, "GLOBEX", "202106") # MNQ : Nasdaq, MES : S&P500, M2K : Russell 2000
  # connect to TWS
  # twsDisconnect(tws) # disconnect from TWS
  # reqCurrentTime(tws)
  # serverVersion(tws)
  
  # execute a daily save of 5 second bar data afterwards
  message("\n")
  Daily_Hist_Data_Save(Contract=contract, Data_Dir=data.dir, Log_Dir=log.dir, Force=T, Log=T) #
  
  # print out progress
  setTxtProgressBar(pb, i)
  message("\n")
}

# Equity
for(Equity in Equities){
  i=i+1
  # connect to tws
  while(!isConnected(tws)){
    tws=twsConnect(port=Port)
  }
  contract=twsEquity(Equity) # MNQ : Nasdaq, MES : S&P500, M2K : Russell 2000
  # connect to TWS
  # twsDisconnect(tws) # disconnect from TWS
  # reqCurrentTime(tws)
  # serverVersion(tws)
  
  # execute a daily save of 5 second bar data afterwards
  message("\n")
  Daily_Hist_Data_Save(Contract=contract, Data_Dir=data.dir, Log_Dir=log.dir, Force=T, Log=T) #
  
  # print out progress
  setTxtProgressBar(pb, i)
  message("\n")
}




