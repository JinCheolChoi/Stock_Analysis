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
working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/" # desktop
#working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/", # laptop
data.dir="E:/Stock_Data/" # upper folder that has a folder storing stock data
Symbol="MNQ"
First_Date="2021-01-20"
Last_Date=as.Date(format(Sys.time(), tz="America/Los_Angeles"))
BarSize=60*30 # secs (30 mins bar size seems to need a touch up in the code)

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
# required functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))

# import packages
for(Package in
    c("IBrokers",
      "TTR",
      "data.table",
      "dplyr",
      "DescTools")){ # candle chart
  checkpackages(Package)
}



#************
# import data
#************
# collapse data to the chosen-sized bar data
Get_Data(Symbol,
         Data_Dir=data.dir,
         BarSize,
         First_Date, 
         Last_Date)



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

#*********
# HistData
#*********
# request historical data of 5 seconds bar
HistData.Original=as.data.table(reqHistoricalData(tws, contract, barSize="30 mins", duration="2 M", useRTH="0")) # useRTH="0" : not limited to regular trading hours
colnames(HistData.Original)=c("Time", "Open", "High", "Low", "Close", "Volume", "Wap", "hasGaps", "Count")
HistData.Original[, `:=`(Wap=NULL, hasGaps=NULL)]
Time_From=MNQ$Time[1]
Time_To=tail(MNQ$Time, 1)
# Time_From=as.POSIXct(
#   format(as.POSIXct(paste0(as.Date(format(Sys.time(), tz="America/Los_Angeles"))-2, " 15:00:00"), tz="America/Los_Angeles"),
#          tz=attr(MNQ$Time, "tzone")),
#   tz=attr(MNQ$Time, "tzone")
# )
# Time_To=as.POSIXct(
#   format(as.POSIXct(paste0(as.Date(format(Sys.time(), tz="America/Los_Angeles"))-1, " 15:00:00"), tz="America/Los_Angeles"),
#          tz=attr(MNQ$Time, "tzone")),
#   tz=attr(MNQ$Time, "tzone")
# )

# 
Collapsed_BarData=MNQ[Time>=Time_From &
                        Time<Time_To, ] %>% as.data.frame() %>% as.data.table()

HistData=data.table(Symbol="MNQ",
                    HistData.Original[Time>=Time_From &
                                        Time<Time_To, ] %>% as.data.frame() %>% as.data.table())


#*****
# NOTE
#*****
# There are discrepancies in value between Collapsed_BarData and HistData.
# Despite HistData is imported from TWS, Collapsed_BarData seems more accurate.
sum(Collapsed_BarData!=HistData)

# 
Collapsed_BarData[which(Collapsed_BarData!=HistData, arr.ind=T)[, 1], ]
HistData[which(Collapsed_BarData!=HistData, arr.ind=T)[, 1], ]

#
`5SecsBarHistData`[Time==Collapsed_BarData[which(Collapsed_BarData!=HistData, arr.ind=T)[, 1], "Time"][1]$Time, ]




