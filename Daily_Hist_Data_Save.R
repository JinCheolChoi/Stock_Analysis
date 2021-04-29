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
      "DescTools", # candle chart
      "slackr")){
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

# set up slackr
slackr_setup(
    channel="#stanalytics",
    incoming_webhook_url="https://hooks.slack.com/services/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",
    bot_user_oauth_token='xoxb-2015929948418-2028585561121-7O3bYbbAcNgwSunc8abSWCCT'
    )

#***************
#
# set parameters
#
#***************
# Futures
tws=twsConnect(port=Port)
for(Future in Futures){
  #Future=Futures[1]
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
  Daily_Hist_Data_Save(Contract=contract, Data_Dir=data.dir, Working_Dir=working.dir, Force=T, Log=T) #
  
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
  Daily_Hist_Data_Save(Contract=contract, Data_Dir=data.dir, Working_Dir=working.dir, Force=T, Log=T) #
  
  # print out progress
  setTxtProgressBar(pb, i)
  message("\n")
}

# check if market data of all symbols are saved
Data_Summ=data.table(File=character(), N=numeric())
for(Symbol in c(Futures, Equities)){
  if(file.exists(paste0(data.dir, Symbol, "/", Symbol, "_", as.Date(format(Sys.time(), tz="America/Los_Angeles")), ".csv"))){
    Temp=fread(paste0(data.dir, Symbol, "/", Symbol, "_", as.Date(format(Sys.time(), tz="America/Los_Angeles")), ".csv"))
    
    Data_Summ=rbind(Data_Summ,
                    data.table(
                      File=paste0(Symbol, "_", as.Date(format(Sys.time(), tz="America/Los_Angeles"))),
                      N=nrow(Temp)
                      )
                    )
  }
}

# message
if(nrow(Data_Summ)!=0){
  Message=Data_Summ
  print(Message) # echo Message in terminal
  slackr("New data",
         Message) # send Message to Slack
}else{
  Message=paste0("No new data on ", as.Date(format(Sys.time(), tz="America/Los_Angeles")))
  print(Message) # echo Message in terminal
  slackr(Message) # send Message to Slack
}

# put the systemp on sleep
Sys.sleep(5)


