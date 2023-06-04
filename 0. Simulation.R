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
# # desktop
# working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/"
# data.dir="E:/Stock_Data/" # upper folder that has a folder storing stock data
# rdata.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/"

# laptop
working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/"
data.dir="C:/Users/jchoi02/Desktop/Data/" # upper folder that has a folder storing stock data
rdata.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis_Daily_Data/Rdata/"


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
# source(paste0("C:/Users/JinCheol Choi/Desktop/R/Functions/Functions.R")) # desktop
source(paste0("C:/Users/jchoi02/Desktop/R/Functions/Functions.R")) # laptop

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

# # import data
# Get_Data(Symbols=list("MNQ"),
#          Data_Dir=data.dir,
#          BarSize=5,
#          Convert_Tz=T,
#          Filter=T)

# bar data
# SPY
# Training_BarData=MNQ[Time<"2021-07-22 15:00:00", ]
# Test_BarData=MNQ[Time>="2021-07-22 15:00:00", ]
# fwrite(MNQ,
#        paste0("E:/Stock_Data/1min/MNQ/MNQ.csv"))
# fwrite(MNQ,
#        paste0("C:/Users/jchoi02/Desktop/Data/1min/MNQ/MNQ.csv"))

# MNQ=fread("E:/Stock_Data/60mins/MNQ/MNQ.csv")
MNQ=fread("C:/Users/jchoi02/Desktop/Data/1min/MNQ/MNQ.csv")

# MNQ[, Time:=as.POSIXct(format(as.POSIXct(Time), tz="America/Los_Angeles"), tz="America/Los_Angeles")]
Training_BarData=copy(MNQ[1:round(nrow(MNQ)/2)])
Test_BarData=copy(MNQ[(round(nrow(MNQ)/2)+1):nrow(MNQ)])

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
# 

# Simple_BBands_1_Long_PctB=seq(0.1, 0.2, by=0.05)
# Simple_BBands_2_Short_PctB=seq(0.55, 0.65, by=0.05)
# Profit_Order=c(seq(30, 100, by=10))
# Stop_Order=c(2*Profit_Order, 1000)
Simple_BBands_1_Long_PctB=seq(0.1, 0.3, by=0.05)
Simple_BBands_1_Short_PctB=seq(0.7, 0.9, by=0.05)
Simple_BBands_2_Long_PctB=seq(0.25, 0.45, by=0.05)
Simple_BBands_2_Short_PctB=seq(0.55, 0.75, by=0.05)

Params=data.table(
  expand.grid(Simple_BBands_1_Long_PctB,
              Simple_BBands_1_Short_PctB,
              Simple_BBands_2_Long_PctB,
              Simple_BBands_2_Short_PctB)
)
colnames(Params)=c("Simple_BBands_1_Long_PctB",
                   "Simple_BBands_1_Short_PctB",
                   "Simple_BBands_2_Long_PctB",
                   "Simple_BBands_2_Short_PctB")

# Live_Trading
Live_Trading=FALSE

# Optimal_Params=data.table(
#   c(0.15, 0.15, 0.2, 0.15, 0.15, 0.15, 0.1, 0.15, 0.15, 0.15, 0.15),
#   c(0.6, 0.65, 0.6, 0.65, 0.6, 0.6, 0.6, 0.55, 0.55, 0.55, 0.65),
#   c(100, 100, 100, 100, 200, 200, 100, 160, 200, 160, 200),
#   c(50, 15, 50, 20, 50, 30, 50, 70, 90, 100, 100)
# )
# colnames(Params)=colnames(Optimal_Params)=c("Simple_BBands_1_Long_PctB",
#                                             "Simple_BBands_2_Short_PctB",
#                                             "Stop_Order",
#                                             "Profit_Order")
# Params=rbind(Params, Optimal_Params)
# Params=Optimal_Params
for(i in 1:nrow(Params)){
  # i=1
  if(Params[i, Simple_BBands_1_Long_PctB]==0 &
     Params[i, Simple_BBands_2_Short_PctB]==1){
    Params$Net_Profit_on_Training[i]=0
    Params$Net_Profit_on_Test[i]=0
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
  
  #****************
  # run Backtesting
  #****************
  # #############################################################################################################
  # Strategies="Test_Strategy_1"
  # # create profit variables for strategies
  # if(i==1){
  #   Additional_Cols=apply(expand.grid(Strategies, c("_NP_on_Training", "_NP_on_Test")), 1, paste, collapse="")
  #   Temp=setNames(data.table(matrix(nrow=0, ncol=length(Additional_Cols))), Additional_Cols)
  #   Temp[, (Additional_Cols):=lapply(.SD, as.numeric), .SDcols=Additional_Cols]
  #   Params=cbind(Params,
  #                Temp)
  # }
  # 
  # for(Strategy_Name in Strategies){
  #   # Strategy_Name=Strategies
  #   # on training data sets
  #   T1_1=system.time({
  #     Training_Results_Temp=Live_Trading_Imitator(BarData=Training_BarData,
  #                                                 Strategy=get(Strategies[which(Strategies==Strategy_Name)]))
  #   })
  #   
  #   # save results
  #   assign(paste0(Strategy_Name, "_Training_", "Setting_", i),
  #          list(T1_1,
  #               Training_Results_Temp))
  # 
  #   # save net profits
  #   Params[i, paste0(Strategy_Name, "_NP_on_Training"):=get(paste0(Strategy_Name, "_Training_", "Setting_", i))[[2]]$Net_Profit]
  # 
  #   #******************
  #   # on test data sets
  #   T2_1=system.time({
  #     Test_Results_Temp=Live_Trading_Imitator(BarData=Test_BarData,
  #                                             Strategy=get(Strategies[which(Strategies==Strategy_Name)]))
  #   })
  # 
  #   # save results
  #   assign(paste0(Strategy_Name, "_Test_", "Setting_", i),
  #          list(T2_1,
  #               Test_Results_Temp))
  # 
  #   # save net profits
  #   Params[i, paste0(Strategy_Name, "_NP_on_Test"):=get(paste0(Strategy_Name, "_Test_", "Setting_", i))[[2]]$Net_Profit]
  # }
  # #############################################################################################################
  
  #############################################################################################################
  # Strategies="Test_Strategy_1"
  # create profit variables for strategies
  if(i==1){
    Additional_Cols=apply(expand.grid(Strategies, c("_NP_on_Training", "_NP_on_Test")), 1, paste, collapse="")
    Temp=setNames(data.table(matrix(nrow=0, ncol=length(Additional_Cols))), Additional_Cols)
    Temp[, (Additional_Cols):=lapply(.SD, as.numeric), .SDcols=Additional_Cols]
    Params=cbind(Params,
                 Temp)
  }
  
  for(Strategy_Name in Strategies){
    # Strategy_Name=Strategies
    # on training data sets
    T1_2=system.time({
      Training_Results_Temp=Backtesting(BarData=Training_BarData,
                                        Strategy=get(Strategies[which(Strategies==Strategy_Name)]))
    })
    
    # save results
    assign(paste0(Strategy_Name, "_Training_", "Setting_", i),
           list(T1_2,
                Training_Results_Temp))
    
    # save net profits
    Params[i, paste0(Strategy_Name, "_NP_on_Training"):=get(paste0(Strategy_Name, "_Training_", "Setting_", i))[[2]]$Net_Profit]
    
    #******************
    # on test data sets
    T2_2=system.time({
      Test_Results_Temp=Backtesting(BarData=Test_BarData,
                                    Strategy=get(Strategies[which(Strategies==Strategy_Name)]))
    })
    
    # save results
    assign(paste0(Strategy_Name, "_Test_", "Setting_", i),
           list(T2_2,
                Test_Results_Temp))
    
    # save net profits
    Params[i, paste0(Strategy_Name, "_NP_on_Test"):=get(paste0(Strategy_Name, "_Test_", "Setting_", i))[[2]]$Net_Profit]
  }
  #############################################################################################################
  
  #***************
  # print messages
  #***************
  # progress
  print(paste0(i, " / ", nrow(Params), " (", round(i/nrow(Params)*100, 2), "%)"))
  
  # if(i%%20==0){
  #   save.image("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/Futures_2022-01-23.Rdata")
  # }
}

#****************************
# calculate useful indicators
#****************************
# row index
Params[, Row:=.I]

Elapsed_Time=0
for(i in 1:nrow(Params)){
  for(Strategy in Strategies){
    # elapsed time
    Elapsed_Time=Elapsed_Time+get(paste0(Strategy, "_Training_", "Setting_", i))[[1]][3]
    Elapsed_Time=Elapsed_Time+get(paste0(Strategy, "_Test_", "Setting_", i))[[1]][3]
    
    # standard deviation
    Params[i, paste0(Strategy, "_Training_Standard_Deviation"):=sd(get(paste0(Strategy, "_Training_", "Setting_", i))[[2]]$Ind_Profit$Daily_Profit)]
    Params[i, paste0(Strategy, "_Test_Standard_Deviation"):=sd(get(paste0(Strategy, "_Test_", "Setting_", i))[[2]]$Ind_Profit$Daily_Profit)]
    
    # Max_Loss (same as MDD, but just not percentage)
    Data_Temp=get(paste0(Strategy, "_Training_", "Setting_", i))[[2]]$Ind_Profit
    # Data_Temp[, Max_Loss:=Cum_Profit-sapply(1:nrow(Data_Temp),
    #                                         function(x) Data_Temp[, min(Cum_Profit[.I>=x])])]
    # Params[i, paste0(Strategy, "_Training_", "Max_Loss"):=-max(Data_Temp$Max_Loss)]
    Params[i, paste0(Strategy, "_Training_", "Max_Loss"):=-maxdrawdown(Data_Temp$Cum_Profit)$maxdrawdown]
    
    Data_Temp=get(paste0(Strategy, "_Test_", "Setting_", i))[[2]]$Ind_Profit
    # Data_Temp[, Max_Loss:=Cum_Profit-sapply(1:nrow(Data_Temp),
    #                                         function(x) Data_Temp[, min(Cum_Profit[.I>=x])])]
    # Params[i, paste0(Strategy, "_Test_", "Max_Loss"):=-max(Data_Temp$Max_Loss)]
    Params[i, paste0(Strategy, "_Test_", "Max_Loss"):=-maxdrawdown(Data_Temp$Cum_Profit)$maxdrawdown]
    
    # minimum Cum_Profit
    Data_Temp=get(paste0(Strategy, "_Training_", "Setting_", i))[[2]]$Ind_Profit
    Params[i, paste0(Strategy, "_Training_", "Min_Cum_Profit"):=min(Data_Temp$Cum_Profit)]
    
    Data_Temp=get(paste0(Strategy, "_Test_", "Setting_", i))[[2]]$Ind_Profit
    Params[i, paste0(Strategy, "_Test_", "Min_Cum_Profit"):=min(Data_Temp$Cum_Profit)]
    
    if(Params[i, .SD, .SDcols=paste0(Strategy, "_NP_on_Training")]>0 &
       Params[i, .SD, .SDcols=paste0(Strategy, "_NP_on_Test")]>0){
      Params[, paste0(Strategy, "_Profitable"):=1] # Yes
    }else{
      Params[, paste0(Strategy, "_Profitable"):=0] # No
    }
  }
}
Params[apply(Params[, .SD, .SDcols=paste0(Strategies, "_Profitable")], 1, sum)>0, ] # models profitable on both training and test data sets


#**************
# save and load
#**************
#save.image(paste0(rdata.dir, "Futures_2023-02-01.Rdata"))
#load(paste0(rdata.dir, "Futures_2023-02-01.Rdata"))



#*********************************************************
# 1. make trend-based models (ex. Simple_RSI_1 -> Trend_Simple_RSI_1)
# 2. apply Stop_Order & Profit_Order to Backtesting()
# 3. work on "remove redundant long & short signals that are not supposed to be filled" in Backtesting()
# 4. calculate indicators only once prior to fitting models for different parameter settings
# 5. output expense for commissions in Orders_Transmitted
# 6. utilize switch()
