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
source(paste0("C:/Users/JinCheol Choi/Desktop/R/Functions/Functions.R"))

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
         BarSize=60*1,
         Convert_Tz=T,
         Filter=T)

# bar data
# SPY
# Training_BarData=MNQ[Time<"2021-07-22 15:00:00", ]
# Test_BarData=MNQ[Time>="2021-07-22 15:00:00", ]
Training_BarData=MNQ
Test_BarData=MNQ


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
Simple_BBands_1_Long_PctB=seq(0.1, 0.2, by=0.05)
Simple_BBands_1_Short_PctB=seq(0.55, 0.65, by=0.05)
Simple_BBands_2_Long_PctB=seq(0.25, 0.45, by=0.05)
Simple_BBands_2_Short_PctB=seq(0.7, 0.9, by=0.05)

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
  Strategies="Test_Strategy_2"
  # create profit variables for strategies
  if(i==1){
    Additional_Cols=apply(expand.grid(Strategies, c("_NP_on_Training", "_NP_on_Test")), 1, paste, collapse="")
    Temp=setNames(data.table(matrix(nrow=0, ncol=length(Additional_Cols))), Additional_Cols)
    Temp[, (Additional_Cols):=lapply(.SD, as.numeric), .SDcols=Additional_Cols]
    Params=cbind(Params,
                 Temp)
  }
  
  #****************
  # run Backtesting
  #****************
  # for(Strategy in Strategies){
  #   # Strategy=Strategies
  #   # on training data sets
  #   T1=system.time({
  #     Training_Results_Temp=Live_Trading_Imitator(BarData=Training_BarData,
  #                                                 Strategy=get(Strategies[which(Strategies==Strategy)]))
  #   })
  # 
  #   # save results
  #   assign(paste0(Strategy, "_Training_", "Setting_", i),
  #          list(T1,
  #               Training_Results_Temp))
  # 
  #   # save net profits
  #   Params[i, paste0(Strategy, "_NP_on_Training"):=get(paste0(Strategy, "_Training_", "Setting_", i))[[2]]$Net_Profit]
  # 
  #   #******************
  #   # on test data sets
  #   T2=system.time({
  #     Test_Results_Temp=Live_Trading_Imitator(BarData=Test_BarData,
  #                                             Strategy=get(Strategies[which(Strategies==Strategy)]))
  #   })
  # 
  #   # save results
  #   assign(paste0(Strategy, "_Test_", "Setting_", i),
  #          list(T2,
  #               Test_Results_Temp))
  # 
  #   # save net profits
  #   Params[i, paste0(Strategy, "_NP_on_Test"):=get(paste0(Strategy, "_Test_", "Setting_", i))[[2]]$Net_Profit]
  # }
  
  ############################################
  
  
  
  for(Strategy in Strategies){
    # Strategy=Strategies
    # on training data sets
    T1=system.time({
      Training_Results_Temp=Backtesting(BarData=Training_BarData,
                                        Strategy=get(Strategies[which(Strategies==Strategy)]))
    })

    # save results
    assign(paste0(Strategy, "_Training_", "Setting_", i),
           list(T1,
                Training_Results_Temp))

    # save net profits
    Params[i, paste0(Strategy, "_NP_on_Training"):=get(paste0(Strategy, "_Training_", "Setting_", i))[[2]]$Net_Profit]

    #******************
    # on test data sets
    T2=system.time({
      Test_Results_Temp=Backtesting(BarData=Test_BarData,
                                    Strategy=get(Strategies[which(Strategies==Strategy)]))
    })

    # save results
    assign(paste0(Strategy, "_Test_", "Setting_", i),
           list(T2,
                Test_Results_Temp))

    # save net profits
    Params[i, paste0(Strategy, "_NP_on_Test"):=get(paste0(Strategy, "_Test_", "Setting_", i))[[2]]$Net_Profit]
  }
  
  #***************
  # print messages
  #***************
  # progress
  print(paste0(i, " / ", nrow(Params), " (", round(i/nrow(Params)*100, 2), "%)"))
  
  # if(i%%20==0){
  #   save.image("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/Futures_2022-01-06.Rdata")
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
    Data_Temp[, Max_Loss:=Cum_Profit-sapply(1:nrow(Data_Temp),
                                            function(x) Data_Temp[, min(Cum_Profit[.I>=x])])]
    Params[i, paste0(Strategy, "_Training_", "Max_Loss"):=-max(Data_Temp$Max_Loss)]
    
    Data_Temp=get(paste0(Strategy, "_Test_", "Setting_", i))[[2]]$Ind_Profit
    Data_Temp[, Max_Loss:=Cum_Profit-sapply(1:nrow(Data_Temp),
                                            function(x) Data_Temp[, min(Cum_Profit[.I>=x])])]
    Params[i, paste0(Strategy, "_Test_", "Max_Loss"):=-max(Data_Temp$Max_Loss)]
    
    # minimum Cum_Profit
    Data_Temp=get(paste0(Strategy, "_Training_", "Setting_", i))[[2]]$Ind_Profit
    Params[i, paste0(Strategy, "_Training_", "Min_Cum_Profit"):=min(Data_Temp$Cum_Profit)]
    
    Data_Temp=get(paste0(Strategy, "_Test_", "Setting_", i))[[2]]$Ind_Profit
    Params[i, paste0(Strategy, "_Test_", "Min_Cum_Profit"):=min(Data_Temp$Cum_Profit)]
  }
}


#
Params[Stop_Order!=1000&
         Test_Strategy_NP_on_Training>3500&
         Test_Strategy_NP_on_Test>3500&
         Long_Strategy_NP_on_Training>0&
         Long_Strategy_NP_on_Test>0&
         Short_Strategy_NP_on_Training>0&
         Short_Strategy_NP_on_Test>0&
         
         Test_Strategy_Test_Min_Cum_Profit>-1000
       ,
       .SD,
       .SDcols=c("Simple_BBands_1_Long_PctB", "Simple_BBands_2_Short_PctB", "Stop_Order", "Profit_Order",
                 "Test_Strategy_NP_on_Training",
                 "Test_Strategy_Training_Standard_Deviation",
                 "Test_Strategy_Training_Max_Loss",
                 "Test_Strategy_Training_Min_Cum_Profit",
                 
                 
                 "Test_Strategy_NP_on_Test",
                 "Test_Strategy_Test_Standard_Deviation",
                 "Test_Strategy_Test_Max_Loss",
                 "Test_Strategy_Test_Min_Cum_Profit",
                 
                 
                 "Long_Strategy_NP_on_Training",
                 "Long_Strategy_Training_Standard_Deviation",
                 "Long_Strategy_Training_Max_Loss",
                 "Long_Strategy_Training_Min_Cum_Profit",
                 
                 
                 "Long_Strategy_NP_on_Test",
                 "Long_Strategy_Test_Standard_Deviation",
                 "Long_Strategy_Test_Max_Loss",
                 "Long_Strategy_Test_Min_Cum_Profit",
                 
                 
                 "Short_Strategy_NP_on_Training",
                 "Short_Strategy_Training_Standard_Deviation",
                 "Short_Strategy_Training_Max_Loss",
                 "Short_Strategy_Training_Min_Cum_Profit",
                 
                 "Short_Strategy_NP_on_Test",
                 "Short_Strategy_Test_Standard_Deviation",
                 "Short_Strategy_Test_Max_Loss",
                 "Short_Strategy_Test_Min_Cum_Profit",
                 
                 "Row")]

Params$Short_Strategy_Test_Min_Cum_Profit %>% summary
Params[Stop_Order!=1000&
         Test_Strategy_Training_Min_Cum_Profit>0,
       .SD,
       .SDcols=c("Test_Strategy_NP_on_Training", "Test_Strategy_NP_on_Test")] %>% plot


Params[Stop_Order!=1000&
         Test_Strategy_NP_on_Training>2000&
         Test_Strategy_NP_on_Test>2000,
       .SD,
       .SDcols=c("Simple_BBands_1_Long_PctB", "Simple_BBands_2_Short_PctB", "Stop_Order", "Profit_Order",
                 "Test_Strategy_NP_on_Training",
                 "Test_Strategy_NP_on_Test",
                 "Long_Strategy_NP_on_Training",
                 "Long_Strategy_NP_on_Test",
                 "Short_Strategy_NP_on_Training",
                 "Short_Strategy_NP_on_Test")] %>% cor

Params[Stop_Order!=1000&
         Test_Strategy_Test_Min_Cum_Profit>0,Row]
Params[i, ]
i=13
par(mfrow=c(2,1))
get(paste0("Test_Strategy_Training_", "Setting_", i))[[2]]$Ind_Profit[, .SD, .SDcols=c("Date", "Daily_Cum_Profit")] %>% plot(type='o', main="Training")
get(paste0("Test_Strategy_Test_", "Setting_", i))[[2]]$Ind_Profit[, .SD, .SDcols=c("Date", "Daily_Cum_Profit")] %>% plot(type='o', main="Test")
get(paste0("Long_Strategy_Test_", "Setting_", i))[[2]]$Ind_Profit[, .SD, .SDcols=c("Date", "Daily_Cum_Profit")] %>% plot(type='o', main="Test")
get(paste0("Short_Strategy_Test_", "Setting_", i))[[2]]$Ind_Profit[, .SD, .SDcols=c("Date", "Daily_Cum_Profit")] %>% plot(type='o', main="Test")


Params[, .SD, .SDcols=c(colnames(Params)[grepl("NP_on_", colnames(Params))], "Row")]



Params[Net_Profit_on_Training>0, ]
Params[Net_Profit_on_Test>0, ]

Contingency_Table_Generator_Conti_X(Data=Params,
                                    Row_Var="Net_Profit_on_Test",
                                    Col_Var="Stop_Order",
                                    Missing="Not_Include")[c(3, 4),]
Contingency_Table_Generator_Conti_X(Data=Params,
                                    Row_Var="Net_Profit_on_Training",
                                    Col_Var="Stop_Order",
                                    Missing="Not_Include")[c(3, 4),]


# 
# #5548.48
# Params[Stop_Order==10 & Profit_Order==100, ]
# Params$Net_Profit[i]=get(paste0("Training_", "Setting_", i))[[2]]$Net_Profit
# 
# Non_NA_Params=Params[Stop_Order<=10000, ]
# Non_NA_Params[which.max(Net_Profit), ]
# Non_NA_Params$Net_Profit %>% plot
# 
# Non_NA_Params[, c("Stop_Order", "Net_Profit")] %>% plot
# Non_NA_Params[, c("Profit_Order", "Net_Profit")] %>% plot
# 
# 8702.72 #0.15, 0.6, 100, 50
# 3993.2 #0.15, 0.65, 100, 15
# 7876.72 #0.2, 0.6, 100, 50
# 6102.24 #0.15 0.65 100 20
# 
# Params[Simple_BBands_1_Long_PctB<=0.2 &
#          Simple_BBands_2_Short_PctB<=0.65, .SD, .SDcols=c("Profit_Order", "Net_Profit")] %>% plot()
# Temp[order(Net_Profit, decreasing=F), ] %>% head(30)
# Temp[order(Net_Profit, decreasing=T), ] %>% head(30)
# 
# summary(lm(Net_Profit~Simple_BBands_1_Long_PctB+
#              Simple_BBands_2_Short_PctB+
#              Stop_Order+
#              Profit_Order, data=Params))
# #**************
# # save and load
# #**************
# #save.image("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/Futures_2021-09-25_Trend.Rdata")
# #load("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/Futures_2022-01-06_Trend.Rdata")
# 
# #***********************
# # visualize in bar chart
# #***********************
# # convert xts.Collapsed_BarData
# library(quantmod)
# BarData[, Volume:=abs(Net_Volume)]
# xts.Collapsed_BarData=as.xts.data.table(BarData[, -1])
# chartSeries(xts.Collapsed_BarData[1:1000, ],
#             name=Symbols,
#             theme="white")
# 
# 
# 
# lm(Net_Profit~Simple_BBands_1_Long_PctB+
#      Simple_BBands_2_Short_PctB+
#      Stop_Order+
#      Profit_Order, data=Params[!(is.na(Net_Profit)|
#                                    Net_Profit==-Inf), ]) %>% summary
# Params[!(is.na(Net_Profit)|
#            Net_Profit==-Inf),
#        .SD,
#        .SDcols=c("Stop_Order", "Net_Profit")] %>% plot
# Params[!(is.na(Net_Profit)|
#            Net_Profit==-Inf),
#        .SD,
#        .SDcols=c("Profit_Order", "Net_Profit")] %>% plot
# Params[!(is.na(Net_Profit)|
#            Net_Profit==-Inf),
#        .SD,
#        .SDcols=c("Simple_BBands_1_Long_PctB", "Net_Profit")] %>% plot
# Params[!(is.na(Net_Profit)|
#            Net_Profit==-Inf),
#        .SD,
#        .SDcols=c("Simple_BBands_2_Short_PctB", "Net_Profit")] %>% plot
# 
# Params[!(is.na(Net_Profit)|
#            Net_Profit==-Inf)&
#          Stop_Order==1000,
#        .SD,
#        .SDcols=c("Simple_BBands_2_Short_PctB", "Net_Profit")] %>% plot
# 
# Params[!(is.na(Net_Profit)|
#            Net_Profit==-Inf)&
#          Stop_Order==5,
#        .SD,
#        .SDcols=c("Profit_Order", "Net_Profit")] %>% plot
# 
# 
# Params[!(is.na(Net_Profit)|
#            Net_Profit==-Inf)&
#          Stop_Order==5 &
#          Profit_Order==35,
#        .SD,
#        .SDcols=c("Net_Profit")]$Net_Profit %>% hist










