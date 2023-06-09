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
MNQ=fread("C:/Users/jchoi02/Desktop/Data/15mins/MNQ/MNQ.csv")

# MNQ[, Time:=as.POSIXct(format(as.POSIXct(Time), tz="America/Los_Angeles"), tz="America/Los_Angeles")]
Training_BarData=copy(MNQ[1:round(nrow(MNQ)/2)])
Test_BarData=copy(MNQ[(round(nrow(MNQ)/2)+1):nrow(MNQ)])

# Live_Trading
Live_Trading=FALSE

#************
# grid search
#************
Simple_BBands_1_Long_PctB=seq(0.2, 0.4, by=0.05)
Simple_BBands_1_Short_PctB=seq(0.7, 0.9, by=0.05)
Simple_BBands_2_Long_PctB=seq(0.3, 0.5, by=0.05)
Simple_BBands_2_Short_PctB=seq(0.6, 0.8, by=0.05)
RSI_n=c(10, 14, 18)
Reverse=c(TRUE, FALSE)
Params=data.table(
  expand.grid(
    Simple_BBands_1_Long_PctB,
    Simple_BBands_1_Short_PctB,
    Simple_BBands_2_Long_PctB,
    Simple_BBands_2_Short_PctB,
    RSI_n,
    Reverse
  )
)
Tuning_Parameters=c(
  "Simple_BBands_1_Long_PctB",
  "Simple_BBands_1_Short_PctB",
  "Simple_BBands_2_Long_PctB",
  "Simple_BBands_2_Short_PctB",
  "RSI_n",
  "Reverse"
)
colnames(Params)=Tuning_Parameters
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
  # Strategies="Long_Short_Strategy"
  # create profit variables for strategies
  if(i==1){
    Additional_Cols=apply(expand.grid(Strategies, c("_NP_on_Training", "_NP_on_Test")), 1, paste, collapse="")
    Temp=setNames(data.table(matrix(nrow=0, ncol=length(Additional_Cols))), Additional_Cols)
    Temp[, (Additional_Cols):=lapply(.SD, as.numeric), .SDcols=Additional_Cols]
    Params=cbind(Params,
                 Temp)
  }
  
  for(Strategy_Name in Strategies){
    # Strategy_Name="Long_Short_Strategy"
    # Strategy_Name=Strategies
    # on training data sets
    T1_2=system.time({
      Training_Results_Temp=Backtesting(BarData=Training_BarData,
                                        Strategy_Name=Strategy_Name,
                                        Working_Dir=working.dir)
    })
    
    if(i==1){
      Params[, paste0(Strategy_Name, "_NP_on_Training"):=-10000]
    }
    if(Training_Results_Temp[["Net_Profit"]]>0){
      # save results
      assign(paste0(Strategy_Name, "_Training_", "Setting_", i),
             list(T1_2,
                  Training_Results_Temp))
      
      # save net profits
      Params[i, paste0(Strategy_Name, "_NP_on_Training"):=get(paste0(Strategy_Name, "_Training_", "Setting_", i))[[2]]$Net_Profit]
    }
    
    #******************
    # on test data sets
    T2_2=system.time({
      Test_Results_Temp=Backtesting(BarData=Test_BarData,
                                    Strategy_Name=Strategy_Name,
                                    Working_Dir=working.dir)
    })
    
    if(i==1){
      Params[, paste0(Strategy_Name, "_NP_on_Test"):=-10000]
    }
    if(Test_Results_Temp[["Net_Profit"]]>0){
      # save results
      assign(paste0(Strategy_Name, "_Test_", "Setting_", i),
             list(T2_2,
                  Test_Results_Temp))
      
      # save net profits
      Params[i, paste0(Strategy_Name, "_NP_on_Test"):=get(paste0(Strategy_Name, "_Test_", "Setting_", i))[[2]]$Net_Profit]
    }
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
Profitable_Strategies=c()
{
  for(Strategy in Strategies){
    Temp=copy(Params)
    Temp[, paste0("NP_on_Training"):=eval(parse(text=paste0(Strategy, "_NP_on_Training")))]
    Temp[, paste0("NP_on_Test"):=eval(parse(text=paste0(Strategy, "_NP_on_Test")))]
    
    for(i in 1:nrow(Temp)){
      # elapsed time
      Elapsed_Time=0
      Elapsed_Time=Elapsed_Time+get(paste0(Strategy, "_Training_", "Setting_", i))[[1]][3]
      Elapsed_Time=Elapsed_Time+get(paste0(Strategy, "_Test_", "Setting_", i))[[1]][3]
      
      Temp$Elapsed_Time=Elapsed_Time
      
      # standard deviation
      Temp[i, paste0("Training_Standard_Deviation"):=sd(get(paste0(Strategy, "_Training_", "Setting_", i))[[2]]$Ind_Profit$Daily_Profit)]
      Temp[i, paste0("Test_Standard_Deviation"):=sd(get(paste0(Strategy, "_Test_", "Setting_", i))[[2]]$Ind_Profit$Daily_Profit)]
      
      #
      Training_Data_Temp=get(paste0(Strategy, "_Training_", "Setting_", i))[[2]]$Ind_Profit
      if(nrow(Training_Data_Temp[!is.na(Cum_Profit)])>0){
        # Max_Loss (same as MDD, but just not percentage)
        # Training_Data_Temp[, Max_Loss:=Cum_Profit-sapply(1:nrow(Training_Data_Temp),
        #                                         function(x) Training_Data_Temp[, min(Cum_Profit[.I>=x])])]
        # Temp[i, paste0(Strategy, "_Training_", "Max_Loss"):=-max(Training_Data_Temp$Max_Loss)]
        Temp[i, paste0("Training_Maxdrawdown"):=-maxdrawdown(Training_Data_Temp$Cum_Profit)$maxdrawdown]
        
        # minimum Cum_Profit
        Temp[i, paste0("Training_Min_Cum_Profit"):=min(Training_Data_Temp$Cum_Profit)]
      }
      
      Test_Data_Temp=get(paste0(Strategy, "_Test_", "Setting_", i))[[2]]$Ind_Profit
      if(nrow(Test_Data_Temp[!is.na(Cum_Profit)])>0){
        # Test_Data_Temp[, Max_Loss:=Cum_Profit-sapply(1:nrow(Test_Data_Temp),
        #                                         function(x) Test_Data_Temp[, min(Cum_Profit[.I>=x])])]
        # Temp[i, paste0(Strategy, "_Test_", "Max_Loss"):=-max(Test_Data_Temp$Max_Loss)]
        Temp[i, paste0("Test_Maxdrawdown"):=-maxdrawdown(Test_Data_Temp$Cum_Profit)$maxdrawdown]
        
        # minimum Cum_Profit
        Temp[i, paste0("Test_Min_Cum_Profit"):=min(Test_Data_Temp$Cum_Profit)]
      }
      
      if(Temp[i, .SD, .SDcols=paste0("NP_on_Training")]>0 &
         Temp[i, .SD, .SDcols=paste0("NP_on_Test")]>0){
        Temp[i, paste0("Profitable"):=1] # Yes
      }else{
        Temp[i, paste0("Profitable"):=0] # No
      }
    }
    
    assign(
      paste0("Params_", Strategy),
      Temp[, .SD, .SDcols=c(Tuning_Parameters,
                            "Row",
                            "Elapsed_Time",
                            "NP_on_Training",
                            "NP_on_Test",
                            "Training_Standard_Deviation",
                            "Test_Standard_Deviation",
                            "Training_Maxdrawdown",
                            "Training_Min_Cum_Profit",
                            "Test_Maxdrawdown",
                            "Test_Min_Cum_Profit",
                            "Profitable")])
    Temp=get(paste0("Params_", Strategy))
    # models profitable on both training and test data sets
    if(nrow(Temp[apply(Temp[, .SD, .SDcols=paste0("Profitable")], 1, sum)>0, ])>0){
      Profitable_Strategies=rbind(
        Profitable_Strategies,
        data.table(
          Strategy=Strategy,
          Temp[apply(Temp[, .SD, .SDcols=paste0("Profitable")], 1, sum)>0, ]
        )
      )
    }
  }
  Profitable_Strategies[, NP:=NP_on_Training+NP_on_Test]
}

# all profitable strategies
Profitable_Strategies[, .SD[NP==max(NP)]]
Best_Profitable_Strategy=Profitable_Strategies[, .SD[NP==max(NP)]][1]
Profitable_Strategies[NP_on_Training>2000&
                        NP_on_Test>2000, ]

Strategy_Name=Best_Profitable_Strategy[["Strategy"]] # strategy name
Strategy_Name
Best_Profitable_Strategy[, .SD, .SDcols=c(Tuning_Parameters)] # tuning parameters

# double-check
i=Best_Profitable_Strategy[["Row"]]
source(paste0(working.dir, "Strategies.R"))
Strategy_Name=Best_Profitable_Strategy[["Strategy"]]
Training_Results_Temp=Backtesting(BarData=Training_BarData,
                                  Strategy=get(Strategies[which(Strategies==Strategy_Name)]),
                                  Working_Dir=working.dir)
Test_Results_Temp=Backtesting(BarData=Test_BarData,
                              Strategy=get(Strategies[which(Strategies==Strategy_Name)]),
                              Working_Dir=working.dir)

Best_Profitable_Strategy[, c("NP_on_Training", "NP_on_Test")]
c(Training_Results_Temp$Net_Profit, Test_Results_Temp$Net_Profit)

get(paste0(Strategy_Name, "_Training_Setting_", i))[[2]]$Ind_Profit$Cum_Profit %>% plot
get(paste0(Strategy_Name, "_Test_Setting_", i))[[2]]$Ind_Profit$Cum_Profit %>% plot

#**************
# save and load
#**************
#save.image(paste0(rdata.dir, "Futures_2023-06-08.Rdata"))
#load(paste0(rdata.dir, "Futures_2023-06-08.Rdata"))

#*********************************************************
# 1. make trend-based models (ex. Simple_RSI_1 -> Trend_Simple_RSI_1)
# 2. apply Stop_Order & Profit_Order to Backtesting()
# 3. work on "remove redundant long & short signals that are not supposed to be filled" in Backtesting()
# 4. calculate indicators only once prior to fitting models for different parameter settings
# 5. output expense for commissions in Orders_Transmitted
# 6. utilize switch()



# revise the reverse part in Live_Trading.R
# include the reverse parameter in model functions
Restuls_Temp=rbind(Training_Results_Temp$Ind_Profit,
                   Test_Results_Temp$Ind_Profit)

Restuls_Temp[, Date:=as.Date(Time, tz="America/Los_Angeles")]
Restuls_Temp[, Cum_Profit:=cumsum(Profit)]
Restuls_Temp[, Daily_Cum_Profit:=Cum_Profit[Time==max(Time)], by="Date"]
plot(unique(Restuls_Temp[, .SD, .SDcols=c("Date", "Daily_Cum_Profit")]),
     type='o')



#
get(paste0(Strategy_Name, "_Training_Setting_", i))[[2]]$Ind_Profit

#
summary(as.numeric(abs(Collapse_Orders_Transmitted[, Sell_Time-Buy_Time])))

#
library(quantmod)
# create a chart - 1
system.time({
  chartSeries(MNQ[which(as.POSIXlt(MNQ$Time, tz="UTC")>=as.POSIXlt("2021-07-02 07:45:00", tz="UTC") & 
                          as.POSIXlt(MNQ$Time, tz="UTC")<=as.POSIXlt("2021-07-05 02:45:00", tz="UTC")), -1],
              name="MNQ",
              theme="white")
})

system.time({
  chartSeries(MNQ[, -1],
              name="MNQ",
              theme="white")
})

#
MNQ[, Row:=.I]
MNQ[which(as.POSIXlt(MNQ$Time, tz="UTC")>=as.POSIXlt("2021-07-02 07:45:00", tz="UTC") & 
            as.POSIXlt(MNQ$Time, tz="UTC")<=as.POSIXlt("2021-07-05 02:45:00", tz="UTC")), -1]


# verify Profit:=2*(Sell_Price-Buy_Price)-2*Commission is always correct by comparing the profit/loss given Max_Orders > 1 in different scenarios




