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
Device="laptop" # "laptop" or "desktop"

if(Device=="desktop"){
  # desktop
  working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/"
  data.dir="E:/Stock_Data/" # upper folder that has a folder storing stock data
  rdata.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/"
  
  source(paste0("C:/Users/JinCheol Choi/Desktop/R/Functions/Functions.R")) # desktop
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
BarSize="15mins"

# import data
BarData=fread(paste0(data.dir, BarSize, "/", Symbols, "/", Symbols, ".csv"))

# BarData[, Time:=as.POSIXct(format(as.POSIXct(Time), tz="America/Los_Angeles"), tz="America/Los_Angeles")]

# the number of sub-datasets splited from the original data set
k=15

# divide data into k subsets
for(k_ind in 1:k){
  Temp=BarData[which(rep(c(1:k), each=round(nrow(BarData)/k))==k_ind), ]
  Temp=na.omit(Temp)
  assign(
    paste0("BarData_", k_ind),
    Temp
  )
  rm(Temp)
}

# Live_Trading
Live_Trading=FALSE

# repetitive simultation parameters
Sim_N=500
Sample_Size=round(nrow(BarData)/k)
Simulation_Results=c()

#************
# grid search
#************
# Market_Time=c(2)
# Simple_BBands_1_Long_PctB=seq(0.1, 0.3, by=0.05)
# Simple_BBands_1_Short_PctB=seq(0.7, 0.9, by=0.05)
# Simple_BBands_2_Long_PctB=1
# Simple_BBands_2_Short_PctB=1
# Open_Long_Consec_Times=c(4)
# Open_Short_Consec_Times=c(4)
# Multiplier=100
# Reverse=c(TRUE)
# # Market_Time=2
# # Simple_BBands_1_Long_PctB=0.3
# # Simple_BBands_1_Short_PctB=0.7
# # Simple_BBands_2_Long_PctB=0.25
# # Simple_BBands_2_Short_PctB=0.75
# # Open_Long_Consec_Times=4
# # Open_Short_Consec_Times=4
# # Multiplier=100
# # Reverse=TRUE
# Params=data.table(
#   expand.grid(
#     Market_Time,
#     Simple_BBands_1_Long_PctB,
#     Simple_BBands_1_Short_PctB,
#     Simple_BBands_2_Long_PctB,
#     Simple_BBands_2_Short_PctB,
#     Open_Long_Consec_Times,
#     Open_Short_Consec_Times,
#     Multiplier,
#     Reverse
#   )
# )
# Tuning_Parameters=c(
#   "Market_Time",
#   "Simple_BBands_1_Long_PctB",
#   "Simple_BBands_1_Short_PctB",
#   "Simple_BBands_2_Long_PctB",
#   "Simple_BBands_2_Short_PctB",
#   "Open_Long_Consec_Times",
#   "Open_Short_Consec_Times",
#   "Multiplier",
#   "Reverse"
# )

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

# Market_Time=c(1, 2, 3)
# RSI_RSI_MA_Diff_Min=c(1, 2, 3)
# RSI_RSI_MA_Diff_Max=c(4, 5, 6)
# Early_Execution_Gap=c(7, 8, 9, 10)
# Open_N=c(1:5)
# Close_N=c(1:5)
# Reverse=c(FALSE)
# Params=data.table(
#   expand.grid(
#     Market_Time,
#     RSI_RSI_MA_Diff_Min,
#     RSI_RSI_MA_Diff_Max,
#     Early_Execution_Gap,
#     Open_N,
#     Close_N,
#     Reverse
#   )
# )
# Tuning_Parameters=c(
#   "Market_Time",
#   "RSI_RSI_MA_Diff_Min",
#   "RSI_RSI_MA_Diff_Max",
#   "Early_Execution_Gap",
#   "Open_N",
#   "Close_N",
#   "Reverse"
# )
colnames(Params)=Tuning_Parameters
for(i in 1:nrow(Params)){
  # i=1
  
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
  # Strategies="Long_Short_Strategy"
  # create profit variables for strategies
  if(i==1){
    Additional_Cols=c("Row",
                      "Elapsed_Time",
                      paste0("NP_on_", 1:k),
                      paste0("SD_on_", 1:k),
                      paste0("MDD_on_", 1:k),
                      paste0("MCP_on_", 1:k),
                      "Profitable",
                      "NP",
                      "K")
    Temp=setNames(data.table(matrix(nrow=nrow(Params), ncol=length(Additional_Cols))), Additional_Cols)
    Temp[, (Additional_Cols):=lapply(.SD, as.numeric), .SDcols=Additional_Cols]
    Temp[, Row:=.I]
    # Params=cbind(Params,
    #              Temp)
    for(Strategy_Name in Strategies){
      assign(
        paste0("Results_", Strategy_Name),
        Temp
      )
    }
  }
  
  for(Strategy_Name in Strategies){
    # Strategy_Name=Strategies[1]
    assign("Temp",
           get(paste0("Results_", Strategy_Name)))
    
    # elapsed time
    Temp$Elapsed_Time[i]=0
    
    for(k_ind in 1:k){
      Time_Elapsed=system.time({
        Results_Temp=Backtesting(BarData=get(paste0("BarData_", k_ind)),
                                 Strategy_Name=Strategy_Name,
                                 Working_Dir=working.dir)
      })
      
      Temp$Elapsed_Time[i]=Temp$Elapsed_Time[i]+Time_Elapsed[3]
      
      if(!is.na(Results_Temp[["Net_Profit"]])){
        # save results
        assign("Results_Temp",
               list(Time_Elapsed,
                    Results_Temp))
        
        # Net_Profit
        Temp[i, paste0("NP_on_", k_ind):=Results_Temp[[2]]$Net_Profit]
        
        # standard deviation
        Temp[i, paste0("SD_on_", k_ind):=sd(Results_Temp[[2]]$Ind_Profit$Daily_Profit)]
        
        # MDD
        Data_Temp=Results_Temp[[2]]$Ind_Profit
        if(nrow(Data_Temp[!is.na(Cum_Profit)])>0){
          # Max_Loss (same as MDD, but just not percentage)
          # Data_Temp[, Max_Loss:=Cum_Profit-sapply(1:nrow(Data_Temp),
          #                                         function(x) Data_Temp[, min(Cum_Profit[.I>=x])])]
          # Temp[i, paste0(Strategy, "_Training_", "Max_Loss"):=-max(Data_Temp$Max_Loss)]
          Temp[i, paste0("MDD_on_", k_ind):=-maxdrawdown(Data_Temp$Cum_Profit)$maxdrawdown]
          
          # minimum Cum_Profit (MCP)
          Temp[i, paste0("MCP_on_", k_ind):=min(Data_Temp$Cum_Profit)]
        }
      }
    }
    
    # NP
    Temp[i, NP:=sum(Temp[i, .SD, .SDcols=paste0("NP_on_", 1:k)], na.rm=T)]
    
    # K
    Temp[i, K:=sum(Temp[i, .SD, .SDcols=paste0("NP_on_", 1:k)]>0, na.rm=T)]
    
    # Profitable
    if(Temp[i, NP]>0){ # if NPs are positive from all subsets
      Temp[i, paste0("Profitable"):=1] # Yes
    }else{
      Temp[i, paste0("Profitable"):=0] # No
    }
    
    assign(paste0("Results_", Strategy_Name),
           get("Temp"))
    
    # remove
    rm(Data_Temp, Temp, Results_Temp)
  }
  
  #***************
  # print messages
  #***************
  # progress
  print(paste0(i, " / ", nrow(Params), " (", round(i/nrow(Params)*100, 2), "%)"))
  
  # if(i%%20==0){
  #   save.image("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/Futures_2022-01-23.Rdata")
  # }
}

#**************
# save and load
#**************
# save.image(paste0(rdata.dir, "Futures_", as.Date(Sys.time()), " - ", BarSize, ".Rdata"))
# load(paste0(rdata.dir, "Futures_", as.Date(Sys.time()), " - ", BarSize, ".Rdata"))




# #****************************
# # calculate useful indicators
# #****************************
# # row index
# Params[, Row:=.I]
# Profitable_Strategies=c()
# {
#   for(Strategy in Strategies){
#     # Strategy=Strategies
#     Temp=copy(Params)
#     
#     setnames(Temp,
#              paste0(Strategy, paste0("_NP_on_", 1:k)),
#              paste0("NP_on_", 1:k))
#     
#     Temp$Elapsed_Time=0
#     
#     for(i in 1:nrow(Temp)){
#       if(is.na(sum(Temp[i, .SD, .SDcols=paste0("NP_on_", 1:k)]))){ # if there is at least one NA, skip
#         next
#       }
#       if(sum(Temp[i, .SD, .SDcols=paste0("NP_on_", 1:k)]>0, na.rm=T)==0){ # if all NPs are negative, skip
#         next
#       }
#       
#       for(k_ind in 1:k){
#         # elapsed time
#         Temp$Elapsed_Time[i]=Temp$Elapsed_Time[i]+get(paste0(Strategy_Name, "_", k_ind, "_", i))[[1]][3]
#         
#         # standard deviation
#         Temp[i, paste0("SD_on_", k_ind):=sd(get(paste0(Strategy_Name, "_", k_ind, "_", i))[[2]]$Ind_Profit$Daily_Profit)]
#         
#         #
#         Data_Temp=get(paste0(Strategy_Name, "_", k_ind, "_", i))[[2]]$Ind_Profit
#         if(nrow(Data_Temp[!is.na(Cum_Profit)])>0){
#           # Max_Loss (same as MDD, but just not percentage)
#           # Data_Temp[, Max_Loss:=Cum_Profit-sapply(1:nrow(Data_Temp),
#           #                                         function(x) Data_Temp[, min(Cum_Profit[.I>=x])])]
#           # Temp[i, paste0(Strategy, "_Training_", "Max_Loss"):=-max(Data_Temp$Max_Loss)]
#           Temp[i, paste0("MDD_on_", k_ind):=-maxdrawdown(Data_Temp$Cum_Profit)$maxdrawdown]
#           
#           # minimum Cum_Profit (MCP)
#           Temp[i, paste0("MCP_on_", k_ind):=min(Data_Temp$Cum_Profit)]
#         }
#       }
#       
#       # NP
#       Temp[i, NP:=sum(Temp[i, .SD, .SDcols=paste0("NP_on_", 1:k)], na.rm=T)]
#       
#       # K
#       Temp[i, K:=sum(Temp[i, .SD, .SDcols=paste0("NP_on_", 1:k)]>0, na.rm=T)]
#       
#       # Profitable
#       if(Temp[i, NP]>0){ # if NPs are positive from all subsets
#         Temp[i, paste0("Profitable"):=1] # Yes
#       }else{
#         Temp[i, paste0("Profitable"):=0] # No
#       }
#     }
#     
#     if(is.null(Temp[["Elapsed_Time"]])){
#       next
#     }
#     assign(
#       paste0("Results_", Strategy),
#       Temp[, .SD, .SDcols=c(Tuning_Parameters,
#                             "Row",
#                             "Elapsed_Time",
#                             paste0("NP_on_", 1:k),
#                             paste0("SD_on_", 1:k),
#                             paste0("MDD_on_", 1:k),
#                             paste0("MCP_on_", 1:k),
#                             "Profitable",
#                             "NP",
#                             "K")])
#     Temp=get(paste0("Results_", Strategy))
#     
#     # models profitable on all sub-datasets
#     if(nrow(Temp[apply(Temp[, .SD, .SDcols=paste0("Profitable")], 1, sum)>0, ])>0){
#       Profitable_Strategies=rbind(
#         Profitable_Strategies,
#         data.table(
#           Strategy=Strategy,
#           Temp[apply(Temp[, .SD, .SDcols=paste0("Profitable")], 1, sum)>0, ]
#         )
#       )
#     }
#   }
# }

# double-check
for(Strategy_Name in Strategies){
  i=get(paste0("Results_",
               Strategy_Name))[order(NP, decreasing=TRUE)][["Row"]][1]
  source(paste0(working.dir, "Strategies.R"))
  
}
Results_Temp=c()
for(k_ind in 1:k){
  Results_Temp=c(Results_Temp,
                 Backtesting(BarData=get(paste0("BarData_", k_ind)),
                             Strategy_Name=Strategy_Name,
                             Working_Dir=working.dir)$Net_Profit)
}
get(paste0("Results_",
           Strategy_Name))[order(NP, decreasing=TRUE)][, .SD, .SDcols=paste0("NP_on_", 1:k)][1, ]
Results_Temp

apply(
  matrix(1:k),
  1,
  function(x){
    plot(Backtesting(BarData=get(paste0("BarData_", x)),
                     Strategy_Name=Strategy_Name,
                     Working_Dir=working.dir)$Ind_Profit$Time,
         Backtesting(BarData=get(paste0("BarData_", x)),
                     Strategy_Name=Strategy_Name,
                     Working_Dir=working.dir)$Ind_Profit$Cum_Profit,
         main=paste0("Subset_", x),
         xlab="Time",
         ylab="Profit")
  }
)



#*********************************************************
# 1. make trend-based models (ex. Simple_RSI_1 -> Trend_Simple_RSI_1)
# 2. apply Stop_Order & Profit_Order to Backtesting()
# 3. work on "remove redundant long & short signals that are not supposed to be filled" in Backtesting()
# 4. calculate indicators only once prior to fitting models for different parameter settings
# 5. output expense for commissions in Orders_Transmitted
# 6. utilize switch()


#**********************
#
# repetitive simulation
#
#**********************
Top_Ten_Models=get(paste0("Results_",
                          Strategy_Name))[order(NP, decreasing=TRUE)][1:10, ]
for(ind in 1:nrow(Top_Ten_Models)){
  i=Top_Ten_Models$Row[ind]
  # i=1
  
  # import strategies
  source(paste0(working.dir, "Strategies.R"))
  
  #*********************
  # simulation algorithm
  #***********************************************
  # all strategies saved in the global environment
  Strategies=ls()[sapply(ls(), function(x) any(class(get(x))=='Strategy'))]
  
  for(Strategy_Name in Strategies){
    #***********
    # simulation
    Simulation_Results_Temp=c()
    for(sim_n in 1:Sim_N){
      Starting_Point=sample(1:(nrow(BarData)-Sample_Size), 1)
      Ending_Point=Starting_Point+Sample_Size
      
      Simulation_Results_Temp=c(Simulation_Results_Temp,
                                Backtesting(BarData=BarData[Starting_Point:Ending_Point, ],
                                            Strategy_Name=Strategy_Name,
                                            Working_Dir=working.dir)$Net_Profit)
    }
    Simulation_Results=rbind(Simulation_Results,
                             Simulation_Results_Temp)
  }
  
  #***************
  # print messages
  #***************
  # progress
  print(paste0(I, " / ", nrow(Top_Ten_Models), " (", round(I/nrow(Top_Ten_Models)*100, 2), "%)"))
  
  # if(i%%20==0){
  #   save.image("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/Futures_2022-01-23.Rdata")
  # }
}

# summary
apply(Simulation_Results,
      1,
      function(x){
        quantile(x, c(0, 0.05, 0.1))})
apply(Simulation_Results,
      1,
      function(x){
        summary(x)})
apply(Simulation_Results,
      1,
      function(x){
        hist(x, breaks=50)})


#**************
#
# Visualization
#
#**************
i=Top_Ten_Models$Row[1]
# import strategies
source(paste0(working.dir, "Strategies.R"))

All_Results_Temp=Backtesting(BarData=BarData,
                             Strategy=get(Strategies[which(Strategies==Strategy_Name)]),
                             Working_Dir=working.dir)
Orders_Transmitted_Temp=All_Results_Temp$Orders_Transmitted

Ind_Profit_Temp=All_Results_Temp$Ind_Profit

Orders_Transmitted_Temp=Orders_Transmitted_Temp[Filled_Time<=max(Ind_Profit_Temp$Time), ]
Orders_Transmitted_Temp[, Ind:=.I]

plot(All_Results_Temp$Ind_Profit$Time,
     All_Results_Temp$Ind_Profit$Cum_Profit)

# #
# # create a chart - 1
# system.time({
#   chartSeries(BarData[which(as.POSIXlt(BarData$Time, tz="UTC")>=as.POSIXlt("2021-07-02 07:45:00", tz="UTC") & 
#                           as.POSIXlt(BarData$Time, tz="UTC")<=as.POSIXlt("2021-07-05 02:45:00", tz="UTC")), -1],
#               name="BarData",
#               theme="white")
# })
hist(as.numeric(c(Orders_Transmitted_Temp[Detail=="STC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="BTO", ][["Submit_Time"]],
                  Orders_Transmitted_Temp[Detail=="BTC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="STO", ][["Submit_Time"]])),
     breaks=50)
head(c(Orders_Transmitted_Temp[Detail=="STC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="BTO", ][["Submit_Time"]],
       Orders_Transmitted_Temp[Detail=="BTC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="STO", ][["Submit_Time"]]), 10)
as.numeric(c(Orders_Transmitted_Temp[Detail=="STC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="BTO", ][["Submit_Time"]],
             Orders_Transmitted_Temp[Detail=="BTC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="STO", ][["Submit_Time"]])) %>% summary


#***********
# Long graph
library(quantmod)
Ind=145
# elapsed time summary
Orders_Transmitted_Temp[Detail=="STC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="BTO", ][["Submit_Time"]]
which.max(Orders_Transmitted_Temp[Detail=="STC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="BTO", ][["Submit_Time"]])
summary(as.numeric(Orders_Transmitted_Temp[Detail=="STC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="BTO", ][["Submit_Time"]]))
Orders_Transmitted_Temp[Detail=="BTO", ][Ind, ]
Orders_Transmitted_Temp[Detail=="STC",][Ind, ]
Ind_Profit_Temp[Time==Orders_Transmitted_Temp[Detail=="STC",][Ind, Filled_Time], ]
chartSeries(BarData[which(as.POSIXlt(BarData$Time, tz="UTC")>=Orders_Transmitted_Temp[Detail=="BTO", ][Ind, Filled_Time] & 
                            as.POSIXlt(BarData$Time, tz="UTC")<=(Orders_Transmitted_Temp[Detail=="STC",][Ind, Filled_Time]-15*60)), -1],
            name="BarData",
            theme="white")


#************
# Short graph
Ind=161
# elapsed time summary
Orders_Transmitted_Temp[Detail=="BTC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="STO", ][["Submit_Time"]]
which.max(Orders_Transmitted_Temp[Detail=="BTC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="STO", ][["Submit_Time"]])
summary(as.numeric(Orders_Transmitted_Temp[Detail=="BTC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="STO", ][["Submit_Time"]]))
Orders_Transmitted_Temp[Detail=="STO", ][Ind, ]
Orders_Transmitted_Temp[Detail=="BTC",][Ind, ]
Ind_Profit_Temp[Time==Orders_Transmitted_Temp[Detail=="BTC",][Ind, Filled_Time], ]
chartSeries(BarData[which(as.POSIXlt(BarData$Time, tz="UTC")>=Orders_Transmitted_Temp[Detail=="STO", ][Ind, Filled_Time] & 
                            as.POSIXlt(BarData$Time, tz="UTC")<=(Orders_Transmitted_Temp[Detail=="BTC",][Ind, Filled_Time]-15*60)), -1],
            name="BarData",
            theme="white")

# Ind for maximum profit
# if it is long...
Orders_Transmitted_Temp[Detail=="STC",][, .I[Filled_Time==Ind_Profit_Temp[Profit==max(Profit), ][["Time"]]]]
# if it is short...
Orders_Transmitted_Temp[Detail=="BTC",][, .I[Filled_Time==Ind_Profit_Temp[Profit==max(Profit), ][["Time"]]]]

# Ind for maximum loss
# if it is long...
Orders_Transmitted_Temp[Detail=="STC",][, .I[Filled_Time==Ind_Profit_Temp[Profit==min(Profit), ][["Time"]]]]
# if it is short...
Orders_Transmitted_Temp[Detail=="BTC",][, .I[Filled_Time==Ind_Profit_Temp[Profit==min(Profit), ][["Time"]]]]


# 1. try to utilize the Kelly criterion again (apply a condition to construct a probability distribution)...
# 2. combine RSI_Averages_Band with others
