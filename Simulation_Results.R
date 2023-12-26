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

load(paste0(rdata.dir, "Futures_2023-07-11 - 15mins_RSI.Rdata"))
# load(paste0(rdata.dir, "Futures_2023-07-22 - 5mins_RSI.Rdata"))



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
BarData[, Time:=as.POSIXct(format(as.POSIXct(Time),
                                  tz="America/Los_Angeles"),
                           tz="America/Los_Angeles")]

# BarData[, Time:=as.POSIXct(format(as.POSIXct(Time), tz="America/Los_Angeles"), tz="America/Los_Angeles")]

# the number of sub-datasets splited from the original data set
k=15

# divide data into k subsets
Unique_Dates=unique(as.Date(BarData$Time))
for(k_ind in 1:k){
  Target_Date=Unique_Dates[rep(c(1:k), each=ceiling(length(Unique_Dates)/k))==k_ind]
  Target_Date=Target_Date[!is.na(Target_Date)]
  Temp=do.call(rbind,
               lapply(Target_Date,
                      #x=Unique_Dates[804]
                      function(x){
                        x=as.Date(x)
                        BarData=BarData[Time>=paste0(x-1, " ", Market_Close_Time)&
                                          Time<paste0(x, " ", Market_Close_Time), ]
                        # BarData=BarData[!(Time>=paste0(x, " ", Market_Open_Time)&
                        #                     Time<paste0(x, " ", Market_Close_Time)), ]
                        
                        BarData[, Ind:=.I]
                      })
  )
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

# double-check
{
  All_Results=c()
  for(Strategy_Name in Strategies){
    Strategy_Results_Temp=get(paste0("Results_",
                                     Strategy_Name))
    Strategy_Results_Temp$Strategy=Strategy_Name
    All_Results=rbind(All_Results,
                      Strategy_Results_Temp)
  }
  
  i=All_Results[order(NP, decreasing=TRUE)][["Row"]][1]
  
  Params[, Ind:=.I]
  # i=All_Results[Params[Market_Time==3, Ind], ][order(NP, decreasing=TRUE)][["Row"]][1]
  
  source(paste0(working.dir, "Strategies.R"))
  
  Strategy_Name=All_Results[order(NP, decreasing=TRUE)][["Strategy"]][1]
  
  # common parameters
  source(paste0(working.dir, "/Common_Parameters.R"))
  
  #
  Additional_Cols=c("Row",
                    "Elapsed_Time",
                    paste0("NP_on_", 1:k),
                    paste0("SD_on_", 1:k),
                    paste0("MDD_on_", 1:k),
                    paste0("MCP_on_", 1:k),
                    "Profitable",
                    "NP",
                    "K")
  Temp=setNames(data.table(matrix(nrow=1, ncol=length(Additional_Cols))), Additional_Cols)
  Temp[, (Additional_Cols):=lapply(.SD, as.numeric), .SDcols=Additional_Cols]
  Temp[, Row:=.I]
  Orders_Transmitted=c()
  
  # elapsed time
  Temp$Elapsed_Time=0
  
  for(k_ind in 1:k){
    # k_ind=1
    BarData_Temp=get(paste0("BarData_", k_ind))
    Trading_Dates=unique(as.Date(BarData_Temp$Time))
    
    # Market_Time=1
    Backtesting_Output=Run_Backtesting(Market_Time=Market_Time,
                                       BarData=BarData_Temp,
                                       Trading_Dates=Trading_Dates,
                                       Strategy_Name=Strategy_Name,
                                       Working_Dir=working.dir)
    Results_Temp=Backtesting_Output$Results
    Time_Elapsed=Backtesting_Output$Time_Elapsed
    
    # Params=cbind(Params,
    #              Temp)
    # for(Strategy_Name in Strategies){
    #   assign(
    #     paste0("Results_", Strategy_Name),
    #     Temp
    #   )
    # }
    
    Temp$Elapsed_Time=Temp$Elapsed_Time+Time_Elapsed[3]
    
    Orders_Transmitted_Temp=do.call(rbind,
                                    c(do.call(rbind,
                                              Results_Temp)[, "Orders_Transmitted"],
                                      fill=TRUE))
    
    Orders_Transmitted=rbind(Orders_Transmitted,
                             Orders_Transmitted_Temp)
    
    if(ncol(Orders_Transmitted_Temp)>1 & !sum(apply(Orders_Transmitted_Temp,
                                                    2,
                                                    function(x){is.na(x)}))==nrow(Orders_Transmitted_Temp)){
      
      Ind_Profit_Temp=Balance_Calculator(Orders_Transmitted_Temp)[["Ind_Profit"]]
      Net_Profit_Temp=Balance_Calculator(Orders_Transmitted_Temp)[["Net_Profit"]]
      
      assign(
        paste0(
          "Ind_Profit_", k_ind
        ),
        Ind_Profit_Temp
      )
      
      # save results
      assign("Results_Temp",
             list(Time_Elapsed,
                  list(
                    Orders_Transmitted=Orders_Transmitted_Temp,
                    Ind_Profit=Ind_Profit_Temp,
                    Net_Profit=Net_Profit_Temp
                  )))
      
      # Net_Profit
      Temp[, paste0("NP_on_", k_ind):=Net_Profit_Temp]
      
      # standard deviation
      Temp[, paste0("SD_on_", k_ind):=sd(Ind_Profit_Temp[["Daily_Profit"]])]
      
      # MDD
      if(nrow(Ind_Profit_Temp[!is.na(Cum_Profit)])>0){
        # Max_Loss (same as MDD, but just not percentage)
        # Ind_Profit_Temp[, Max_Loss:=Cum_Profit-sapply(1:nrow(Ind_Profit_Temp),
        #                                         function(x) Ind_Profit_Temp[, min(Cum_Profit[.I>=x])])]
        # Temp[, paste0(Strategy, "_Training_", "Max_Loss"):=-max(Ind_Profit_Temp$Max_Loss)]
        Temp[, paste0("MDD_on_", k_ind):=-maxdrawdown(Ind_Profit_Temp[["Cum_Profit"]])[["maxdrawdown"]]]
        
        # minimum Cum_Profit (MCP)
        Temp[, paste0("MCP_on_", k_ind):=min(Ind_Profit_Temp[["Cum_Profit"]])]
      }
    }
  }
  
  # remove
  rm(Backtesting_Output,
     Orders_Transmitted_Temp,
     Ind_Profit_Temp,
     Net_Profit_Temp,
     # Temp,
     Results_Temp)
  
  
  print(get(paste0("Results_",
                   Strategy_Name))[order(NP, decreasing=TRUE)][, .SD, .SDcols=paste0("NP_on_", 1:k)][1, ])
  print(Temp[, .SD, .SDcols=paste0("NP_on_", 1:k)][1, ])
  
  # #
  # apply(
  #   matrix(1:k),
  #   1,
  #   function(x){
  #     plot(get(paste0("Ind_Profit_", x))[["Time"]],
  #          get(paste0("Ind_Profit_", x))[["Cum_Profit"]],
  #          main=paste0("Subset_", x),
  #          xlab="Time",
  #          ylab="Profit")
  #   }
  # )
  
  # overall plot
  plot(Balance_Calculator(Orders_Transmitted)[["Ind_Profit"]]$Time,
       Balance_Calculator(Orders_Transmitted)[["Ind_Profit"]]$Cum_Profit)
  
}


#*********************************************************
# 2. apply Stop_Order & Profit_Order to Backtesting()
# 4. calculate indicators only once prior to fitting models for different parameter settings
# 5. output expense for commissions in Orders_Transmitted
# 6. utilize switch()

# #**********************
# #
# # repetitive simulation
# #
# #**********************
# All_Results=c()
# for(Strategy_Name in Strategies){
#   Strategy_Results_Temp=get(paste0("Results_",
#                                    Strategy_Name))
#   Strategy_Results_Temp$Strategy=Strategy_Name
#   All_Results=rbind(All_Results,
#                     Strategy_Results_Temp)
# }
# 
# Top_Ten_Models=c()
# Top_Ten_Models$i=All_Results[order(NP, decreasing=TRUE)][["Row"]][1:10]
# Top_Ten_Models$Strategy_Name=All_Results[order(NP, decreasing=TRUE)][["Strategy"]][1:10]
# for(ind in 1:length(Top_Ten_Models$i)){
#   # i=3
#   i=Top_Ten_Models$i[ind]
#   
#   # import strategies
#   source(paste0(working.dir, "Strategies.R"))
#   
#   Strategy_Name=Top_Ten_Models$Strategy_Name[ind]
#   
#   # common parameters
#   source(paste0(working.dir, "/Common_Parameters.R"))
#   
#   #*********************
#   # simulation algorithm
#   #***********************************************
#   # all strategies saved in the global environment
#   Strategies=ls()[sapply(ls(), function(x) any(class(get(x))=='Strategy'))]
#   
#   #***********
#   # simulation
#   Simulation_Results_Temp=c()
#   for(sim_n in 1:Sim_N){
#     Starting_Point=sample(1:(nrow(BarData)-Sample_Size), 1)
#     Ending_Point=Starting_Point+Sample_Size
#     
#     BarData_Temp=BarData[Starting_Point:Ending_Point, ]
#     Trading_Dates=unique(as.Date(BarData_Temp$Time))
#     
#     Results_Temp=Run_Backtesting(Market_Time=Market_Time,
#                                  BarData=BarData_Temp,
#                                  Trading_Dates=Trading_Dates,
#                                  Strategy_Name=Strategy_Name,
#                                  Working_Dir=working.dir)$Results
#     
#     Orders_Transmitted_Temp=do.call(rbind,
#                                     c(do.call(rbind,
#                                               Results_Temp)[, "Orders_Transmitted"],
#                                       fill=TRUE))
#     
#     if(ncol(Orders_Transmitted_Temp)>1 & !sum(apply(Orders_Transmitted_Temp,
#                                                     2,
#                                                     function(x){is.na(x)}))==nrow(Orders_Transmitted_Temp)){
#       Orders_Transmitted_Temp=Orders_Transmitted_Temp[!is.na(Price)]
#       Net_Profit_Temp=Balance_Calculator(Orders_Transmitted_Temp)[["Net_Profit"]]
#     }else{
#       Net_Profit_Temp=0
#     }
#     Simulation_Results_Temp=c(Simulation_Results_Temp,
#                               Net_Profit_Temp)
#   }
#   Simulation_Results=rbind(Simulation_Results,
#                            Simulation_Results_Temp)
#   
#   #***************
#   # print messages
#   #***************
#   # progress
#   print(paste0(ind, " / ", length(Top_Ten_Models$i), " (", round(ind/length(Top_Ten_Models$i)*100, 2), "%)"))
#   
#   # if(ind%%2==0){
#   #   save.image("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/Futures_2022-01-23.Rdata")
#   # }
#   
#   rm(Orders_Transmitted_Temp,
#      Simulation_Results_Temp,
#      Net_Profit_Temp)
# }
# 
# # summary
# apply(Simulation_Results,
#       1,
#       function(x){
#         quantile(x, c(0, 0.05, 0.1))})
# apply(Simulation_Results,
#       1,
#       function(x){
#         summary(x)})
# apply(Simulation_Results,
#       1,
#       function(x){
#         hist(x, breaks=50)})

#**************
#
# Visualization
#
#**************
All_Results=c()
for(Strategy_Name in Strategies){
  Strategy_Results_Temp=get(paste0("Results_",
                                   Strategy_Name))
  Strategy_Results_Temp$Strategy=Strategy_Name
  All_Results=rbind(All_Results,
                    Strategy_Results_Temp)
}

Top_Ten_Models=c()
All_Results$Market_Time=Params$Market_Time
All_Results=All_Results[Market_Time==3,]
Top_Ten_Models$i=All_Results[order(NP, decreasing=TRUE)][["Row"]][1:10]
Top_Ten_Models$Strategy_Name=All_Results[order(NP, decreasing=TRUE)][["Strategy"]][1:10]

i=Top_Ten_Models$i[1]

# import strategies
source(paste0(working.dir, "Strategies.R"))

Strategy_Name=Top_Ten_Models$Strategy_Name[1]
# common parameters
source(paste0(working.dir, "/Common_Parameters.R"))

BarData_Temp=BarData
Trading_Dates=unique(as.Date(BarData_Temp$Time))

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
# MDD
maxdrawdown(Ind_Profit_Temp[["Cum_Profit"]])

Orders_Transmitted_Temp=Orders_Transmitted_Temp[Filled_Time<=max(Ind_Profit_Temp$Time), ]
Orders_Transmitted_Temp[, Ind:=.I]

plot(Ind_Profit_Temp$Time,
     Ind_Profit_Temp$Cum_Profit)

# Temp[, paste0(Strategy, "_Training_", "Max_Loss"):=-max(Ind_Profit_Temp$Max_Loss)]

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
Ind=6
# elapsed time summary
Orders_Transmitted_Temp[Detail=="STC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="BTO", ][["Submit_Time"]]
which.max(Orders_Transmitted_Temp[Detail=="STC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="BTO", ][["Submit_Time"]])
summary(as.numeric(Orders_Transmitted_Temp[Detail=="STC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="BTO", ][["Submit_Time"]]))
Orders_Transmitted_Temp[Detail=="BTO", ][Ind, ]
Orders_Transmitted_Temp[Detail=="STC",][Ind, ]
Ind_Profit_Temp[Time==Orders_Transmitted_Temp[Detail=="STC",][Ind, Filled_Time], ]
Row_Ind=which(as.POSIXlt(BarData$Time, tz="America/Los_Angeles")>=Orders_Transmitted_Temp[Detail=="BTO", ][Ind, Filled_Time] & 
                as.POSIXlt(BarData$Time, tz="America/Los_Angeles")<=(Orders_Transmitted_Temp[Detail=="STC",][Ind, Filled_Time]-15*60))
chartSeries(BarData[Row_Ind, -1],
            name="BarData",
            theme="white")

#************
# Short graph
Ind=118
# elapsed time summary
Orders_Transmitted_Temp[Detail=="BTC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="STO", ][["Submit_Time"]]
which.max(Orders_Transmitted_Temp[Detail=="BTC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="STO", ][["Submit_Time"]])
summary(as.numeric(Orders_Transmitted_Temp[Detail=="BTC",][["Submit_Time"]]-Orders_Transmitted_Temp[Detail=="STO", ][["Submit_Time"]]))
Orders_Transmitted_Temp[Detail=="STO", ][Ind, ]
Orders_Transmitted_Temp[Detail=="BTC",][Ind, ]
Ind_Profit_Temp[Time==Orders_Transmitted_Temp[Detail=="BTC",][Ind, Filled_Time], ]
Row_Ind=which(as.POSIXlt(BarData$Time, tz="America/Los_Angeles")>=Orders_Transmitted_Temp[Detail=="STO", ][Ind, Filled_Time] & 
                as.POSIXlt(BarData$Time, tz="America/Los_Angeles")<=(Orders_Transmitted_Temp[Detail=="BTC",][Ind, Filled_Time]-15*60))
chartSeries(BarData[Row_Ind, -1],
            name="BarData",
            theme="white")
which(as.POSIXlt(BarData$Time, tz="America/Los_Angeles")>=Orders_Transmitted_Temp[Detail=="STO", ][Ind, Filled_Time]) %>% min
which(as.POSIXlt(BarData$Time, tz="America/Los_Angeles")<=(Orders_Transmitted_Temp[Detail=="BTC",][Ind, Filled_Time]-15*60)) %>% max
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

Ind_Profit_Temp[order(Profit, decreasing = T)]
Ind_Profit_Temp
library(ggplot2)
month=seq(as.Date(min(Ind_Profit_Temp$Time)), 
          as.Date(max(Ind_Profit_Temp$Time)), 
          by = "1 month")
plot(as.Date(Ind_Profit_Temp$Time),
     Ind_Profit_Temp$Cum_Profit,
     xaxt="n")
axis(side=1, at=month, labels=format(month, "%b-%Y"), cex.axis=0.7)

