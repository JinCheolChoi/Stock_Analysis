#**************
# save and load
#**************
# save.image(paste0(rdata.dir, "Futures_", as.Date(Sys.time()), " - ", BarSize, ".Rdata"))
# load(paste0(rdata.dir, "Futures_", as.Date(Sys.time()), " - ", BarSize, ".Rdata"))
# load(paste0(rdata.dir, "Futures_2023-07-08 - 5mins.Rdata"))

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
BarSize="1min"

# import data
BarData=fread(paste0(data.dir, BarSize, "/", Symbols, "/", Symbols, ".csv"))
BarData[, Time:=as.POSIXct(format(as.POSIXct(Time), tz="America/Los_Angeles"),
                           tz="America/Los_Angeles")]

# BarData[, Time:=as.POSIXct(format(as.POSIXct(Time), tz="America/Los_Angeles"), tz="America/Los_Angeles")]

# the number of sub-datasets splited from the original data set
k=15

# regular market time to analyze
Market_Open_Time="05:30:00"
Market_Close_Time="14:00:00"

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
                        BarData=BarData[!(Time>=paste0(x, " ", Market_Open_Time)&
                                            Time<paste0(x, " ", Market_Close_Time)), ]
                        
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

# Market_Time=3
# Simple_BBands_1_Long_PctB=0.25
# Simple_BBands_1_Short_PctB=0.7
# Simple_BBands_2_Long_PctB=0.2
# Simple_BBands_2_Short_PctB=0.75
# Open_Long_Consec_Times=4
# Open_Short_Consec_Times=4
# Multiplier=100
# 
# RSI_RSI_MA_Diff_Min=3
# RSI_RSI_MA_Diff_Max=6
# Early_Execution_Gap=7
# Open_N=c(1:9)
# Close_N=c(1:9)
# 
# Reverse=TRUE
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
#     
#     RSI_RSI_MA_Diff_Min,
#     RSI_RSI_MA_Diff_Max,
#     Early_Execution_Gap,
#     Open_N,
#     Close_N,
#     
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
#   
#   "RSI_RSI_MA_Diff_Min",
#   "RSI_RSI_MA_Diff_Max",
#   "Early_Execution_Gap",
#   "Open_N",
#   "Close_N",
#   
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
                      paste0("adjR2_on_", 1:k),
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
    
    # common parameters
    source(paste0(working.dir, "/Common_Parameters.R"))
    
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
      
      Temp$Elapsed_Time[i]=Temp$Elapsed_Time[i]+Time_Elapsed[3]
      
      Orders_Transmitted_Temp=do.call(rbind,
                                      c(do.call(rbind,
                                                Results_Temp)[, "Orders_Transmitted"],
                                        fill=TRUE))
      
      
      if(ncol(Orders_Transmitted_Temp)>1 & !sum(apply(Orders_Transmitted_Temp,
                                                      2,
                                                      function(x){is.na(x)}))==nrow(Orders_Transmitted_Temp)){
        
        Ind_Profit_Temp=Balance_Calculator(Orders_Transmitted_Temp)[["Ind_Profit"]]
        Net_Profit_Temp=Balance_Calculator(Orders_Transmitted_Temp)[["Net_Profit"]]
        
        # save results
        assign("Results_Temp",
               list(Time_Elapsed,
                    list(
                      Orders_Transmitted=Orders_Transmitted_Temp,
                      Ind_Profit=Ind_Profit_Temp,
                      Net_Profit=Net_Profit_Temp
                    )))
        
        # Net_Profit
        Temp[i, paste0("NP_on_", k_ind):=Net_Profit_Temp]
        
        # standard deviation
        Temp[i, paste0("SD_on_", k_ind):=sd(Ind_Profit_Temp[["Daily_Profit"]])]
        
        # adjusted R-squared
        lm_fit=lm(Profit~Time, Ind_Profit_Temp)
        lm_fit_summ=summary(lm_fit)
        Temp[i, paste0("adjR2_on_", k_ind):=lm_fit_summ$adj.r.squared]
        
        # MDD
        if(nrow(Ind_Profit_Temp[!is.na(Cum_Profit)])>0){
          # Max_Loss (same as MDD, but just not percentage)
          # Ind_Profit_Temp[, Max_Loss:=Cum_Profit-sapply(1:nrow(Ind_Profit_Temp),
          #                                         function(x) Ind_Profit_Temp[, min(Cum_Profit[.I>=x])])]
          # Temp[i, paste0(Strategy, "_Training_", "Max_Loss"):=-max(Ind_Profit_Temp$Max_Loss)]
          Temp[i, paste0("MDD_on_", k_ind):=-maxdrawdown(Ind_Profit_Temp[["Cum_Profit"]])[["maxdrawdown"]]]
          
          # minimum Cum_Profit (MCP)
          Temp[i, paste0("MCP_on_", k_ind):=min(Ind_Profit_Temp[["Cum_Profit"]])]
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
    rm(Backtesting_Output,
       Orders_Transmitted_Temp,
       Ind_Profit_Temp,
       Net_Profit_Temp,
       Temp,
       Results_Temp)
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
