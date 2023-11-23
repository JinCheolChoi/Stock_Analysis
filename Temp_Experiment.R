#**************
# save and load
#**************
# save.image(paste0(rdata.dir, "Temp_Experiment.Rdata"))
# load(paste0(rdata.dir, "Temp_Experiment.Rdata"))

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

# RSI_Threshold=seq(10, 90, by=5)
# Consec_N=c(1:10)
RSI_Threshold=c(30, 70)
Consec_N=c(1, 2)

Market_Open_Time="05:30:00"
Market_Close_Time="14:00:00"

Output=data.table(expand.grid(
  RSI_Threshold=RSI_Threshold,
  Consec_N=Consec_N,
  Percentage=NaN,
  Mean=NaN,
  N=NaN
))

Output[, Ind:=.I]

Trading_Dates=unique(as.Date(BarData$Time))

# Time_Unit
Time_Unit=BarData$Time[2]-BarData$Time[1]

#
BarData_to_Use=BarData[
  data.table(
    Symbol=Symbols[1],
    Time=seq(min(BarData$Time),
             max(BarData$Time),
             by=BarData$Time[2]-BarData$Time[1])
  ),
  on=c("Symbol", "Time")]
BarData_to_Use[, Ind:=.I]

# compute quantities
for(ind in 1:nrow(Output)){
  
  assign(
    paste0("BarData_Temp_", ind),
    
    do.call(
      rbind,
      lapply(Trading_Dates,
             #x=Trading_Dates[2]
             function(x){
               x=as.Date(x)
               
               # consider the pre-market trading time
               Temp=BarData[Time>=paste0(x-1, " ", Market_Close_Time)&
                              Time<paste0(x, " ", Market_Close_Time), ]
               Temp=Temp[!(Time>=paste0(x, " ", Market_Open_Time)&
                             Time<paste0(x, " ", Market_Close_Time)), ]
               
               # compute informative quantities
               if(nrow(Temp)>0){
                 Temp[, RSI_:=RSI(Close)]
                 
                 if(Output[ind, RSI_Threshold]<50){
                   Temp[, RSI_Bi:=(RSI_<Output$RSI_Threshold[ind])]
                   Temp[, RSI_Signal:=c(rep(NA, length=Output$Consec_N[ind]-1),
                                        RcppRoll::roll_sum(RSI_Bi, Output$Consec_N[ind]))==Output$Consec_N[ind]]
                   
                   # Temp[, Lagged_Close:=lag(Close)]
                   # Temp[, Close_Diff:=Close-Open]
                   # Temp[, High_Diff:=High-Open]
                   # Temp[, Low_Diff:=Low-Open]
                   # 
                   # Temp[, Shifted_Open:=shift(Open, -1)]
                   # Temp[, Open_Diff:=Shifted_Open-Open]
                 }else if(Output[ind, RSI_Threshold]>=50){
                   Temp[, RSI_Bi:=(RSI_>=Output$RSI_Threshold[ind])]
                   Temp[, RSI_Signal:=c(rep(NA, length=Output$Consec_N[ind]-1),
                                        RcppRoll::roll_sum(RSI_Bi, Output$Consec_N[ind]))==Output$Consec_N[ind]]
                   
                   # Temp[, Lagged_Close:=lag(Close)]
                   # Temp[, Close_Diff:=Close-Open]
                   # Temp[, High_Diff:=High-Open]
                   # Temp[, Low_Diff:=Low-Open]
                   # 
                   # Temp[, Shifted_Open:=shift(Open, -1)]
                   # Temp[, Open_Diff:=Shifted_Open-Open]
                 }
                 
                 # first observation among consecutive observations that meet the criterion
                 Temp[RSI_Signal==TRUE &
                        frollapply(frollapply(RSI_Signal, 2, sum, align="right"),
                                   2, sum, align="right")==1, ]
               }else{
                 NULL
               }
             })
    )
    
  )
  
}

# Ind
for(ind in 1:nrow(Output)){
  Temp=get(
    paste0("BarData_Temp_", ind)
  )
  
  assign(
    paste0("BarData_Temp_", ind),
    BarData_to_Use[, .SD, .SDcols=c("Symbol",
                                    "Time",
                                    "Ind")][Temp, on=c("Symbol", "Time")]
  )
}


# compute informative quantities
for(ind in 1:nrow(Output)){
  Temp=get(
    paste0("BarData_Temp_", ind)
  )
  
  # if(Output[ind, RSI_Threshold]<50){
  #   # Percentage of Close_Diff<0
  #   Output[ind,
  #          Percentage:=mean(Temp[RSI_Signal==TRUE,
  #                                Close_Diff<0])]
  #   
  # }else if(Output[ind, RSI_Threshold]>=50){
  #   # Percentage of Close_Diff>=0
  #   Output[ind,
  #          Percentage:=mean(Temp[RSI_Signal==TRUE,
  #                                Close_Diff>=0])]
  # }
  # 
  # # Mean
  # Output[ind,
  #        Mean:=mean(Temp[RSI_Signal==TRUE,
  #                        Close_Diff])]
  
  # N
  Output[ind,
         N:=nrow(Temp)]
}


# # 3D plot
# library(plotly)
# Output[is.na(Percentage), Percentage:=0]
# Output[is.na(Mean), Mean:=0]
# plot_ly(Output,
#         x = ~RSI_Threshold,
#         y = ~Consec_N, z = ~Percentage,
#         color=~N)
# 
# Output[RSI_Threshold==30 &
#          Consec_N==1, ]

for(ind in 1:nrow(Output)){
  assign(
    paste0("BarData_Normalized_", ind),
    Value_Difference_Normalizer(get(paste0("BarData_Temp_", ind)),
                                Width<-30,
                                Time_Unit)
  )
  # summary(Temp[Order_Ind==10, Normalized_Close])
  plot(get(paste0("BarData_Normalized_", ind))[, mean(Normalized_Close), by="Order_Ind"])
}


lapply(
  1:nrow(Output),
  function(ind){
    Temp=get(paste0("BarData_Normalized_", ind))
    sapply(c(10),
           function(x){
             sum(Temp[order(Ind)][Order_Ind==x, Normalized_Close])
           })
  }
)


Commission=0.62


# plot(Temp[, median(Normalized_Close), by="Order_Ind"])
