
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
  
  source(paste0("C:/Users/JinCheol Choi/Desktop/R/Functions/Functions.R"))
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
BarData_5Secs=fread(paste0(data.dir, "5secs/", Symbols, "/", Symbols, ".csv"))
BarData=fread(paste0(data.dir, BarSize, "/", Symbols, "/", Symbols, ".csv"))
# BarData[, Time:=as.POSIXct(format(as.POSIXct(Time), tz="America/Los_Angeles"),
#                            tz="America/Los_Angeles")]

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

# Market_Time=c(1, 2, 3)
# Simple_BBands_1_Long_PctB=seq(0.1, 0.3, by=0.05)
# Simple_BBands_1_Short_PctB=seq(0.7, 0.9, by=0.05)
# Simple_BBands_2_Long_PctB=seq(0.1, 0.3, by=0.05)
# Simple_BBands_2_Short_PctB=seq(0.7, 0.9, by=0.05)
# Open_Long_Consec_Times=c(4)
# Open_Short_Consec_Times=c(4)
# Multiplier=100
# Reverse=c(TRUE, FALSE)
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

Market_Time=c(1, 2, 3)
ADX_Value=c(0)
Reverse=c(TRUE, FALSE)
Params=data.table(
  expand.grid(
    Market_Time,
    ADX_Value,
    Reverse
  )
)
Tuning_Parameters=c(
  "Market_Time",
  "ADX_Value",
  "Reverse"
)
colnames(Params)=Tuning_Parameters

i=1

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

Strategy_Name=Strategies[1]

assign("Temp",
       get(paste0("Results_", Strategy_Name)))

# elapsed time
Temp$Elapsed_Time[i]=0

# common parameters
source(paste0(working.dir, "/Common_Parameters.R"))

k_ind=1
BarData_Temp=get(paste0("BarData_", k_ind))
Trading_Dates=unique(as.Date(BarData_Temp$Time))

# Market_Time=1
Market_Time=Market_Time
BarData=BarData_Temp
Trading_Dates=Trading_Dates
Strategy_Name=Strategy_Name
Working_Dir=working.dir

x=Trading_Dates[3]

x=as.Date(x)

BarData=BarData[Time>=paste0(x-1, " ", Market_Close_Time)&
                  Time<paste0(x, " ", Market_Close_Time), ]

rm(x)

BarData[, Ind:=.I]

BarData=BarData
Strategy_Name=Strategy_Name
Working_Dir=Working_Dir

# BarData_Include_Last_Time
BarData_Include_Last_Time=copy(BarData)

# remove the last time time point
# this allows the second last time point to be the last possible order to be transmitted
BarData=BarData[-nrow(BarData), ]

# BarData is not allowed to be empty
if(nrow(BarData)==0){
  return(NULL)
}

#****************
# import packages
#****************
lapply(c("IBrokers",
         "TTR",
         "data.table",
         "dplyr"),
       checkpackages)

#*****************
# local parameters
#*****************
source(paste0(Working_Dir, "/Common_Parameters.R"))

#*********************
# simulation algorithm
#*********************
# define Orders_Transmitted
Orders_Transmitted=data.table(Symbol=tail(BarData, 1)[, Symbol],
                              Submitted_Time=tail(BarData, 1)[, Time],
                              Filled_Time=tail(BarData, 1)[, Time],
                              Action="",
                              Detail="",
                              TotalQuantity=0,
                              OrderType="MKT",
                              Price=0,
                              Filled=0,
                              Sigs_N=0)
Orders_Transmitted=Orders_Transmitted[-1,]
Time_Unit=BarData$Time[2]-BarData$Time[1]

#*********************
# calculate indicators
#*********************
Calculated_Indicators=Indicator_Calculator(BarData=BarData,
                                           Strategy_Indicators=Strategy_Indicators,
                                           Indicators=Indicators)

#***********
# fit models
#***********
Signals=Signal_Obtainer(Strategy_Models=Strategy_Models,
                        Models_Env=Models_Env,
                        Models=Models,
                        Strategy_Models_Class=Strategy_Models_Class,
                        Calculated_Indicators=Calculated_Indicators)

Long_Signals=as.data.table(sapply(Strategy_Models,
                                  function(x){
                                    Signals[[x]][[1]]
                                  }))
# Long_Signals[nrow(Long_Signals), ]=FALSE # this part is to not transfer orders at the very last time
Short_Signals=as.data.table(sapply(Strategy_Models,
                                   function(x){
                                     Signals[[x]][[2]]
                                   }))
# Short_Signals[nrow(Short_Signals), ]=FALSE # this part is to not transfer orders at the very last time

Long_Signals=Long_Signals[, lapply(.SD, as.numeric)]
Short_Signals=Short_Signals[, lapply(.SD, as.numeric)]
Long_Signals=Long_Signals[, lapply(.SD, function(x){ifelse(is.na(x), 0, x)})]
Short_Signals=Short_Signals[, lapply(.SD, function(x){ifelse(is.na(x), 0, x)})]

Long_Signals_Sums=apply_row_sum_C(as.matrix(Long_Signals))
Short_Signals_Sums=apply_row_sum_C(as.matrix(Short_Signals))

#****************************
# transmit order & fill order
#****************************
Long_Which_Signals=c()
Long_Which_Signals_BTO=c()
Long_Which_Signals_STC=c()

Short_Which_Signals=c()
Short_Which_Signals_STO=c()
Short_Which_Signals_BTC=c()

BuyToOpen_Signals=c()
SellToOpen_Signals=c()
SellToClose_Signals=c()
BuyToClose_Signals=c()
if("Long"%in%Position_Names){
  BuyToOpen_Min_Sig_N=as.numeric(Order_Rules[["Long"]][["BuyToOpen"]][["Min_Sig_N"]])
  # BuyToOpen_Min_Sig_N=1
  SellToClose_Min_Sig_N=as.numeric(Order_Rules[["Long"]][["SellToClose"]][["Min_Sig_N"]])
  
  BuyToOpen_Signals=Long_Signals_Sums>=BuyToOpen_Min_Sig_N
  BuyToOpen_Signals[length(BuyToOpen_Signals)]=FALSE # this part is to not transfer open orders at the very last time
  SellToClose_Signals=Short_Signals_Sums>=SellToClose_Min_Sig_N
  
  if(sum(BuyToOpen_Signals)>0){
    Long_Which_Signals_BTO=data.table(
      Ind=which(BuyToOpen_Signals),
      Time=BarData[BuyToOpen_Signals, Time],
      Signals=Long_Signals_Sums[which(BuyToOpen_Signals)],
      Action="Buy",
      Detail="BTO",
      Quantity=as.numeric(Order_Rules[["Long"]][["BuyToOpen"]][["Quantity"]])
    )
  }
  
  if(sum(SellToClose_Signals)>0){
    Long_Which_Signals_STC=data.table(
      Ind=which(SellToClose_Signals)[which(SellToClose_Signals)>=min(which(BuyToOpen_Signals))],
      Time=BarData[which(SellToClose_Signals)[which(SellToClose_Signals)>=min(which(BuyToOpen_Signals))], Time],
      Signals=Short_Signals_Sums[which(SellToClose_Signals)[which(SellToClose_Signals)>=min(which(BuyToOpen_Signals))]],
      Action="Sell",
      Detail="STC",
      Quantity=-as.numeric(Order_Rules[["Long"]][["SellToClose"]][["Quantity"]])
    )
  }
  
  Long_Which_Signals=rbind(Long_Which_Signals_BTO,
                           Long_Which_Signals_STC)
  
}
if("Short"%in%Position_Names){
  SellToOpen_Min_Sig_N=as.numeric(Order_Rules[["Short"]][["SellToOpen"]][["Min_Sig_N"]])
  # SellToOpen_Min_Sig_N=1
  BuyToClose_Min_Sig_N=as.numeric(Order_Rules[["Short"]][["BuyToClose"]][["Min_Sig_N"]])
  
  SellToOpen_Signals=Short_Signals_Sums>=SellToOpen_Min_Sig_N
  SellToOpen_Signals[length(SellToOpen_Signals)]=FALSE # this part is to not transfer open orders at the very last time
  BuyToClose_Signals=Long_Signals_Sums>=BuyToClose_Min_Sig_N
  
  if(sum(SellToOpen_Signals)>0){
    Short_Which_Signals_STO=data.table(
      Ind=which(SellToOpen_Signals),
      Time=BarData[SellToOpen_Signals, Time],
      Signals=Short_Signals_Sums[which(SellToOpen_Signals)],
      Action="Sell",
      Detail="STO",
      Quantity=-as.numeric(Order_Rules[["Short"]][["SellToOpen"]][["Quantity"]])
    )
  }
  
  if(sum(SellToOpen_Signals)>0 & sum(BuyToClose_Signals)>0){
    Short_Which_Signals_BTC=data.table(
      Ind=which(BuyToClose_Signals)[which(BuyToClose_Signals)>=min(which(SellToOpen_Signals))],
      Time=BarData[which(BuyToClose_Signals)[which(BuyToClose_Signals)>=min(which(SellToOpen_Signals))], Time],
      Signals=Long_Signals_Sums[which(BuyToClose_Signals)[which(BuyToClose_Signals)>=min(which(SellToOpen_Signals))]],
      Action="Buy",
      Detail="BTC",
      Quantity=as.numeric(Order_Rules[["Short"]][["BuyToClose"]][["Quantity"]])
    )
  }
  
  Short_Which_Signals=rbind(Short_Which_Signals_STO,
                            Short_Which_Signals_BTC)
  
}

if(sum(BuyToOpen_Signals)==0 &
   sum(SellToOpen_Signals)==0){
  return(list(Orders_Transmitted=NA,
              Ind_Profit=NA,
              Net_Profit=NA))
}
# if(sum(SellToClose_Signals)==0 &
#    sum(BuyToClose_Signals)==0){
#   return(list(Orders_Transmitted=NA,
#               Ind_Profit=NA,
#               Net_Profit=NA))
# }

Which_Signals=rbind(
  Long_Which_Signals,
  Short_Which_Signals
)

if(is.null(Which_Signals)){
  return(list(Orders_Transmitted=NA,
              Ind_Profit=NA,
              Net_Profit=NA))
}

Which_Signals[, Simultaneous:=duplicated(Which_Signals[["Time"]], fromLast=T)|duplicated(Which_Signals[["Time"]], fromLast=F)]
Which_Signals=Which_Signals[order(Time, Detail)] # make sure BTO comes ahead of STO given Simultaneous==TRUE

#************
# Market_Time
# 1: both regular and pre-market trading time, 2: only regular trading time, 3: only pre-market trading time
# last trading time prior to market close
Which_Signals[, Submitted_Time:=Time+Time_Unit]
Which_Signals[, Submitted_Time:=format(Submitted_Time, tz="America/Los_Angeles")]
Which_Signals[, Trading_Time:=format(as.POSIXct(Submitted_Time), format="%H:%M:%S")]

Last_Trading_Time=format(as.POSIXct(paste0("1970-01-01 ", Market_Close_Time))-Time_Unit, format="%H:%M:%S")

if(length(Which_Signals[Detail=="BTO" |
                        Detail=="STO", Ind])==0){
  return(list(Orders_Transmitted=NA,
              Ind_Profit=NA,
              Net_Profit=NA))
}
Which_Signals=Which_Signals[Ind>=min(Which_Signals[Detail=="BTO" |
                                                     Detail=="STO", Ind]), ]

# the first row must be either BTO or STO
while(Which_Signals[1, Detail]=="BTC"|
      Which_Signals[1, Detail]=="STC"){
  Which_Signals=Which_Signals[-1, ]
}

# order by Time and Action such that BTO is prioritized over STO
Which_Signals=Which_Signals[order(Time, Action)]

# Submitted_Time
Which_Signals[, Submitted_Time:=as.POSIXct(Submitted_Time)]


