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
         BarSize=60*5,
         Convert_Tz=T,
         Filter=T)

# bar data
# SPY
BarData=MNQ

#************
# grid search
#************
# Simple_BBands_1_Long_PctB=c(0.25)
# Simple_BBands_2_Short_PctB=c(0.7)
# Stop_Order=c(1000000, 10, seq(20, 200, by=20))
# Profit_Order=seq(10, 200, by=5)
Simple_BBands_1_Long_PctB=seq(0, 1, by=0.05)
Simple_BBands_2_Short_PctB=seq(0, 1, by=0.05)
Stop_Order=c(10)
Profit_Order=c(100)

# Simple_BBands_1_Long_PctB=0.25
# Simple_BBands_2_Short_PctB=0.7
# Stop_Order=c(20)
# Profit_Order=c(175)
Params=data.table(
  expand.grid(Simple_BBands_1_Long_PctB,
              Simple_BBands_2_Short_PctB,
              Stop_Order,
              Profit_Order),
  NA
)

colnames(Params)=c("Simple_BBands_1_Long_PctB",
                   "Simple_BBands_2_Short_PctB",
                   "Stop_Order",
                   "Profit_Order",
                   "Net_Profit")

Net_Profit=c()
for(i in 1:nrow(Params)){
  if(Params[i, Simple_BBands_1_Long_PctB]==0 &
     Params[i, Simple_BBands_2_Short_PctB]==1){
    Params$Net_Profit[i]=0
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
  
  # run Backtesting
  T1=system.time({
    Sim_Results=Live_Trading_Imitator(BarData=MNQ,
                                      Strategy=get(Strategies[which(Strategies=="Test_Strategy")]))
  })
  
  # save results
  assign(paste0("Setting_", i),
         list(T1,
              Sim_Results))
  
  # save net profits
  Params$Net_Profit[i]=get(paste0("Setting_", i))[[2]]$Net_Profit
  
  # print the progress
  print(paste0(i, " / ", nrow(Params), " (", round(i/nrow(Params)*100, 2), "%)"))
}

#Sim_Results$Ind_Profit[, .SD, .SDcols=c("Time", "Cum_Profit")] %>% plot(type='o')
Params[, Row:=1:nrow(Params)]

Temp=Params

Temp=Params[Stop_Order<=10000 &
              Stop_Order>Profit_Order, ]

Temp=Params[Stop_Order<=10000 &
              Stop_Order<Profit_Order, ]

Temp=Params[Stop_Order<=10000, ]

i=Temp[Net_Profit==max(Net_Profit), Row]
Params[Row>=(i-10) &
         Row<=(i+10), ]
Params[i, ]

get(paste0("Setting_", i))[[2]]$Net_Profit
get(paste0("Setting_", i))[[2]]$Ind_Profit[, .SD, .SDcols=c("Date", "Daily_Cum_Profit")] %>% plot(type='o')
unique(get(paste0("Setting_", i))[[2]]$Ind_Profit[, .SD, .SDcols=c("Date", "Daily_Profit")])
unique(get(paste0("Setting_", i))[[2]]$Ind_Profit[, .SD, .SDcols=c("Date", "Daily_Profit")]) %>% plot(type="o")



Params[Stop_Order==10 & Profit_Order==100, ]
Params$Net_Profit[i]=get(paste0("Setting_", i))[[2]]$Net_Profit


4970.64

#**************
# save and load
#**************
#save.image("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/Futures_2021-07-11.Rdata")
#load("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/Futures_2021-07-11.Rdata")




RSIs=RSI(BarData$Close, n=9)
RSIs[which(MNQ$Time==Sim_Results$Orders_Transmitted[,Submit_Time][1])]
RSIs[which(MNQ$Time==Sim_Results$Orders_Transmitted[,Submit_Time][2])]


RSIs[which(MNQ$Time==Sim_Results$Orders_Transmitted[,Submit_Time][1])]
RSIs[which(MNQ$Time==Sim_Results$Orders_Transmitted[,Submit_Time][2])]

RSIs[which(MNQ$Time==Sim_Results$Orders_Transmitted[,Submit_Time][5])]
RSIs[which(MNQ$Time==Sim_Results$Orders_Transmitted[,Submit_Time][6])]

MNQ[, RSI:=RSI(Close, n=9)]
MNQ[, Shift_Close:=shift(Close, 1)]
MNQ[, Shift_RSI:=shift(RSI, 1)]
MNQ[, Diff_Close:=Close-Shift_Close]
MNQ[, Trend:=sign(Diff_Close)]
MNQ[, Shift_Trend:=shift(Trend, 1)]
MNQ[, Diff_RSI:=RSI-Shift_RSI]
MNQ[, BBands:=BBands(Close)[, 4]]
MNQ[, Shift_BBands:=shift(BBands, 1)]

MNQ[, .SD, .SDcols=c("Shift_RSI", "Diff_Close")] %>% plot
MNQ[, .SD, .SDcols=c("Shift_RSI", "Diff_Close")] %>% cor(use="complete.obs")
MNQ[, .SD, .SDcols=c("Shift_BBands", "Diff_Close")] %>% plot
MNQ[, .SD, .SDcols=c("Shift_BBands", "Diff_Close")] %>% cor(use="complete.obs")


MNQ[, .SD, .SDcols=c("Diff_Close", "Shift_RSI", "Shift_BBands", "Shift_Trend")]

which(MNQ$Time==Sim_Results$Orders_Transmitted[,Submit_Time][1])

summary(RSIs)
sum((RSIs<=20), na.rm=T)/length(RSIs[!is.na(RSIs)])*100
sum((RSIs>=(80)), na.rm=T)/length(RSIs[!is.na(RSIs)])*100


# run Backtesting
T2=system.time({
  Sim_Results=Backtesting(BarData<-MNQ,
                          Strategy<-get(Strategies[1]))
})




#***********************************************
#
# 
#
#***********************************************
names(unlist(as.list(args(Add_Model)))) # see all arguments in a function
names(Strategy_Simple_BBands$Models)

#********************************************************
Strategy_Simple_BBands


# compute indicators
# Bollinger Bands
Collapsed_BarData=data.table(Collapsed_BarData,
                             Collapsed_BarData[, BBands(Close)])
setnames(Collapsed_BarData,
         c("dn", "mavg", "up", "pctB"),
         c("LBand", "MBand", "HBand", "PctB"))

# RSI
Collapsed_BarData[, RSI:=RSI(Close)]



# parse Collapsed_BarData to determine an action to take
# Bollinger bands strategy simulation
Result=BBands_Sim(Consec_Times=2,
                  Long_PctB=0,
                  Short_PctB=1)

Result




#
Result$Tradings[, Time_Diff:=as.numeric(Short_Time-Long_Time)]
Result$Tradings[Time_Diff>100, ]
Result$Tradings[, Time_Diff] %>% summary
#



Collapsed_BarData[, Sign_in_Price_Change:=sign(Close-Open)]
Collapsed_BarData[, Volume_Change:=Volume/shift(Volume, 1)]
Collapsed_BarData[, Future_Direction:=sign(shift(Close, -1)-Close)]


Collapsed_BarData=Collapsed_BarData[!is.na(Future_Direction)&
                                      !is.na(RSI)&
                                      !is.na(Volume_Change), ]
#
cor(Collapsed_BarData[, .SD, .SDcols=c("RSI", "Future_Direction")])
cor(Collapsed_BarData[RSI>=80, .SD, .SDcols=c("RSI", "Future_Direction")])
cor(Collapsed_BarData[RSI>=80 & Volume_Change>2.5, .SD, .SDcols=c("RSI", "Future_Direction")])

Collapsed_BarData[RSI>=80 & Volume_Change>2.5, ]
# establish criteria to make a deicision




Collapsed_BarData

#
# remove a line displaying an error message in eWrapper_cust

#


# missing times
setdiff(seq(from=min(as.POSIXct(BarData$Time)),
            to=max(as.POSIXct(BarData$Time)),
            by=5),
        as.POSIXct(BarData$Time)) %>% as.POSIXct(origin="1970-01-01")







sma_len1=1
sma_len2=5
max_pos=2
currentPosition=0

if(!exists("toyData") ){
  toydata=reqMktData(tws,contract, eventWrapper=eWrapper.data(1), CALLBACK=snapShot)
}
while(TRUE){
  toydata=rbind(toydata, reqMktData(tws, contract, eventWrapper= eWrapper.data(1),
                                    CALLBACK=snapShot))
  
  toydata=unique(toydata)
  if(nrow(toydata)<sma_len2){
    print(paste0("not enough data yet: ", nrow(toydata)," lines"))
    Sys.sleep(1)
    next
  }
  short_sma= mean(tail(toydata$Last,sma_len1))
  long_sma= mean(tail(toydata$Last,sma_len2))
  action =""
  if(short_sma >(long_sma+0.005))
    action="BUY"
  if(short_sma <(long_sma-0.005))
    action="SELL"
  quantity=1
  if(action!=""){
    print(paste(action,quantity,currentPosition))
    if(abs(currentPosition+ (action=="BUY") *as.numeric(quantity)
           - (action=="SELL")*as.numeric(quantity))<max_pos)
    {
      orderId=as.numeric(reqIds(tws))
      toyorder=twsOrder(orderId,orderType="MKT",action=action,
                        totalQuantity=quantity,transmit=T)
      placeOrder(tws,contract,toyorder)
      currentPosition=currentPosition+ (action=="BUY") *as.numeric(quantity)
      - (action=="SELL")*as.numeric(quantity)
    }
  }
  else
  {print(paste0("not action yet: ",nrow(toydata)," lines ; ",
                "smas: short long ", short_sma, " ", long_sma," ", currentPosition))}
}



ac=reqAccountUpdates(tws)
twsPortfolioValue(ac)

orderId=as.numeric(reqIds(tws))
myorder=twsOrder(orderId, orderType="MKT", action="BUY", totalQuantity="1", transmit=T)
placeOrder(tws, contract, myorder)

orderId=as.numeric(reqIds(tws))
myorder=twsOrder(orderId, orderType="MKT", action="SELL", totalQuantity="1", transmit=T)
placeOrder(tws, contract, myorder)




