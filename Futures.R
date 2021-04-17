#********************
#
# empty the workspace
#
#********************
rm(list=ls())
library(IBrokers)
library(TTR)
library(data.table)
library(dplyr)
library(DescTools) # candle chart
tws=twsConnect(port=7497)



#**********************
#
# Operational inquiries
#
#**********************
isConnected(tws)
# reqCurrentTime(tws)
# serverVersion(tws)
# twsDisconnect(tws) # disconnect from TWS



#***************
#
# import sources
#
#***************
working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/" # desktop
#working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/" # laptop
source(paste0(working.dir, "Future_Functions.R"))
#source(paste0("C:/Users/jchoi02/Desktop/R/Functions/Functions.R"))



#********************
#
# account information
#
#********************
# margin account="U4524665"
# paper trading account="DU2656942"
reqAccountUpdates(tws,
                  acctCode="DU2656942")


#**************
#
# contract info
#
#**************
contract=twsFuture("MNQ", "GLOBEX", "202106")



#***********************
#
# live trading algorithm
#
#***********************
BarData=c()
# BarData5Secs=c()
while(TRUE){
  # if connection is lost, reconnect
  while(!isConnected(tws)){tws=twsConnect(port=7497)}
  
  # a break during periods of market close time
  System_Break()
  
  # execute a daily save of 5 second bar data afterwards
  Daily_Hist_Data_Save(Force=T)
  
  # request realtime bar data
  # output : BarData
  ReqRealTimeBars(BarSize=5)
  
  # candle chart
  Candle_Chart(BarData)
  
  # determine an action
  
  
  # place an order
  
}


T_1=system.time({
  z=0
  for(i in 1:100000){
    # a break during periods of market close time
    System_Break()

    # execute a daily save of 5 second bar data afterwards
    Daily_Hist_Data_Save(Force=F)

    z=z+1
  }
})

T_2=system.time({
  r=0
  for(i in 1:100000){
    r=r+1
  }
})


#*********************
#
# simulation algorithm
#
#*********************
# import data
# output : `5SecsBarHistData`
Import_HistData(Location=paste0(working.dir, "Data/"),
                Symbol="MNQ",
                First_date="2021-03-15",
                Last_date=as.Date(format(Sys.time(), tz="PST8PDT")))

# collapse data to the chosen-sized bar data
Collapsed_BarData=Collapse_5SecsBarData(`5SecsBarHistData`,
                                        BarSize=60*5)



# parse Collapsed_BarData to determine an action to take
Collapsed_BarData
Collapsed_BarData[, RSI:=RSI(Close, n=9)]



Collapsed_BarData[, Sign:=sign(Close-Open)]
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




BB=BBands(Collapsed_BarData[, c("High", "Low", "Close")])

Collapsed_BarData

#
# remove a line displaying an error message in eWrapper_cust

#


# missing times
setdiff(seq(from=min(as.POSIXct(BarData$Time)),
            to=max(as.POSIXct(BarData$Time)),
            by=5),
        as.POSIXct(BarData$Time)) %>% as.POSIXct(origin="1970-01-01")









#**************
# save and load
#**************
#save.image(paste0(working.dir, "Rdata/Futures_2021-04-12.Rdata"))
#load(paste0(working.dir, "Rdata/Futures_2021-04-12.Rdata"))








# # real time market data
# barSize=5
# Temp_MktData=c()
# BarData=c()
# while(TRUE){
#   # raw market data
#   Raw_MktData=reqMktData(tws, contract, snapshot=T) 
#   
#   #*************************************
#   # record bar data + reset Temp_MktData
#   # if
#   # (1) the current Raw_MktData yields the remainder of 0; and
#   # (2) maximum time stamp in the current Temp_MktData does yield the remainder of 0
#   #*************************************#*******************************************
#   if(as.numeric(Raw_MktData$lastTimeStamp)%%barSize==0 &
#      max(as.numeric(Temp_MktData$lastTimeStamp))%%barSize!=0){
#     # record bar data
#     BarData=rbind(BarData,
#                   data.table(
#                     Symbol=unique(Temp_MktData$symbol),
#                     Timestamp=Temp_MktData$lastTimeStamp[nrow(Temp_MktData)]+1,
#                     Open=Temp_MktData$lastPrice[1],
#                     High=max(Temp_MktData$lastPrice),
#                     Low=min(Temp_MktData$lastPrice),
#                     Close=Temp_MktData$lastPrice[nrow(Temp_MktData)]
#                   )
#     )
#     
#     # reset Temp_MktData 
#     Temp_MktData=c()
#   }
#   
#   # Temp_MktData
#   Temp_MktData=unique(
#     rbind(
#       Temp_MktData,
#       Raw_MktData
#       )
#     )
# }


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
















