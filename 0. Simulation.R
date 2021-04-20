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
#***********
# working directory
working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/" # desktop
#working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/" # laptop

# BarSize
BarSize=60*1

#*****************
#
# preliminary step
#
#*******************
# required functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))

# import packages
lapply(
  c(
    "IBrokers",
    "TTR",
    "data.table",
    "dplyr",
    "DescTools" # candle chart
  ), 
  checkpackages)


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
                                        BarSize=BarSize)


# compute indicators
# Bollinger Bands
Collapsed_BarData=data.table(Collapsed_BarData,
                             Collapsed_BarData[, BBands(Close, n=30, sd=2.58)])
setnames(Collapsed_BarData,
         c("dn", "mavg", "up", "pctB"),
         c("LBand", "MBand", "HBand", "PctB"))

# RSI
Collapsed_BarData[, RSI:=RSI(Close, n=9)]



# parse Collapsed_BarData to determine an action to take
# Bollinger bands strategy simulation
Result=BBands_Sim(Consec_Times=1,
                  Long_PctB=0.0,
                  Short_PctB=0.7)

Result
Result$Tradings[, Time_Diff:=as.numeric(Short_Time-Long_Time)]
Result$Tradings[Time_Diff>100, ]
Result$Tradings[, Time_Diff] %>% summary
#



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
















