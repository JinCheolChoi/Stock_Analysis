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
tws=twsConnect(port=7497)


#**********************
# Operational inquiries
#**********************
isConnected(tws)
# reqCurrentTime(tws)
# serverVersion(tws)
# twsDisconnect(tws) # disconnect from TWS

#***************
# import sources
#***************
function.dir.path="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/" # desktop
#function.dir.path="C:/Users/jchoi02/Desktop/R/Stock_Analysis/" # laptop
source(paste0(function.dir.path, "Future_Functions.R"))


#********************
# account information
#********************
# margin account = "U4524665"
# paper trading account = "DU2656942"
reqAccountUpdates(tws,
                  acctCode="DU2656942")
  

#**************
# contract info
#**************
contract=twsFuture("MNQ", "GLOBEX", "202106")


#
#reqRealTimeBars(tws, contract, barSize="5", useRTH=F)


# real time market data
temp_data=c()
while(TRUE){
  temp_data = rbind(
    temp_data,
    reqMktData(
      tws,
      contract,
      snapshot=T
    )
  )
  
  temp_data=unique(temp_data)
  
}

temp_data=as.data.table(temp_data)
temp_data[, lastTimeStampNum:=as.numeric(lastTimeStamp)]


#**************
# save and load
#**************
#save.image(paste0(function.dir.path, "Rdata/Futures_2021-04-05.Rdata"))
#load(paste0(rdata.dir, "CLMM_Analysis_2021-02-13_Additional.Rdata"))




chart_size=5
temp_data[, lastTimeStampNum%%chart_size]



strftime(temp_data$lastTimeStamp, format="%D")
strftime(temp_data$lastTimeStamp, format="%H:%M:%S")


as.numeric(temp_data$lastTimeStamp)%%5



reqMktData(tws, contract, eventWrapper=eWrapper.data(1),
           CALLBACK=snapShot, snapshot = 1)

reqMktData(tws, contract, snapshot = 1)




# historical data
Hist_Dat_Original=reqHistoricalData(tws, contract, barSize="15 mins", duration="1 D", useRTH="0") # not limited to regular trading hours
Hist_Dat=as.data.table(Hist_Dat_Original)
Hist_Dat[, Sign:=sign(MNQM1.Close-MNQM1.Open)]

Hist_Dat[, RSI:=RSI(MNQH1.Close, n=9)]
Hist_Dat[, Shifted_Sign:=sign(shift(MNQH1.Close, -5)-MNQH1.Close)]
Hist_Dat[, Volume_Change:=MNQH1.Volume/shift(MNQH1.Volume, 1)]

Hist_Dat=Hist_Dat[!is.na(Shifted_Sign)&
                    !is.na(RSI)&
                    !is.na(Volume_Change), ]
#
cor(Hist_Dat[, .SD, .SDcols=c("RSI", "Shifted_Sign")])
cor(Hist_Dat[RSI>=70 & Volume_Change>2.5, .SD, .SDcols=c("RSI", "Shifted_Sign")])


pacf(Hist_Dat$MNQH1.Close)
Hist_Dat[RSI>=85 & Volume_Change>2.5, ]

Hist_Dat$Shifted_Sign

# #
# Bar_Data=reqRealTimeBars(tws, contract, barSize="1")
# print(head(Bar_Data))
# print(head(reqRealTimeBars(tws, contract, barSize="1")))
# 
# # write to an open file connection
# fh=file('C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/out.dat',open='a')
# reqMktData(tws, contract, file=fh)
# close(fh)


########################




sma_len1=1
sma_len2=5
max_pos=2
currentPosition=0

if(!exists("toyData") ){
  toydata = reqMktData(tws,contract, eventWrapper=eWrapper.data(1), CALLBACK=snapShot)
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
    action = "BUY"
  if(short_sma <(long_sma-0.005))
    action = "SELL"
  quantity = 1
  if(action!=""){
    print(paste(action,quantity,currentPosition))
    if(abs(currentPosition+ (action=="BUY") *as.numeric(quantity)
           - (action=="SELL")*as.numeric(quantity))<max_pos)
    {
      orderId=as.numeric(reqIds(tws))
      toyorder=twsOrder(orderId,orderType = "MKT",action=action,
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
myorder=twsOrder(orderId, orderType="MKT", action="BUY", totalQuantity = "1", transmit = T)
placeOrder(tws, contract, myorder)

orderId=as.numeric(reqIds(tws))
myorder=twsOrder(orderId, orderType="MKT", action="SELL", totalQuantity = "1", transmit = T)
placeOrder(tws, contract, myorder)
















