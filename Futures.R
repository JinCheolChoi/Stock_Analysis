#********************
#
# empty the workspace
#
#********************
rm(list=ls())
library(IBrokers)
library(TTR)
library(data.table)
tws=twsConnect(port=7497)


#**********************
# Operational inquiries
#**********************
isConnected(tws)
# reqCurrentTime(tws)
# serverVersion(tws)
# twsDisconnect(tws) # disconnect from TWS

#*********************
#
#*********************
function.dir.path="C:/Users/jchoi02/Desktop/R/Stock_Analysis/"
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
reqRealTimeBars(tws, contract, barSize="5", useRTH=F)

reqMktData(tws, security, eventWrapper= eWrapper.data(1),
           CALLBACK=snapShot)



# real time market data
temp_data=c()
while(TRUE){
  temp_data = rbind(
    temp_data,
    reqMktData(
      tws,
      contract,
      snapshot = T
    )
  )
  
  temp_data=unique(temp_data)
}


# request market data
reqMktData(
  tws,
  contract
)
reqMktDepth(tws, contract)

# real time bar data (5 seconds bar chart)
temp_data=c()
temp_data=reqRealTimeBars(tws, contract, barSize="5", useRTH=F)


# historical data
Hist_Dat_Original=reqHistoricalData(tws, contract, barSize="15 mins", duration="30 D", useRTH="0") # not limited to regular trading hours
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
  toydata = reqMktData(tws,security,eventWrapper=eWrapper.data(1),CALLBACK=snapShot)
}
while(TRUE){
  toydata=rbind(toydata, reqMktData(tws, security, eventWrapper= eWrapper.data(1),
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
      placeOrder(tws,security,toyorder)
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
















