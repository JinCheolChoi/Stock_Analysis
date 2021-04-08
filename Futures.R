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



#********************
#
# account information
#
#********************
# margin account = "U4524665"
# paper trading account = "DU2656942"
reqAccountUpdates(tws,
                  acctCode="DU2656942")



#**************
#
# contract info
#
#**************
contract=twsFuture("MNQ", "GLOBEX", "202106")



#******************************************
#
# request and save 5 seconds bar chart data
#
#******************************************
# fh=file(paste0(working.dir, "out.dat"), open='a')
# reqRealTimeBars(tws, contract, barSize="5", useRTH=F, file=fh)
# close(fh)
BarData=c()
while(TRUE){
  #************************************
  # request realtime 5 seconds bar data
  reqRealTimeBars(tws, contract, barSize="5", useRTH=F,
                  eventWrapper=eWrapper_cust(),
                  CALLBACK=twsCALLBACK_cust)
  
  # if it fails to create RealTimeBarData, suspend execution for a while to avoid the system going break
  if(!exists("RealTimeBarData")){
    Sys.sleep(0.5)
  }else if(exists("RealTimeBarData")){
    BarData=unique(rbind(BarData, RealTimeBarData))
    
    # remove RealTimeBarData everytime it's combined
    rm(RealTimeBarData)
  }
  
  # a break during the temporary market close time
  System_Break()
  
}




# missing times
setdiff(seq(from=min(as.POSIXct(BarData$Time)),
            to=max(as.POSIXct(BarData$Time)),
            by=5),
        as.POSIXct(BarData$Time)) %>% as.POSIXct(origin="1970-01-01")



#






#********************************
# import 5 seconds bar chart data
#********************************
library(data.table)
while(T){
  if(file.exists(paste0(working.dir, "out.dat"))){
    if(round(as.numeric(Sys.time())%%5)==0){
      Bar_Data=fread(paste0(working.dir, "out.dat"))
      print(Bar_Data[nrow(Bar_Data),])
      
      Sys.sleep(5)
    }
  }else{break}
}

# filter bar data
Bar_Data=fread(paste0(working.dir, "out.dat"))
Colnames=c("Symbol", "Date", "Time", "Open", "High", "Low", "Close", "Volume", "Wap", "Count")
colnames(Bar_Data)=Colnames
Bar_Data_Filtered=Bar_Data[, lapply(.SD, function(x) unlist(strsplit(x, "="))[seq(from=2, to=2*nrow(Bar_Data), by=2)]), 
                           .SDcols=Colnames[Colnames!="Time"]]
Bar_Data_Filtered[, Time:=Bar_Data$Time]
setcolorder(Bar_Data_Filtered, Colnames) # re-order columns




#**************
# save and load
#**************
#save.image(paste0(working.dir, "Rdata/Futures_2021-04-05.Rdata"))
#load(paste0(working.dir, "Rdata/Futures_2021-04-05.Rdata"))







if(file.exists(paste0(working.dir, "out.dat"))){
  file.remove(paste0(working.dir, "out.dat"))
}

min(as.ITime(Test$`15:58:15`))

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


reqMktData(tws, contract) 
reqMktData(tws, contract)

unique(Temp_MktData)
reqMktData(tws, contract)



#*************
# candle chart
#*************
library(DescTools)







Temp_BarData=as.matrix(BarData[, 3:6])
rownames(Temp_BarData)=as.character(as.Date(BarData$Timestamp)+(0:(nrow(Temp_BarData)-1)))
PlotCandlestick(x=as.Date(rownames(Temp_BarData)), y=Temp_BarData, border=NA, las=1, ylab="")

Target_HistData=HistData[index>=min(BarData$Timestamp) & index<=max(BarData$Timestamp), 1:5]
Temp_HistData=as.matrix(Target_HistData[, 2:5])
colnames(Temp_HistData)=c("Open", "High", "Low", "Close")
rownames(Temp_HistData)=as.character(as.Date(Target_HistData$index)+(0:(nrow(Target_HistData)-1)))
PlotCandlestick(x=as.Date(rownames(Temp_HistData)), y=Temp_HistData, border=NA, las=1, ylab="")







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
















