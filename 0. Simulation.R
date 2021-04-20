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
BarSize=60*15


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



# parameters
Indicators=c("BBands", "RSI")
Consec_Times=1
Long_PctB=0
Short_PctB=1
Live_Data_Max_Rows=50
Max_Positions=1 # 
OrderType="MKT" # "LMT"
Order_Direction="both" # "both", "long", "short"
# Collapsed_BarData[, BBands(Close, n=30, sd=2.58)]
# Collapsed_BarData[, RSI:=RSI(Close, n=9)]


#
if(Order_Direction=="both"){
  Long_Max_Positions=Short_Max_Positions=Max_Positions-1
}else if(Order_Direction=="long"){
  Long_Max_Positions=Max_Positions-1
  Short_Max_Positions=-1
}else if(Order_Direction=="short"){
  Long_Max_Positions=-1
  Short_Max_Positions=Max_Positions-1
}



# nrow(Collapsed_BarData)
system.time({
  for(i in 1:nrow(Collapsed_BarData)){
    # i=90
    if(!exists("Live_Data")){
      Live_Data=Collapsed_BarData[i, ]
      Order_Transmit=data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                                Submit_Time=tail(Live_Data, 1)[, Time],
                                Filled_Time=tail(Live_Data, 1)[, Time],
                                Action="", 
                                TotalQuantity=0,
                                OrderType="MKT",
                                LmtPrice=0,
                                Fill=0)
    }else{
      Live_Data=rbind(Live_Data, Collapsed_BarData[i, ], fill=T)
    }
    
    # add indicators
    # bollinger bands
    if("BBands"%in%Indicators){
      Long_By_pctB=0
      Short_By_pctB=0
      
      if(nrow(Live_Data)>19){
        BBands_Data=Live_Data[, BBands(Close)]
        
        # determine position by pctB
        Long_By_pctB=sum(tail(BBands_Data, Consec_Times)[,"pctB"]<=Long_PctB, na.rm=T)==Consec_Times
        Short_By_pctB=sum(tail(BBands_Data, Consec_Times)[,"pctB"]>=Short_PctB, na.rm=T)==Consec_Times
      }
    }
    
    # rsi
    if("RSI"%in%Indicators){
      if(nrow(Live_Data)>15){
        Live_Data[, RSI:=RSI(Close)]
      }
    }
    
    # macd
    if("MACD"%in%Indicators){
      if(nrow(Live_Data)>34){
        MACD_Data=Live_Data[, MACD(Close)]
      }
    }
    
    
    #**************
    # fill position
    #**************
    # buy
    if(nrow(Order_Transmit[Action=="Buy"&Fill==0, ])>0){
      
      Unfilled_Buy_Position_Times=Order_Transmit[Action=="Buy"&Fill==0, Submit_Time]
      Unfilled_Buy_Position_Prices=Order_Transmit[Submit_Time%in%Unfilled_Buy_Position_Times, LmtPrice]
      Which_Buy_Position_to_Fill=which(tail(Live_Data, 1)[, Low]<Unfilled_Buy_Position_Prices)[1] # fill the earlier one among positions that have met the price criterion
      
      Order_Transmit[Submit_Time==Unfilled_Buy_Position_Times[Which_Buy_Position_to_Fill],
                     `:=`(Filled_Time=tail(Live_Data, 1)[, Time],
                          Fill=1)]
    }
    # sell
    if(nrow(Order_Transmit[Action=="Sell"&Fill==0, ])>0){
      
      Unfilled_Sell_Position_Times=Order_Transmit[Action=="Sell"&Fill==0, Submit_Time]
      Unfilled_Sell_Position_Prices=Order_Transmit[Submit_Time%in%Unfilled_Sell_Position_Times, LmtPrice]
      Which_Sell_Position_to_Fill=which(tail(Live_Data, 1)[, High]>Unfilled_Sell_Position_Prices)[1] # fill the earlier one among positions that have met the price criterion
      
      Order_Transmit[Submit_Time==Unfilled_Sell_Position_Times[Which_Sell_Position_to_Fill],
                     `:=`(Filled_Time=tail(Live_Data, 1)[, Time],
                          Fill=1)]
    }
    
    
    #******************
    # transmit position
    #******************
    # buy
    if(Long_By_pctB){
      # determine the position
      if(sum(Order_Transmit[Action=="Buy", TotalQuantity])-
         sum(Order_Transmit[Action=="Sell", TotalQuantity])<=
         (Long_Max_Positions)){ # the number of currently filled or transmitted long positions is limited to (Max_Positions + short positions)
        print(paste0("buy : ", i))
        Order_Transmit=rbind(Order_Transmit,
                             data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                                        Submit_Time=tail(Live_Data, 1)[, Time],
                                        Filled_Time=tail(Live_Data, 1)[, Time],
                                        Action="Buy",
                                        TotalQuantity=1,
                                        OrderType=OrderType,
                                        LmtPrice=tail(Live_Data, 1)[, Close],
                                        Fill=0))
      }
    }
    
    # sell
    if(Short_By_pctB){
      if(sum(Order_Transmit[Action=="Sell", TotalQuantity])-
         sum(Order_Transmit[Action=="Buy", TotalQuantity])<=
         (Short_Max_Positions)){ # the number of currently filled or transmitted short positions is limited to (Max_Positions + long positions)
        print(paste0("sell : ", i))
        Order_Transmit=rbind(Order_Transmit,
                             data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                                        Submit_Time=tail(Live_Data, 1)[, Time],
                                        Filled_Time=tail(Live_Data, 1)[, Time],
                                        Action="Sell",
                                        TotalQuantity=1,
                                        OrderType=OrderType,
                                        LmtPrice=tail(Live_Data, 1)[, Close],
                                        Fill=0))
      }
    }
    
  }
  Order_Transmit=Order_Transmit[-1, ]
  rm(Live_Data)
})



# calculate the balance
Collapse_Order_Transmit=cbind(Order_Transmit[Action=="Buy", c("Filled_Time", "LmtPrice")],
                              Order_Transmit[Action=="Sell", c("Filled_Time", "LmtPrice")])
colnames(Collapse_Order_Transmit)=c("Buy_Time", "Buy_Price", "Sell_Time", "Sell_Price")

Collapse_Order_Transmit=Collapse_Order_Transmit[-unique(c(which(duplicated(Collapse_Order_Transmit[, c("Buy_Time", "Buy_Price")])), 
                                                          which(duplicated(Collapse_Order_Transmit[, c("Sell_Time", "Sell_Price")])))), ]

2*sum(Collapse_Order_Transmit[, Sell_Price-Buy_Price])-0.52*nrow(Collapse_Order_Transmit)







#








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
Result=BBands_Sim(Consec_Times=Consec_Times,
                  Long_PctB=Long_PctB,
                  Short_PctB=Short_PctB)

Result




#
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




