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
#working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/" # laptop


#****************
# data parameters
#****************
Symbols=c("MNQ", "SPY")


#*****************
#
# preliminary step
#
#*****************
# required functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R")) # desktop
#source(paste0(working.dir, "0. Stock_Analysis_Functions.R")) # laptop

# import libraries
for(pack in c("IBrokers",
              "TTR",
              "data.table",
              "dplyr",
              "DescTools")){ # candle chart
  lapply(pack, checkpackages)
}

# initiate a strategy called "BBands_Strategy"
Init.Strategy(Name="BBands_Strategy",
              Order_Params=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
                                OrderType="MKT", # "LMT"
                                Position_Direction="both", # direction of position ("both", "long", "short")
                                Parsed_Data_Max_Rows=50)) # the maximum number of rows in a temp dataset to parse

# add indicator
Add.Indicator(Strategy="BBands_Strategy",
              Indicator="BBands",
              IndicatorParams=list(BBands_N=20,
                                   BBands_SD=2))
Add.Indicator(Strategy="BBands_Strategy",
              Indicator="RSI",
              IndicatorParams=list(RSI_N=14))

# add model (to run in combination with other included models to decide to transmit an order)
Add.Model(Strategy="BBands_Strategy",
          Model="Simple_BBands",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=0,
                           Short_PctB=1))
Add.Model(Strategy="BBands_Strategy",
          Model="Simple_RSI",
          ModelParams=list())

# import data
Get_Data(Symbols=list("MNQ", "SPY"),
         BarSize=60*5)

# bar data
# SPY
BarData=MNQ


#*********************
#
# simulation algorithm
#
#*********************
system.time({
  Sim_Results=do.call(Run_Simulation, get(Strategies[1]))
})
Sim_Results


#***********************************************
#
# all strategies saved in the global environment
#
#***********************************************
Strategies=ls()[sapply(ls(), function(x) any(class(get(x))=='Strategy'))]

names(unlist(as.list(args(Add.Model)))) # see all arguments in a function

names(BBands_Strategy$Models)
#********************************************************
BBands_Strategy


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




