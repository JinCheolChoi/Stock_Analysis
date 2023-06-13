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
# working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/" # desktop
working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/" # laptop

# account
# margin account="U4524665"
# paper trading account="DU2656942"
Account_Code="DU2656942"

# port
Port=7497 # tws : 7497, IB gateway : 4002

# BarSize
BarSize=5

#*****************
#
# preliminary step
#
#*******************
# required functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))
source(paste0(working.dir, "Echos/Echo_Live_Trading.R"))

#****************
# import packages
#****************
for(Package in
    c("IBrokers",
      "TTR",
      "data.table",
      "dplyr",
      "DescTools")){ # candle chart
  checkpackages(Package)
}

# import strategies
source(paste0(working.dir, "/Live_Trading_Strategy.R"))

#***********************
#
# live trading algorithm
#
#***********************
# contract info
contract=twsFuture("MNQ", "CME", "202309")
# connect to TWS
tws=twsConnect(clientId=round(runif(1)*10000000), port=Port)
# twsDisconnect(tws) # disconnect from TWS
# reqCurrentTime(tws)
# serverVersion(tws)

#************************
# assign local parameters
#************************
Strategy_Name="Live_Strategy"
source(paste0(working.dir, "/Common_Parameters.R"))

# Live_Trading
Live_Trading=TRUE

Transmitted_Orders=0

# Positions=0
Orders_Transmitted=c()

# N_Orders_held
while(!exists("N_Orders_held")){
  if(length(reqAccountUpdates(tws)[[2]])>0){
    N_Orders_held=reqAccountUpdates(tws)[[2]][[1]]$portfolioValue$position
  }else if(length(reqAccountUpdates(tws)[[2]])==0){
    N_Orders_held=0
  }
  Sys.sleep(0.5) # suspend execution for a while to prevent the system from breaking
}

#********
# BarData
BarData=Initiate_BarData(BarSize=BarSize,
                         Ignore_Prep=FALSE)

#***************
# main algorithm
#***************
# BarData5Secs=c()
print("the main algorithm initiated")
while(TRUE){
  #***************
  # connect to tws
  #***************
  while(!isConnected(tws)){
    tws=twsConnect(clientId=round(runif(1)*10000000), port=Port)
  }
  
  #**************************
  # request realtime bar data
  #**************************
  # output : BarData
  if(!ReqRealTimeBars(BarSize, Log=F)){ # skip to the next iteration if the new data is not derived (New_Data==0)
    # #****************************
    # # see how many positions held
    # if(!is.null(Orders_Transmitted)){
    #   # number of open orders
    #   Open_Orders=unique(do.call(rbind, reqopenorders_cb(tws))[, 3])
    #   
    #   # check if transmitted order is filled
    #   # Positions=reqAccountUpdates(tws)[[2]][[1]]$portfolioValue$position
    #   if(length(Open_Orders)==2>0){
    #     Orders_Transmitted[Filled==0, Filled:=1]
    #   }
    #   
    #   #*******************
    #   # cancel open orders
    #   if(length(Open_Orders)==3|
    #      length(Open_Orders)==1){
    #     
    #     # (1) if main order is not filled for longer than 1 hours, cancel orders
    #     if(Sys.time()-Orders_Transmitted[Filled==0, Submit_Time]>60*60){
    #       Transmitted_Orders=0 # reset Transmitted_Orders to 0
    #       
    #       Open_Orders=unique(do.call(rbind, reqopenorders_cb(tws))[, 3])
    #       if(!is.null(Open_Orders)){
    #         for(Open_Order in Open_Orders){ # 
    #           cancelOrder(tws, Open_Order)
    #         }
    #         rm(Open_Orders)
    #       }
    #     }
    #     
    #     # (2) if there is no position and there are some remaining open orders, cancel all of the remaining open orders
    #     if(abs(Transmitted_Orders)>0){
    #       Transmitted_Orders=0 # reset Transmitted_Orders to 0
    #       
    #       Open_Orders=unique(do.call(rbind, reqopenorders_cb(tws))[, 3])
    #       if(!is.null(Open_Orders)){
    #         for(Open_Order in Open_Orders){ # 
    #           cancelOrder(tws, Open_Order)
    #         }
    #         rm(Open_Orders)
    #       }
    #     }
    #     
    #   }
    # }
    
    # move to the next iteration
    next
  }
  # while(!isConnected(tws)){
  #   tws=twsConnect(clientId=round(runif(1)*10000000), port=Port)
  # }
  # print("---------------------------------")
  
  #*************
  # candle chart
  #*************
  # Candle_Chart(BarData)
  
  #********************
  # determine an action
  #********************
  Live_Data_Temp=tail(BarData, Max_Rows)
  
  # algorithm determine to take a position
  if(!is.null(Live_Data_Temp) &
     Transmitted_Orders<Inf){ # run the algorithm only when there is no transmitted order
    #*********************
    # calculate indicators
    #*********************
    Calculated_Indicators=Indicator_Calculator(BarData=Live_Data_Temp,
                                               Strategy_Indicators=Strategy_Indicators,
                                               Indicators=Indicators)
    
    # if there is an indicator that hasn't been computed, skip the iteration
    if(sum(unlist(lapply(Calculated_Indicators,
                         function(x){
                           is.null(x)
                         })))>0){
      next
    }
    
    #***********
    # fit models
    #***********
    Signals=Signal_Obtainer(Strategy_Models=Strategy_Models,
                            Models_Env=Models_Env,
                            Models=Models,
                            Strategy_Models_Class=Strategy_Models_Class,
                            Calculated_Indicators=Calculated_Indicators)
    # rownames(Signals)=c("Long", "Short")
    
    #***************
    # transmit order
    #***************
    if(nrow(Signals)>0){
      # - N of models <= Sigs_N <= N of models
      Sigs_N=apply(Signals, 1, sum)
      
      # message if both long and short signals are maximal
      if(sum(Sigs_N==ncol(Signals))==2){
        print("both long and short signals are maximal")
      }
      
      # number of orders held (+:more long, -:more short)
      if(abs(N_Orders_held)>Max_Orders){
        warning("abs(N_Orders_held)>Max_Orders")
        break
      }
      
      # Position_Names_Temp
      # This part allows to force the long position entrance when there is no position filled yet while Sigs_N indicates to enter both positions at the same time
      # Also, it makes sure to transmit only once when N_Orders_held=1 & Max_Orders=1 (or N_Orders_held=-1 & Max_Orders=1).
      # If N_Orders_held>0, Short_Function is ignored and Long_Function is considered to take care of N_Orders_held (and vice versa).
      if(N_Orders_held>0){
        Position_Names_Temp=sort(Position_Names, decreasing=T)
      }else{
        Position_Names_Temp=sort(Position_Names, decreasing=F)
      }
      
      # Order_to_Transmit
      Order_to_Transmit=lapply(Position_Names_Temp,
                               function(x){
                                 do.call(OrderRules_Env[[paste0(x, "_Function")]],
                                         c(list(Live_Data=Live_Data_Temp,
                                                Time_Unit=BarSize,
                                                Stop_Order=Stop_Order,
                                                Profit_Order=Profit_Order,
                                                Max_Orders=Max_Orders,
                                                Sigs_N=Sigs_N,
                                                N_Orders_held=N_Orders_held),
                                           Params=list(Order_Rules[[x]]),
                                           Live_Trading=TRUE)
                                 )
                               })
      
      # record open and closed orders
      if(exists("Order_to_Transmit")){
        if(!is.null(do.call(rbind, Order_to_Transmit))){
          # print(Calculated_Indicators)
          # add Order_to_Transmit to Orders_Transmitted
          Orders_Transmitted=rbind(Orders_Transmitted,
                                   do.call(rbind, Order_to_Transmit),
                                   fill=T)
          #print(paste0("Transmit order / i : ", i, " / action : ", tail(Orders_Transmitted[["Detail"]], 1)))
          
          # reconnect (required after the version, 0.10-2)
          tws=twsConnect(clientId=round(runif(1)*10000000), port=Port)
        }
        
        # remove Orders_Transmitted
        rm(Order_to_Transmit)
      }
      
      # remove Signals
      rm(Signals)
    }
  }
  
}

