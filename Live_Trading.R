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

# account
# margin account="U4524665"
# paper trading account="DU2656942"
Account_Code="DU2656942"

# port
Port=7497 # tws : 7497, IB gateway : 4002

# BarSize
BarSize=60*1

#*****************
#
# preliminary step
#
#*******************
# required functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))
source(paste0(working.dir, "Remotes.R"))
source(paste0(working.dir, "Additional_Functions.R"))
source(paste0(working.dir, "Echos/Echo_Live_Trading.R"))
source(paste0(working.dir, "0. Models.R"))

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
contract=twsFuture("MNQ", "GLOBEX", "202212")
# connect to TWS
tws=twsConnect(port=Port)
# twsDisconnect(tws) # disconnect from TWS
# reqCurrentTime(tws)
# serverVersion(tws)


#************************
# assign local parameters
#************************
Max_Rows=Live_Strategy[["Max_Rows"]]
Order_Rules=Live_Strategy[["Order_Rules"]]
Indicators=Live_Strategy[["Indicators"]]
Models=Live_Strategy[["Models"]]

Max_Orders=as.numeric(Order_Rules[["General"]][["Max_Orders"]])
Scenario=Order_Rules[["General"]][["Scenario"]]
Reverse=Order_Rules[["General"]][["Reverse"]]
Stop_Order=as.numeric(Order_Rules[["General"]][["Stop_Order"]])
Profit_Order=as.numeric(Order_Rules[["General"]][["Profit_Order"]])
Strategy_Indicators=names(Indicators)
Strategy_Models=names(Models)
General_Strategy="General"
Position_Names=names(Order_Rules)[names(Order_Rules)!="General"]

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
    tws=twsConnect(port=Port)
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
  #   tws=twsConnect(port=Port)
  # }
  print("---------------------------------")
  
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
    Calculated_Indicators=lapply(Strategy_Indicators, # lapply is used here unlike simulation functions where sapply is used
                                 function(x)
                                   if(x=="Close"){
                                     Live_Data_Temp[["Close"]]
                                   }else{
                                     if(x=="BBands" & nrow(Live_Data_Temp)>=Indicators[[x]][['n']]+1){ # BBands : n-1, RSI : n+1
                                       do.call(x, 
                                               c(list(Live_Data_Temp[["Close"]]), # for now only using "Close price", additional work would be required in the future if the indicator does not depend on "Close price"
                                                 Indicators[[x]]))
                                     }else if(x!="BBands" & nrow(Live_Data_Temp)>Indicators[[x]][['n']]+1){
                                       do.call(x, 
                                               c(list(Live_Data_Temp[["Close"]]), # for now only using "Close price", additional work would be required in the future if the indicator does not depend on "Close price"
                                                 Indicators[[x]]))
                                     }
                                   }
    )
    names(Calculated_Indicators)=Strategy_Indicators
    # Calculated_Indicators=Calculated_Indicators[-which(sapply(Calculated_Indicators, is.null))]
    
    #***********
    # fit models
    #***********
    Signals=as.data.table(sapply(Strategy_Models,
                                 function(x){
                                   Model_Info=Models_Env[[x]] # variables and functions defined for the model object
                                   Calculated_Indicators_Combined=do.call(cbind, Calculated_Indicators) # combined Calculated_Indicators
                                   Calculated_Indicators_Names=names(Calculated_Indicators)[unlist(lapply(Calculated_Indicators, function(x) !is.null(x)))] #
                                   if(sum(!Model_Info[["Essential_Indicators"]]%in%Calculated_Indicators_Names)==0){ # if none of essential indicators hasn't been calculated in Calculated_Indicators, proceed to run the model
                                     do.call(Model_Info[["Function"]],
                                             c(list(Calculated_Indicators_Combined),
                                               Models[[x]]))}
                                 }))
    
    # if(nrow(Signals)==2){
    #   # Signals are assigned opposite if Reverse=TRUE & Reverse is TRUE for either direction
    #   if(Reverse==TRUE){
    #     if(sum(Signals$Trend)>0){
    #       Signals[, which(sapply(Signals, function(x) sum(x==T)==1)):=lapply(.SD, function(x) x==F), .SDcols=which(sapply(Signals, function(x) sum(x==T)==1))]
    #     }else{
    #       Signals[1, ]=FALSE
    #       Signals[2, ]=FALSE
    #     }
    #   }
    # }
    
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
        break
      }
      
      # Position_Names_Temp
      if(N_Orders_held<0){
        Position_Names_Temp=Position_Names[order(Position_Names, decreasing=T)]
      }else{
        Position_Names_Temp=Position_Names
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
          print(Calculated_Indicators)
          # add Order_to_Transmit to Orders_Transmitted
          Orders_Transmitted=rbind(Orders_Transmitted,
                                   do.call(rbind, Order_to_Transmit),
                                   fill=T)
          #print(paste0("Transmit order / i : ", i, " / action : ", tail(Orders_Transmitted[["Detail"]], 1)))
        }
        
        # remove Orders_Transmitted
        rm(Order_to_Transmit)
      }

      # remove Signals
      rm(Signals)
    }
  }
  
}


