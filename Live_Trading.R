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
BarSize=5


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

# import packages
for(Package in
    c("IBrokers",
      "TTR",
      "data.table",
      "dplyr",
      "DescTools")){ # candle chart
  checkpackages(Package)
}

# import strategies
source(paste0(working.dir, "/Strategy.R"))



#***********************
#
# live trading algorithm
#
#***********************
# contract info
contract=twsFuture("MNQ", "GLOBEX", "202112")
# connect to TWS
tws=twsConnect(port=Port)
# twsDisconnect(tws) # disconnect from TWS
# reqCurrentTime(tws)
# serverVersion(tws)

BarData=c()
# BarData5Secs=c()
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
    ############################################################
    # Positions=reqAccountUpdates(tws)[[2]][[1]]$portfolioValue$position
    # if(Old_Positions<Positions){
    #   N_Filled_Orders=Positions-Old_Positions
    #   Old_Positions=Positions
    #   Transmitted=Old_Positions*2
    #   
    #   print(Positions)
    #   print(Transmitted)
    # }else if(Old_Positions>Positions){
    #   if(Positions==0){
    #     
    #   }
    # }
    
    ################################################################################
    # work on this part to cancel remaining orders in case that profit or loss is reached or that main order is not filled for longer than 1 hours,
    # for which e_execDetails seems useful to modifiy to export relevant info
    ############################################################
    next
  }
  
  #*************
  # candle chart
  #*************
  # Candle_Chart(BarData)
  
  #********************
  # determine an action
  #********************
  Live_Data_Temp=tail(BarData, Max_Rows)
  Positions=reqAccountUpdates(tws)[[2]][[1]]$portfolioValue$position
  
  if(abs(Transmitted)<Max_Orders & 
     abs(Positions)<Max_Orders){
    #*********************
    # calculate indicators
    #*********************
    Calculated_Indicators=sapply(Strategy_Indicators,
                                 function(x)
                                   if(x=="Close"){
                                     Live_Data_Temp[["Close"]]
                                   }else{
                                     if(nrow(Live_Data_Temp)>Indicators[[x]][['n']]+1){ # BBands : n-1, RSI : n+1
                                       do.call(x, 
                                               c(list(Live_Data_Temp[["Close"]]), # for now only using "Close price", additional work would be required in the future if the indicator does not depend on "Close price"
                                                 Indicators[[x]]))
                                     }
                                   }
    )
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
    
    if(nrow(Signals)==2){
      # Signals are assigned opposite if Trend=TRUE & Trend is TRUE for either direction
      if(Trend==TRUE){
        if(sum(Signals$Trend)>0){
          Signals[, which(sapply(Signals, function(x) sum(x==T)==1)):=lapply(.SD, function(x) x==F), .SDcols=which(sapply(Signals, function(x) sum(x==T)==1))]
        }else{
          Signals[1, ]=FALSE
          Signals[2, ]=FALSE
        }
      }
    }
    
    #***************
    # transmit order
    #***************
    if(nrow(Signals)>0){
      # - N of models <= Sigs_N <= N of models
      Sigs_N=apply(Signals, 1, sum)
      
      # number of orders held (+:more long, -:more short)
      N_Orders_held=Positions
      
      # Order_to_Transmit
      Order_to_Transmit=lapply(Strategy_Rules,
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
      
      if(!is.null(Order_to_Transmit[[1]])){
        Transmitted=3
        #print(Order_to_Transmit[[1]])
      }else if(!is.null(Order_to_Transmit[[2]])){
        Transmitted=-3
        #print(Order_to_Transmit[[2]])
      }
      
      # remove Signals
      rm(Signals)
    }
  }
  
  
  # if(exists("Order_to_Transmit")){
  #   if(!is.null(do.call(rbind, Order_to_Transmit))){
  #     # add Order_to_Transmit to Orders_Transmitted
  #     Orders_Transmitted=rbind(Orders_Transmitted,
  #                              do.call(rbind, Order_to_Transmit),
  #                              fill=T)
  #     # remove Orders_Transmitted
  #     rm(Order_to_Transmit)
  #     #print(paste0("Transmit order / i : ", i, " / action : ", tail(Orders_Transmitted[["Detail"]], 1)))
  #   }
  # }
  
}


