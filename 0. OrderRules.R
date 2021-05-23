#**************************************
#
# create an environment for orders ----
#
#**************************************
OrderRules_Env=new.env()





#*************
#
# General ----
#
#***************************
OrderRules_Env$General=list(
  Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
  #Position_Direction="both",
  Scenario="Positive", # Positive : early profit is prioritized over loss cut
                       # Negative : loss cut is prioritized over early profit
  Stop_Order=10,
  Profit_Order=10,
  Trend=FALSE          # Signals are assigned opposite if Trend=TRUE
)


OrderRules_Env$General_Function=function(...){
  
}


#**********
#
# Long ----
#
#************************
OrderRules_Env$Long=list(
  BuyToOpen=list(OrderType="MKT",
                 Quantity=1,
                 Min_Sig_N=1), # minimum number of positive signals from models to transmit
  SellToClose=list(
    OrderType="MKT",
    Quantity=1,
    Min_Sig_N=1)# minimum number of positive signals from models to transmit
  
  
)

OrderRules_Env$Long_Function=function(Live_Data, Max_Orders, Sigs_N, N_Orders_held, Params){
  #Sigs_N[1] : buy signal
  #Sigs_N[2] : sell signal
  if(0<=N_Orders_held & 
     N_Orders_held<Max_Orders &
     Sigs_N[1]>=Params[["BuyToOpen"]][["Min_Sig_N"]]){
    
    Action="Buy"
    Detail="BTO"
    TotalQuantity=Params[["BuyToOpen"]][["Quantity"]]
    OrderType=Params[["BuyToOpen"]][["OrderType"]]
  }else if(0<N_Orders_held & # if there's a long position
           N_Orders_held<=Max_Orders &
           Sigs_N[2]>=Params[["SellToClose"]][["Min_Sig_N"]]){
    
    Action="Sell"
    Detail="STC"
    TotalQuantity=Params[["SellToClose"]][["Quantity"]]
    OrderType=Params[["SellToClose"]][["OrderType"]]
  }
  
  #
  if(exists("Action")){
    if(Action=="Buy"){
      return(data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                        Submit_Time=tail(Live_Data, 1)[, Time],
                        #Filled_Time=tail(Live_Data, 1)[, Time],
                        Action=Action,
                        Detail=Detail,
                        TotalQuantity=as.numeric(TotalQuantity),
                        OrderType=OrderType,
                        LmtPrice=tail(Live_Data, 1)[, Close],
                        Filled=0,
                        Sigs_N=Sigs_N[1]))
    }
    if(Action=="Sell"){
      return(data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                        Submit_Time=tail(Live_Data, 1)[, Time],
                        #Filled_Time=tail(Live_Data, 1)[, Time],
                        Action=Action,
                        Detail=Detail,
                        TotalQuantity=as.numeric(TotalQuantity),
                        OrderType=OrderType,
                        LmtPrice=tail(Live_Data, 1)[, Close],
                        Filled=0,
                        Sigs_N=Sigs_N[2]))
    }
  }
}



#***********
#
# Short ----
#
#*************************
OrderRules_Env$Short=list(
  SellToOpen=list(
    OrderType="MKT",
    Quantity=1,
    Min_Sig_N=1), # minimum number of positive signals from models to transmit
  BuyToClose=list(OrderType="MKT",
                  Quantity=1,
                  Min_Sig_N=1) # minimum number of positive signals from models to transmit
  
)


OrderRules_Env$Short_Function=function(Live_Data, Max_Orders, Sigs_N, N_Orders_held, Params){
  #Sigs_N[1] : buy signal
  #Sigs_N[2] : sell signal
  if(0>=N_Orders_held &
     N_Orders_held>(-Max_Orders) &
     Sigs_N[2]>=Params[["SellToOpen"]][["Min_Sig_N"]]){
    
    Action="Sell"
    Detail="STO"
    TotalQuantity=Params[["SellToOpen"]][["Quantity"]]
    OrderType=Params[["SellToOpen"]][["OrderType"]]
  }else if(0>N_Orders_held & # if there's a short position
           N_Orders_held>=(-Max_Orders) & 
           Sigs_N[1]>=Params[["BuyToClose"]][["Min_Sig_N"]]){
    
    Action="Buy"
    Detail="BTC"
    TotalQuantity=Params[["BuyToClose"]][["Quantity"]]
    OrderType=Params[["BuyToClose"]][["OrderType"]]
  }
  
  #
  if(exists("Action")){
    if(Action=="Sell"){
      return(data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                        Submit_Time=tail(Live_Data, 1)[, Time],
                        #Filled_Time=tail(Live_Data, 1)[, Time],
                        Action=Action,
                        Detail=Detail,
                        TotalQuantity=as.numeric(TotalQuantity),
                        OrderType=OrderType,
                        LmtPrice=tail(Live_Data, 1)[, Close],
                        Filled=0,
                        Sigs_N=Sigs_N[2]))
    }
    if(Action=="Buy"){
      return(data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                        Submit_Time=tail(Live_Data, 1)[, Time],
                        #Filled_Time=tail(Live_Data, 1)[, Time],
                        Action=Action,
                        Detail=Detail,
                        TotalQuantity=as.numeric(TotalQuantity),
                        OrderType=OrderType,
                        LmtPrice=tail(Live_Data, 1)[, Close],
                        Filled=0,
                        Sigs_N=Sigs_N[1]))
    }
  }
  
}

