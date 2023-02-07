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
  Maximum_Elapsed_Time=Inf, 
  Reverse=FALSE          # Opposite actions are made if Reverse=TRUE
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

OrderRules_Env$Long_Function=function(Live_Data,
                                      Time_Unit,
                                      Max_Orders,
                                      Stop_Order=100,
                                      Profit_Order=100,
                                      Sigs_N,
                                      N_Orders_held,
                                      Params,
                                      Live_Trading=FALSE){
  #Sigs_N[1] : buy signal
  #Sigs_N[2] : sell signal
  # first if condition ensures to short a current long position if there is any
  if(0<N_Orders_held & # if there's a long position
           N_Orders_held<=Max_Orders &
           Sigs_N[2]>=Params[["SellToClose"]][["Min_Sig_N"]]){
    
    Action="Sell"
    Detail="STC"
    TotalQuantity=as.numeric(Params[["SellToClose"]][["Quantity"]])
    OrderType=Params[["SellToClose"]][["OrderType"]]
  }else if(0<=N_Orders_held & 
           N_Orders_held<Max_Orders &
           Sigs_N[1]>=Params[["BuyToOpen"]][["Min_Sig_N"]]){
    
    Action="Buy"
    Detail="BTO"
    TotalQuantity=as.numeric(Params[["BuyToOpen"]][["Quantity"]])
    OrderType=Params[["BuyToOpen"]][["OrderType"]]
  }
  
  # transmit orders for live trading
  if(exists("Action")){
    # adjust TotalQuantity in accordance with Max_Orders 
    # if(Action=="Buy" & abs(N_Orders_held+TotalQuantity)>Max_Orders){
    #   TotalQuantity=ifelse(Max_Orders-N_Orders_held>0, Max_Orders-N_Orders_held, -Max_Orders-N_Orders_held)
    #   print(paste0("Action : ", Action, "/ TotalQuantity : ", TotalQuantity))
    # }else if(Action=="Sell" & abs(N_Orders_held-TotalQuantity)>Max_Orders){
    #   TotalQuantity=ifelse(N_Orders_held+Max_Orders>0, N_Orders_held+Max_Orders, N_Orders_held-Max_Orders)
    #   print(paste0("Action : ", Action, "/ TotalQuantity : ", TotalQuantity))
    # }
    if(Action=="Buy" & N_Orders_held+TotalQuantity>Max_Orders){
      TotalQuantity=ifelse(Max_Orders-N_Orders_held>0, Max_Orders-N_Orders_held, -Max_Orders-N_Orders_held)
      print(paste0("Action : ", Action, "/ TotalQuantity : ", TotalQuantity))
    }else if(Action=="Sell" & N_Orders_held-TotalQuantity<0){
      TotalQuantity=N_Orders_held
      print(paste0("Action : ", Action, "/ TotalQuantity : ", TotalQuantity))
    }
    
    if(Live_Trading==TRUE){
      # order
      while(!isConnected(tws)){
        tws=twsConnect(port=Port)
      }
      Main_Order_Info<<-twsOrder(as.numeric(reqIds(tws)),
                                 orderType=OrderType,
                                 lmtPrice=tail(Live_Data, 1)[, Close],
                                 action=Action,
                                 totalQuantity=TotalQuantity,
                                 transmit=T)
      placeOrder(tws,
                 contract,
                 Main_Order_Info)
      
      # profit order
      if(Profit_Order<=5000){
        if(Action=="Buy"){
          Stop_Action="Sell"
          Profit_Order_Info<<-twsOrder(as.numeric(reqIds(tws)),
                                       orderType="LMT",
                                       lmtPrice=tail(Live_Data, 1)[, Close]+Profit_Order,
                                       action=Stop_Action,
                                       totalQuantity=TotalQuantity,
                                       transmit=T)
          placeOrder(tws,
                     contract,
                     Profit_Order_Info)
          
        }else if(Action=="Sell"){
          Stop_Action="Buy"
          Profit_Order_Info<<-twsOrder(as.numeric(reqIds(tws)),
                                       orderType="LMT",
                                       lmtPrice=tail(Live_Data, 1)[, Close]-Profit_Order,
                                       action=Stop_Action,
                                       totalQuantity=TotalQuantity,
                                       transmit=T)
          placeOrder(tws,
                     contract,
                     Profit_Order_Info)
        }
      }
      
      # stop loss order
      if(Stop_Order<=5000){
        if(Action=="Buy"){
          Stop_Action="Sell"
          Stop_Order_Info<<-twsOrder(as.numeric(reqIds(tws)),
                                     orderType="STP",
                                     #lmtPrice=tail(Live_Data, 1)[, Close]-Stop_Order,
                                     auxPrice=tail(Live_Data, 1)[, Close]-Stop_Order,
                                     action=Stop_Action,
                                     totalQuantity=TotalQuantity,
                                     transmit=T)
          
          placeOrder(tws,
                     contract,
                     Stop_Order_Info)
          
        }else if(Action=="Sell"){
          Stop_Action="Buy"
          Stop_Order_Info<<-twsOrder(as.numeric(reqIds(tws)),
                                     orderType="STP",
                                     #lmtPrice=tail(Live_Data, 1)[, Close]+Stop_Order,
                                     auxPrice=tail(Live_Data, 1)[, Close]+Stop_Order,
                                     action=Stop_Action,
                                     totalQuantity=TotalQuantity,
                                     transmit=T)
          placeOrder(tws,
                     contract,
                     Stop_Order_Info)
        }
      }
    }
    
    # save Old_N_Orders_held
    # Old_N_Orders_held=N_Orders_held
    
    # update N_Orders_held
    # while(Old_N_Orders_held==N_Orders_held){
    #   N_Orders_held=reqAccountUpdates(tws)[[2]][[1]]$portfolioValue$position
    #   Sys.sleep(0.5) # suspend execution for a while to prevent the system from breaking
    # }
    N_Orders_held<<-N_Orders_held+(Action=="Buy")*TotalQuantity-(Action=="Sell")*TotalQuantity
    
    # print the number of positions
    # print(paste0("N of Positions : ", get("N_Orders_held", envir=.GlobalEnv)))
    
    #
    if(Action=="Buy"){
      return(data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                        Submit_Time=tail(Live_Data, 1)[, Time]+Time_Unit,
                        #Filled_Time=tail(Live_Data, 1)[, Time],
                        Action=Action,
                        Detail=Detail,
                        TotalQuantity=TotalQuantity,
                        OrderType=OrderType,
                        Price=tail(Live_Data, 1)[, Close],
                        Filled=0,
                        Sigs_N=Sigs_N[1]))
    }
    if(Action=="Sell"){
      return(data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                        Submit_Time=tail(Live_Data, 1)[, Time]+Time_Unit,
                        #Filled_Time=tail(Live_Data, 1)[, Time],
                        Action=Action,
                        Detail=Detail,
                        TotalQuantity=TotalQuantity,
                        OrderType=OrderType,
                        Price=tail(Live_Data, 1)[, Close],
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


OrderRules_Env$Short_Function=function(Live_Data,
                                       Time_Unit,
                                       Max_Orders,
                                       Stop_Order=100,
                                       Profit_Order=100,
                                       Sigs_N,
                                       N_Orders_held,
                                       Params,
                                       Live_Trading=FALSE){
  #Sigs_N[1] : buy signal
  #Sigs_N[2] : sell signal
  # first if condition ensures to long a current short position if there is any
  if(0>N_Orders_held & # if there's a short position
           N_Orders_held>=(-Max_Orders) & 
           Sigs_N[1]>=Params[["BuyToClose"]][["Min_Sig_N"]]){
    
    Action="Buy"
    Detail="BTC"
    TotalQuantity=as.numeric(Params[["BuyToClose"]][["Quantity"]])
    OrderType=Params[["BuyToClose"]][["OrderType"]]
  }else if(0>=N_Orders_held &
           N_Orders_held>(-Max_Orders) &
           Sigs_N[2]>=Params[["SellToOpen"]][["Min_Sig_N"]]){
    
    Action="Sell"
    Detail="STO"
    TotalQuantity=as.numeric(Params[["SellToOpen"]][["Quantity"]])
    OrderType=Params[["SellToOpen"]][["OrderType"]]
  }
  
  # transmit orders for live trading
  if(exists("Action")){
    # adjust TotalQuantity in accordance with Max_Orders
    # if(Action=="Buy" & abs(N_Orders_held+TotalQuantity)>Max_Orders){
    #   TotalQuantity=ifelse(Max_Orders-N_Orders_held>0, Max_Orders-N_Orders_held, -Max_Orders-N_Orders_held)
    #   print(paste0("Action : ", Action, "/ TotalQuantity : ", TotalQuantity))
    # }else if(Action=="Sell" & abs(N_Orders_held-TotalQuantity)>Max_Orders){
    #   TotalQuantity=ifelse(N_Orders_held+Max_Orders>0, N_Orders_held+Max_Orders, N_Orders_held-Max_Orders)
    #   print(paste0("Action : ", Action, "/ TotalQuantity : ", TotalQuantity))
    # }
    if(Action=="Sell" & N_Orders_held-TotalQuantity < -Max_Orders){
      TotalQuantity=ifelse(N_Orders_held+Max_Orders>0, N_Orders_held+Max_Orders, N_Orders_held-Max_Orders)
      print(paste0("Action : ", Action, "/ TotalQuantity : ", TotalQuantity))
    }else if(Action=="Buy" & N_Orders_held+TotalQuantity>0){
      TotalQuantity=-N_Orders_held
      print(paste0("Action : ", Action, "/ TotalQuantity : ", TotalQuantity))
    }
    
    if(Live_Trading==TRUE){
      # order
      while(!isConnected(tws)){
        tws=twsConnect(port=Port)
      }
      Main_Order_Info<<-twsOrder(as.numeric(reqIds(tws)),
                                 orderType=OrderType,
                                 lmtPrice=tail(Live_Data, 1)[, Close],
                                 action=Action,
                                 totalQuantity=TotalQuantity,
                                 transmit=T)
      
      placeOrder(tws,
                 contract,
                 Main_Order_Info)
      
      # profit order
      if(Profit_Order<=5000){
        if(Action=="Buy"){
          Stop_Action="Sell"
          Profit_Order_Info<<-twsOrder(as.numeric(reqIds(tws)),
                                       orderType="LMT",
                                       lmtPrice=tail(Live_Data, 1)[, Close]+Profit_Order,
                                       action=Stop_Action,
                                       totalQuantity=TotalQuantity,
                                       transmit=T)
          placeOrder(tws,
                     contract,
                     Profit_Order_Info)
          
        }else if(Action=="Sell"){
          Stop_Action="Buy"
          Profit_Order_Info<<-twsOrder(as.numeric(reqIds(tws)),
                                       orderType="LMT",
                                       lmtPrice=tail(Live_Data, 1)[, Close]-Profit_Order,
                                       action=Stop_Action,
                                       totalQuantity=TotalQuantity,
                                       transmit=T)
          
          placeOrder(tws,
                     contract,
                     Profit_Order_Info)
        }
      }
      
      # stop loss order
      if(Stop_Order<=5000){
        if(Action=="Buy"){
          Stop_Action="Sell"
          Stop_Order_Info<<-twsOrder(as.numeric(reqIds(tws)),
                                     orderType="STP",
                                     #lmtPrice=tail(Live_Data, 1)[, Close]-Stop_Order,
                                     auxPrice=tail(Live_Data, 1)[, Close]-Stop_Order,
                                     action=Stop_Action,
                                     totalQuantity=TotalQuantity,
                                     transmit=T)
          placeOrder(tws,
                     contract,
                     Stop_Order_Info)
          
        }else if(Action=="Sell"){
          Stop_Action="Buy"
          Stop_Order_Info<<-twsOrder(as.numeric(reqIds(tws)),
                                     orderType="STP",
                                     #lmtPrice=tail(Live_Data, 1)[, Close]+Stop_Order,
                                     auxPrice=tail(Live_Data, 1)[, Close]+Stop_Order,
                                     action=Stop_Action,
                                     totalQuantity=TotalQuantity,
                                     transmit=T)
          placeOrder(tws,
                     contract,
                     Stop_Order_Info)
        }
      }
    }
    
    # save Old_N_Orders_held
    # Old_N_Orders_held=N_Orders_held
    
    # update N_Orders_held
    # while(Old_N_Orders_held==N_Orders_held){
    #   N_Orders_held=reqAccountUpdates(tws)[[2]][[1]]$portfolioValue$position
    #   Sys.sleep(0.5) # suspend execution for a while to prevent the system from breaking
    # }
    N_Orders_held<<-N_Orders_held+(Action=="Buy")*TotalQuantity-(Action=="Sell")*TotalQuantity
    
    # print the number of positions
    # print(paste0("N of Positions : ", get("N_Orders_held", envir=.GlobalEnv)))
    
    #
    if(Action=="Sell"){
      return(data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                        Submit_Time=tail(Live_Data, 1)[, Time]+Time_Unit,
                        #Filled_Time=tail(Live_Data, 1)[, Time],
                        Action=Action,
                        Detail=Detail,
                        TotalQuantity=TotalQuantity,
                        OrderType=OrderType,
                        Price=tail(Live_Data, 1)[, Close],
                        Filled=0,
                        Sigs_N=Sigs_N[2]))
    }
    if(Action=="Buy"){
      return(data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                        Submit_Time=tail(Live_Data, 1)[, Time]+Time_Unit,
                        #Filled_Time=tail(Live_Data, 1)[, Time],
                        Action=Action,
                        Detail=Detail,
                        TotalQuantity=TotalQuantity,
                        OrderType=OrderType,
                        Price=tail(Live_Data, 1)[, Close],
                        Filled=0,
                        Sigs_N=Sigs_N[1]))
    }
  }
  
}

