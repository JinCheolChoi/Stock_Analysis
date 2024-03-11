Order_Filled_R_Test=function(Which_Signals,
                             Max_Orders,
                             Stop_Order,
                             Profit_Order){
  Which_Ind_=Which_Signals[["Which_Ind"]]
  Action_=Which_Signals[["Action"]]
  Detail_=Which_Signals[["Detail"]]
  Quantity_=Which_Signals[["Quantity"]]
  Open_Price_=Which_Signals[["Open"]]
  Time_=Which_Signals[["Submitted_Time"]]
  Simultaneous_=Which_Signals[["Simultaneous"]]
  Ind_=Which_Signals[["Ind"]]
  
  # Penalty
  # Tick_Size
  
  Which_Ind_Out=c()
  Action_Out=c()
  Detail_Out=c()
  Quantity_Out=c()
  Open_Price_Out=c() # non-used
  Price_Out=c() #
  Time_Out=c()
  Net_Quantity_Out=c() #
  
  Current_Which_Ind=c()
  Current_Action=c()
  Current_Detail=c()
  Current_Quantity=c()
  Current_Open_Price=c()
  Current_Time=c()
  
  Current_Position=c() #
  Current_Position_Net_Quantity=0 #
  Current_Avg_Value=0 #
  
  Last_Simultaneous_Ind=0
  Simultaneous_List=c()
  
  # begin orders with an open position
  ind=0
  while_i=0
  while(while_i==0){
    ind=ind+1
    if(is.na(Detail_[ind])){
      next
    }
    
    if(Detail_[ind]=="BTO" | Detail_[ind]=="STO"){
      while_i=1
    }
  }
  
  for(i in ind:length(Which_Ind_)){
    # i=517
    # i=2077
    # i=2078
    # i=2091
    # i=i+1
    if(abs(Current_Position_Net_Quantity)>=Max_Orders){
      if(!is.na(Detail_[i])){
        if(Detail_[i]=="BTO" | Detail_[i]=="STO"){
          next
        }
      }
    }
    
    # check Simultaneous
    if(!is.na(Ind_[i])){
      if(Last_Simultaneous_Ind==Ind_[i]){
        next
      }
      
      if(Simultaneous_[i]==TRUE){
        Simultaneous_List=Detail_[which(Ind_==Ind_[i])]
        
        if(Current_Position_Net_Quantity>0){
          # Always first try to clear the existing positions
          if("STC"%in%Simultaneous_List){
            if(Detail_[i]=="STC"){
              Last_Simultaneous_Ind=Ind_[i]
            }else{
              next
            }
          }
          else if(!"STC"%in%Simultaneous_List){
            if(Detail_[i]=="BTO"){
              Last_Simultaneous_Ind=Ind_[i]
            }else{
              next
            }
          }
        }
        else if(Current_Position_Net_Quantity<0){
          # Always first try to clear the existing positions
          if("BTC"%in%Simultaneous_List){
            if(Detail_[i]=="BTC"){
              Last_Simultaneous_Ind=Ind_[i]
            }else{
              next
            }
          }
          else if(!"BTC"%in%Simultaneous_List){
            if(Detail_[i]=="STO"){
              Last_Simultaneous_Ind=Ind_[i]
            }else{
              next
            }
          }
        }
        else if(Current_Position_Net_Quantity==0){
          if(Detail_[i]=="BTO" | Detail_[i]=="STO"){
            Last_Simultaneous_Ind=Ind_[i]
          }
        }
        Simultaneous_List=c()
      }
    }
    
    #**************************
    # length(Current_Detail)==0
    #**************************
    if(length(Current_Detail)==0){
      #*********************************************************
      # is.na(Detail_[i])| Detail_[i]=="BTC" | Detail_[i]=="STC"
      #
      # skip 
      if(is.na(Detail_[i])| Detail_[i]=="BTC" | Detail_[i]=="STC"){
        next
      }
      
      #**************************************
      # Detail_[i]=="BTO" | Detail_[i]=="STO"
      #
      # non-skip (BTO or STO)
      if(Detail_[i]=="BTO" | Detail_[i]=="STO"){
        # adjust Quantity_[i]
        Quantity_[i]=sign(Quantity_[i])*min(abs(Quantity_[i]), Max_Orders) # adjust quantity ( <= Max_Orders)
        
        # initialize Current_* vars
        Current_Which_Ind=Which_Ind_[i]
        Current_Action=Action_[i]
        Current_Detail=Detail_[i]
        Current_Quantity=Quantity_[i]
        Current_Open_Price=Open_Price_[i]
        Current_Time=Time_[i]
        Current_Position_Net_Quantity=Current_Quantity
        
        # Current_Position & Current_Avg_Value
        if(Current_Position_Net_Quantity>0){
          Current_Position="Long"
          Current_Avg_Value=sum((Current_Open_Price+Penalty*Tick_Size)*abs(Current_Quantity))/abs(Current_Position_Net_Quantity)
        }else if(Current_Position_Net_Quantity<0){
          Current_Position="Short"
          Current_Avg_Value=sum((Current_Open_Price-Penalty*Tick_Size)*abs(Current_Quantity))/abs(Current_Position_Net_Quantity)
        }else if(Current_Position_Net_Quantity==0){
          Current_Position="No"
          Current_Avg_Value=0
        }
        
        # update *_Out
        Which_Ind_Out=c(Which_Ind_Out, Current_Which_Ind)
        Action_Out=c(Action_Out, Current_Action)
        Detail_Out=c(Detail_Out, Current_Detail)
        Quantity_Out=c(Quantity_Out, Current_Quantity)
        Price_Out=c(Price_Out, Current_Avg_Value)
        Time_Out=c(Time_Out, Current_Time)
        Net_Quantity_Out=c(Net_Quantity_Out, Current_Position_Net_Quantity)
        
        # early take profit or cut loss
        switch(Current_Position,
               "Long"={
                 # take profit
                 if((Current_Avg_Value+Profit_Order)<=Which_Signals[i, High]){
                   # update *_Out
                   Which_Ind_Out=c(Which_Ind_Out, Which_Ind_[i])
                   Action_Out=c(Action_Out, "Sell")
                   Detail_Out=c(Detail_Out, "Early_Profit")
                   Quantity_Out=c(Quantity_Out, -Current_Position_Net_Quantity)
                   Time_Out=c(Time_Out, Time_[i])
                   Price_Out=c(Price_Out, (Current_Avg_Value+Profit_Order))
                   Net_Quantity_Out=c(Net_Quantity_Out, 0)
                 }
                 # take profit
                 else if((Current_Avg_Value-Stop_Order)>=Which_Signals[i, Low]){
                   # update *_Out
                   Which_Ind_Out=c(Which_Ind_Out, Which_Ind_[i])
                   Action_Out=c(Action_Out, "Sell")
                   Detail_Out=c(Detail_Out, "Loss_Cut")
                   Quantity_Out=c(Quantity_Out, -Current_Position_Net_Quantity)
                   Time_Out=c(Time_Out, Time_[i])
                   Price_Out=c(Price_Out, (Current_Avg_Value-Stop_Order))
                   Net_Quantity_Out=c(Net_Quantity_Out, 0)
                 }
                 # else
                 else{
                   next
                 }
                 
                 # reset current variables
                 Current_Which_Ind=c()
                 Current_Action=c()
                 Current_Detail=c()
                 Current_Quantity=c()
                 Current_Open_Price=c()
                 Current_Time=c()
                 Current_Position=c()
                 Current_Position_Net_Quantity=0
                 Current_Avg_Value=0
               },
               "Short"={
                 # take profit
                 if((Current_Avg_Value-Profit_Order)>=Which_Signals[i, Low]){
                   # update *_Out
                   Which_Ind_Out=c(Which_Ind_Out, Which_Ind_[i])
                   Action_Out=c(Action_Out, "Buy")
                   Detail_Out=c(Detail_Out, "Early_Profit")
                   Quantity_Out=c(Quantity_Out, -Current_Position_Net_Quantity)
                   Time_Out=c(Time_Out, Time_[i])
                   Price_Out=c(Price_Out, (Current_Avg_Value-Profit_Order))
                   Net_Quantity_Out=c(Net_Quantity_Out, 0)
                 }
                 # cut loss
                 else if((Current_Avg_Value+Stop_Order)<=Which_Signals[i, High]){
                   # update *_Out
                   Which_Ind_Out=c(Which_Ind_Out, Which_Ind_[i])
                   Action_Out=c(Action_Out, "Buy")
                   Detail_Out=c(Detail_Out, "Loss_Cut")
                   Quantity_Out=c(Quantity_Out, -Current_Position_Net_Quantity)
                   Time_Out=c(Time_Out, Time_[i])
                   Price_Out=c(Price_Out, (Current_Avg_Value+Stop_Order))
                   Net_Quantity_Out=c(Net_Quantity_Out, 0)
                 }
                 # else
                 else{
                   next
                 }
                 
                 # reset current variables
                 Current_Which_Ind=c()
                 Current_Action=c()
                 Current_Detail=c()
                 Current_Quantity=c()
                 Current_Open_Price=c()
                 Current_Time=c()
                 Current_Position=c()
                 Current_Position_Net_Quantity=0
                 Current_Avg_Value=0
               },
               "No"={
                 next
               })
        next
      }
    }
    
    #*************************
    # length(Current_Detail)>0
    #*************************
    if(length(Current_Detail)>0){
      #*******************
      #* is.na(Detail_[i])
      if(is.na(Detail_[i])){
        if(Current_Avg_Value==0){ # this line is to prevent Current_Avg_Value from being calculated repetitively
          if(Current_Position_Net_Quantity>0){
            Current_Position="Long"
            Current_Avg_Value=sum((Current_Open_Price+Penalty*Tick_Size)*abs(Current_Quantity))/abs(Current_Position_Net_Quantity)
          }else if(Current_Position_Net_Quantity<0){
            Current_Position="Short"
            Current_Avg_Value=sum((Current_Open_Price-Penalty*Tick_Size)*abs(Current_Quantity))/abs(Current_Position_Net_Quantity)
          }else if(Current_Position_Net_Quantity==0){
            Current_Position="No"
            Current_Avg_Value=0
          }
        }
        
        switch(Current_Position,
               "Long"={
                 # take profit
                 if((Current_Avg_Value+Profit_Order)<=Which_Signals[i, High]){
                   # update *_Out
                   Which_Ind_Out=c(Which_Ind_Out, Which_Ind_[i])
                   Action_Out=c(Action_Out, "Sell")
                   Detail_Out=c(Detail_Out, "Early_Profit")
                   Quantity_Out=c(Quantity_Out, -Current_Position_Net_Quantity)
                   Time_Out=c(Time_Out, Time_[i])
                   Price_Out=c(Price_Out, (Current_Avg_Value+Profit_Order))
                   Net_Quantity_Out=c(Net_Quantity_Out, 0)
                 }
                 # cut loss
                 else if((Current_Avg_Value-Stop_Order)>=Which_Signals[i, Low]){
                   # update *_Out
                   Which_Ind_Out=c(Which_Ind_Out, Which_Ind_[i])
                   Action_Out=c(Action_Out, "Sell")
                   Detail_Out=c(Detail_Out, "Loss_Cut")
                   Quantity_Out=c(Quantity_Out, -Current_Position_Net_Quantity)
                   Time_Out=c(Time_Out, Time_[i])
                   Price_Out=c(Price_Out, (Current_Avg_Value-Stop_Order))
                   Net_Quantity_Out=c(Net_Quantity_Out, 0)
                 }
                 # else
                 else{
                   next
                 }
                 
                 # reset current variables
                 Current_Which_Ind=c()
                 Current_Action=c()
                 Current_Detail=c()
                 Current_Quantity=c()
                 Current_Open_Price=c()
                 Current_Time=c()
                 Current_Position=c()
                 Current_Position_Net_Quantity=0
                 Current_Avg_Value=0
               },
               "Short"={
                 # take profit
                 if((Current_Avg_Value-Profit_Order)>=Which_Signals[i, Low]){
                   # update *_Out
                   Which_Ind_Out=c(Which_Ind_Out, Which_Ind_[i])
                   Action_Out=c(Action_Out, "Buy")
                   Detail_Out=c(Detail_Out, "Early_Profit")
                   Quantity_Out=c(Quantity_Out, -Current_Position_Net_Quantity)
                   Time_Out=c(Time_Out, Time_[i])
                   Price_Out=c(Price_Out, (Current_Avg_Value-Profit_Order))
                   Net_Quantity_Out=c(Net_Quantity_Out, 0)
                 }
                 # cut loss
                 else if((Current_Avg_Value+Stop_Order)<=Which_Signals[i, High]){
                   # update *_Out
                   Which_Ind_Out=c(Which_Ind_Out, Which_Ind_[i])
                   Action_Out=c(Action_Out, "Buy")
                   Detail_Out=c(Detail_Out, "Loss_Cut")
                   Quantity_Out=c(Quantity_Out, -Current_Position_Net_Quantity)
                   Time_Out=c(Time_Out, Time_[i])
                   Price_Out=c(Price_Out, (Current_Avg_Value+Stop_Order))
                   Net_Quantity_Out=c(Net_Quantity_Out, 0)
                 }
                 # else
                 else{
                   next
                 }
                 
                 # reset current variables
                 Current_Which_Ind=c()
                 Current_Action=c()
                 Current_Detail=c()
                 Current_Quantity=c()
                 Current_Open_Price=c()
                 Current_Time=c()
                 Current_Position=c()
                 Current_Position_Net_Quantity=0
                 Current_Avg_Value=0
               },
               "No"={
                 next
               })
        
        next
      }
      
      #**************************************
      # Detail_[i]=="BTC" | Detail_[i]=="STC"
      if(Detail_[i]=="BTC" | Detail_[i]=="STC"){
        if(Current_Position_Net_Quantity>0){
          Current_Position="Long"
          Current_Avg_Value=sum((Current_Open_Price+Penalty*Tick_Size)*abs(Current_Quantity))/abs(Current_Position_Net_Quantity)
        }else if(Current_Position_Net_Quantity<0){
          Current_Position="Short"
          Current_Avg_Value=sum((Current_Open_Price-Penalty*Tick_Size)*abs(Current_Quantity))/abs(Current_Position_Net_Quantity)
        }else if(Current_Position_Net_Quantity==0){
          Current_Position="No"
          Current_Avg_Value=0
        }
        
        if(Current_Position=="Long" & Detail_[i]=="STC"){
          # adjust Quantity_[i]
          Quantity_[i]=sign(Quantity_[i])*min(abs(Current_Position_Net_Quantity), abs(Quantity_[i]))
          Current_Quantity=c(Current_Quantity, Quantity_[i])
          Current_Position_Net_Quantity=sum(Current_Quantity)
          
          # update *_Out
          Which_Ind_Out=c(Which_Ind_Out, Which_Ind_[i])
          Action_Out=c(Action_Out, "Sell")
          Detail_Out=c(Detail_Out, "STC")
          Quantity_Out=c(Quantity_Out, Quantity_[i])
          Time_Out=c(Time_Out, Time_[i])
          Price_Out=c(Price_Out, Open_Price_[i]-Penalty*Tick_Size)
          Net_Quantity_Out=c(Net_Quantity_Out, Current_Position_Net_Quantity)
          
          if(Current_Position_Net_Quantity==0){
            # reset current variables
            Current_Which_Ind=c()
            Current_Action=c()
            Current_Detail=c()
            Current_Quantity=c()
            Current_Open_Price=c()
            Current_Time=c()
            Current_Position=c()
            Current_Position_Net_Quantity=0
            Current_Avg_Value=0
            
          }else if(Current_Position_Net_Quantity>0){
            # update current variables
            Current_Which_Ind=c(Current_Which_Ind, Which_Ind_[i])
            Current_Action=c(Current_Action, Action_[i])
            Current_Detail=c(Current_Detail, Detail_[i])
            Current_Quantity=Current_Quantity
            Current_Open_Price=c(Current_Open_Price, Open_Price_[i])
            Current_Time=c(Current_Time, Time_[i])
            Current_Position_Net_Quantity=Current_Position_Net_Quantity
            Current_Avg_Value=Current_Avg_Value
          }
          
        }else if(Current_Position=="Short" & Detail_[i]=="BTC"){
          # adjust Quantity_[i]
          Quantity_[i]=sign(Quantity_[i])*min(abs(Current_Position_Net_Quantity), abs(Quantity_[i]))
          Current_Quantity=c(Current_Quantity, Quantity_[i])
          Current_Position_Net_Quantity=sum(Current_Quantity)
          
          # update *_Out
          Which_Ind_Out=c(Which_Ind_Out, Which_Ind_[i])
          Action_Out=c(Action_Out, "Buy")
          Detail_Out=c(Detail_Out, "BTC")
          Quantity_Out=c(Quantity_Out, Quantity_[i])
          Time_Out=c(Time_Out, Time_[i])
          Price_Out=c(Price_Out, Open_Price_[i]+Penalty*Tick_Size)
          Net_Quantity_Out=c(Net_Quantity_Out, Current_Position_Net_Quantity)
          
          if(Current_Position_Net_Quantity==0){
            # reset current variables
            Current_Which_Ind=c()
            Current_Action=c()
            Current_Detail=c()
            Current_Quantity=c()
            Current_Open_Price=c()
            Current_Time=c()
            Current_Position=c()
            Current_Position_Net_Quantity=0
            Current_Avg_Value=0
          }else if(Current_Position_Net_Quantity<0){
            # update current variables
            Current_Which_Ind=c(Current_Which_Ind, Which_Ind_[i])
            Current_Action=c(Current_Action, Action_[i])
            Current_Detail=c(Current_Detail, Detail_[i])
            Current_Quantity=Current_Quantity
            Current_Open_Price=c(Current_Open_Price, Open_Price_[i])
            Current_Time=c(Current_Time, Time_[i])
            Current_Position_Net_Quantity=Current_Position_Net_Quantity
            Current_Avg_Value=Current_Avg_Value
          }
        }
        next
      }
      
      #**************************************
      # Detail_[i]=="BTO" | Detail_[i]=="STO"
      if(Detail_[i]=="BTO" | Detail_[i]=="STO"){
        if(abs(Current_Position_Net_Quantity)>=Max_Orders){
          next
        }else if(abs(Current_Position_Net_Quantity)<Max_Orders){
          if(Current_Position=="Long" & Detail_[i]=="BTO"){
            # adjust Quantity_[i]
            Quantity_[i]=min(Quantity_[i], Max_Orders-Current_Position_Net_Quantity)
            
            # update current variables
            Current_Which_Ind=c(Current_Which_Ind, Which_Ind_[i])
            Current_Action=c(Current_Action, Action_[i])
            Current_Detail=c(Current_Detail, Detail_[i])
            Current_Quantity=c(Current_Quantity, Quantity_[i])
            Current_Open_Price=c(Current_Open_Price, Open_Price_[i])
            Current_Time=c(Current_Time, Time_[i])
            Current_Position_Net_Quantity=sum(Current_Quantity)
            Current_Avg_Value=sum((Current_Open_Price+Penalty*Tick_Size)*abs(Current_Quantity))/abs(Current_Position_Net_Quantity)
            
            # update *_Out
            Which_Ind_Out=c(Which_Ind_Out, Which_Ind_[i])
            Action_Out=c(Action_Out, "Buy")
            Detail_Out=c(Detail_Out, "BTO")
            Quantity_Out=c(Quantity_Out, Quantity_[i])
            Time_Out=c(Time_Out, Time_[i])
            Price_Out=c(Price_Out, Open_Price_[i]+Penalty*Tick_Size)
            Net_Quantity_Out=c(Net_Quantity_Out, Current_Position_Net_Quantity)
          }
          else if(Current_Position=="Short" & Detail_[i]=="STO"){
            # adjust Quantity_[i]
            Quantity_[i]=max(Quantity_[i], -(Max_Orders+Current_Position_Net_Quantity))
            
            # update current variables
            Current_Which_Ind=c(Current_Which_Ind, Which_Ind_[i])
            Current_Action=c(Current_Action, Action_[i])
            Current_Detail=c(Current_Detail, Detail_[i])
            Current_Quantity=c(Current_Quantity, Quantity_[i])
            Current_Open_Price=c(Current_Open_Price, Open_Price_[i])
            Current_Time=c(Current_Time, Time_[i])
            Current_Position_Net_Quantity=sum(Current_Quantity)
            Current_Avg_Value=sum((Current_Open_Price-Penalty*Tick_Size)*abs(Current_Quantity))/abs(Current_Position_Net_Quantity)
            
            # update *_Out
            Which_Ind_Out=c(Which_Ind_Out, Which_Ind_[i])
            Action_Out=c(Action_Out, "Sell")
            Detail_Out=c(Detail_Out, "STO")
            Quantity_Out=c(Quantity_Out, Quantity_[i])
            Time_Out=c(Time_Out, Time_[i])
            Price_Out=c(Price_Out, Open_Price_[i]-Penalty*Tick_Size)
            Net_Quantity_Out=c(Net_Quantity_Out, Current_Position_Net_Quantity)
          }
        }
        next
      }
      
    }
    
  }
  
  # return the output
  return(
    data.table(
      Which_Ind=Which_Ind_Out,
      Action=Action_Out,
      Detail=Detail_Out,
      Quantity=Quantity_Out,
      Time=as.POSIXct(Time_Out),
      Price=Price_Out,
      Net_Quantity=Net_Quantity_Out
    )
  )
}


#**************************************
# copy and paste from this part onwards
#**************************************
# BarData_5Secs_Include_Last_Time
BarData_5Secs_Include_Last_Time=copy(BarData_5Secs)
BarData_5Secs_Include_Last_Time=BarData_5Secs_Include_Last_Time[Time>=min(BarData_Include_Last_Time$Time) &
                                                                  Time<=max(BarData_Include_Last_Time$Time), ]
# BarData_5Secs
BarData_5Secs=BarData_5Secs[Time>=min(BarData$Time) &
                              Time<=max(BarData$Time), ]

# merge Which_Signals and BarData_5Secs_Include_Last_Time
Which_Signals[, Submitted_Time:=as.POSIXct(format(as.POSIXct(Submitted_Time),
                                                  tz="America/Los_Angeles"))]
Which_Signals=Which_Signals[BarData_5Secs_Include_Last_Time, on=c(Submitted_Time="Time")]
Which_Signals[, Which_Ind:=.I]

# Stop_Order=30
# Profit_Order=30
# Max_Orders=5
# Which_Signals[Quantity>0, Quantity:=round(runif(nrow(Which_Signals[Quantity>0, ]), min=1, max=5))]
# Which_Signals[Quantity<0, Quantity:=-round(runif(nrow(Which_Signals[Quantity<0, ]), min=1, max=5))]
if(Stop_Order<Inf & Profit_Order<Inf){
  # Which_Signals_R_Test
  Which_Signals_R_Test=Order_Filled_R_Test(Which_Signals=Which_Signals,
                                           Max_Orders=Max_Orders,
                                           Stop_Order=Stop_Order,
                                           Profit_Order=Profit_Order)
  Which_Signals_R_Test[, Submitted_Time:=Time]
}else{
  # Max_Orders=1
  # Which_Signals[c(sample(1:nrow(Which_Signals), nrow(Which_Signals)*0.5)), Quantity:=Quantity*3]
  Which_Signals_Copy=copy(Which_Signals)
  Which_Signals_Copy=Which_Signals_Copy[!is.na(Ind), ]
  
  Cresult=Order_Filled_C(Which_Signals=Which_Signals_Copy,
                         Max_Orders=Max_Orders)
  
  # Which_Signals_C
  Which_Signals_Copy[, `:=`(Quantity=Cresult$Quantity,
                            Net_Quantity=Cresult$Net_Quantity,
                            Remove=Cresult$Remove)]
  Which_Signals_C=Which_Signals_Copy[Remove==0, ]
}

# compare Which_Signals_C and Which_Signals_R_Test
Which_Signals_C %>% dim
Which_Signals_R_Test %>% dim

Which_Signals_C[, .SD, .SDcols=c("Which_Ind", "Submitted_Time", "Action", "Detail", "Quantity", "Net_Quantity")] %>% head(10)
Which_Signals_R_Test[, .SD, .SDcols=c("Which_Ind", "Submitted_Time", "Action", "Detail", "Quantity", "Net_Quantity")] %>% head(10)

Which_Signals_C[, .SD, .SDcols=c("Which_Ind", "Submitted_Time", "Action", "Detail", "Quantity", "Net_Quantity")] %>% tail(10)
Which_Signals_R_Test[, .SD, .SDcols=c("Which_Ind", "Submitted_Time", "Action", "Detail", "Quantity", "Net_Quantity")] %>% tail(10)


# # convert time-zone
# Which_Signals_C[, Submitted_Time:=as.POSIXct(format(as.POSIXct(Submitted_Time),
#                                                     tz="America/Los_Angeles"))]
# Which_Signals_R_Test[, Submitted_Time:=as.POSIXct(format(as.POSIXct(Submitted_Time),
#                                                          tz="America/Los_Angeles"))]

identical(Orders_Transmitter(BarData_5Secs,
                             BarData_5Secs_Include_Last_Time,
                             Penalty,
                             Tick_Size,
                             Which_Signals,
                             Which_Signals_Order_Filled=Which_Signals_C,
                             Long_Signals_Sums,
                             Short_Signals_Sums),
          Orders_Transmitter(BarData_5Secs,
                             BarData_5Secs_Include_Last_Time,
                             Penalty,
                             Tick_Size,
                             Which_Signals,
                             Which_Signals_Order_Filled=Which_Signals_R_Test,
                             Long_Signals_Sums,
                             Short_Signals_Sums))



