# parameter
Max_Orders=3
Scenario
Reverse
# Stop_Order
# Profit_Order
# Maximum_Elapsed_Time

microbenchmark::microbenchmark(Order_Filled_C(Which_Signals, Max_Orders),
                               Order_Filled_R(Which_Signals, Max_Orders))


Order_Filled_Results_C=Order_Filled_C(Which_Signals, Max_Orders)
Order_Filled_Results_R=Order_Filled_R(Which_Signals, Max_Orders)


Which_Signals_C=copy(Which_Signals)
Which_Signals_R=copy(Which_Signals)

Which_Signals_C[, Quantity:=Order_Filled_Results_C$Quantity]
Which_Signals_C[, Net_Quantity:=Order_Filled_Results_C$Net_Quantity]
Which_Signals_C[, Remove:=Order_Filled_Results_C$Remove]

Which_Signals_R[, Quantity:=Order_Filled_Results_R$Quantity]
Which_Signals_R[, Net_Quantity:=Order_Filled_Results_R$Net_Quantity]
# Which_Signals_R[, Remove:=Order_Filled_Results_R$Remove]

Which_Signals_C
Order_Filled_Results_R


identical(Which_Signals_C[Remove==0, ],
          Which_Signals_R[Remove==0, ])
identical(Which_Signals_C$Ind,
          Which_Signals_R$Ind)
identical(Which_Signals_C$Signals,
          Which_Signals_R$Signals)
identical(Which_Signals_C$Action,
          Which_Signals_R$Action)
identical(Which_Signals_C$Detail,
          Which_Signals_R$Detail)

identical(Which_Signals_C$Quantity,
          Which_Signals_R$Quantity)
sum(Which_Signals_C$Quantity==Which_Signals_R$Quantity)

identical(Which_Signals_C$Both_Direction,
          Which_Signals_R$Both_Direction)

identical(Which_Signals_C$Net_Quantity,
          Which_Signals_R$Net_Quantity)
sum(Which_Signals_C$Net_Quantity==Which_Signals_R$Net_Quantity)

identical(Which_Signals_C$Remove,
          Which_Signals_R$Remove)
sum(Which_Signals_C$Remove==Which_Signals_R$Remove)






Order_Filled_R=function(Which_Signals, Max_Orders){
  Ind_=Which_Signals[["Ind"]]
  Action_=Which_Signals[["Action"]]
  Detail_=Which_Signals[["Detail"]]
  Quantity_=Which_Signals[["Quantity"]]
  Both_Direction_=Which_Signals[["Both_Direction"]]
  
  Net_Quantity_=rep(0, nrow(Which_Signals))
  Net_Quantity_[1]=Which_Signals[, Quantity][1]
  Remove_=rep(0, nrow(Which_Signals))
  Both_Direction_Ind=0
  
  for(i in 2:nrow(Which_Signals)){
    #i=2
    # Quantity_[i]=-4
    # Net_Quantity_[i-1]=0
    
    # adjust Quantity[i]
    if((Net_Quantity_[i-1]<0)&
       (Net_Quantity_[i-1]+Quantity_[i]>0)&
       (Net_Quantity_[i-1]+Quantity_[i]<=Max_Orders)){
      Quantity_[i]=-Net_Quantity_[i-1]
    }else if((Net_Quantity_[i-1])>0&
             (Net_Quantity_[i-1]+Quantity_[i]<0)&
             (Net_Quantity_[i-1]+Quantity_[i]>=-Max_Orders)){
      Quantity_[i]=-Net_Quantity_[i-1]
    }
    
    if(abs(Net_Quantity_[i-1]+Quantity_[i])>Max_Orders){
      if(Quantity_[i]<0){
        Quantity_[i]=max(Quantity_[i], -(Max_Orders+Net_Quantity_[i-1]))
      }else if(Quantity_[i]>=0){
        Quantity_[i]=min(Quantity_[i], Max_Orders-Net_Quantity_[i-1])
      }
    }
    
    
    # if abs(Net_Quantity_[i-1])>=Max_Orders
    if(abs(Net_Quantity_[i-1])>=Max_Orders){
      if(Detail_[i]=="BTO"|
         Detail_[i]=="STO"){
        Net_Quantity_[i]=Net_Quantity_[i-1]
        Remove_[i]=1
        
      }
      
      switch(as.character(Both_Direction_[i]),
             
             "TRUE"={
               if(Both_Direction_Ind==Ind_[i]){
                 Net_Quantity_[i]=Net_Quantity_[i-1]
                 Remove_[i]=1
                 
                 next
               }
               
               # Always first try to clear the existing positions
               if((Net_Quantity_[i-1]>0&Detail_[i]=="STC")|
                  (Net_Quantity_[i-1]<0&Detail_[i]=="BTC")){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i]
                 
                 next
               }
             },
             
             "FALSE"={
               if((Net_Quantity_[i-1]>0&Detail_[i]=="STC")|
                  (Net_Quantity_[i-1]<0&Detail_[i]=="BTC")){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 next
               }
             })
      
      Net_Quantity_[i]=Net_Quantity_[i-1]
      
      Remove_[i]=1
      
      next
    }
    
    
    # if(Net_Quantity_[i-1]>0)
    if(Net_Quantity_[i-1]>0){
      
      switch(as.character(Both_Direction_[i]),
             
             "TRUE"={
               if(Both_Direction_Ind==Ind_[i]){
                 Net_Quantity_[i]=Net_Quantity_[i-1]
                 Remove_[i]=1
                 
                 next
               }
               
               # Always first try to clear the existing positions
               if(Detail_[i]=="STC"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i]
                 
                 next
               }
               
               if(Detail_[i]=="BTO"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i]
                 
                 next
               }
             },
             
             "FALSE"={
               if(Detail_[i]=="BTO"|
                  Detail_[i]=="STC"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 
                 next
               }
             })
      
      Net_Quantity_[i]=Net_Quantity_[i-1]
      
      Remove_[i]=1
      
      next
    }
    
    
    # Net_Quantity_[i-1]<0
    if(Net_Quantity_[i-1]<0){
      
      switch(as.character(Both_Direction_[i]),
             
             "TRUE"={
               if(Both_Direction_Ind==Ind_[i]){
                 Net_Quantity_[i]=Net_Quantity_[i-1]
                 Remove_[i]=1
                 
                 next
               }
               
               # Always first try to clear the existing positions
               if(Detail_[i]=="BTC"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i]
                 
                 next
               }
               
               if(Detail_[i]=="STO"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i]
                 
                 next
               }
             },
             
             "FALSE"={
               if(Detail_[i]=="BTC"|
                  Detail_[i]=="STO"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 
                 next
               }
             })
      
      Net_Quantity_[i]=Net_Quantity_[i-1]
      
      Remove_[i]=1
      
      next
    }
    
    
    # Net_Quantity_[i-1]==0
    if(Net_Quantity_[i-1]==0){
      
      switch(as.character(Both_Direction_[i]),
             
             "TRUE"={
               if(Both_Direction_Ind==Ind_[i]){
                 Net_Quantity_[i]=Net_Quantity_[i-1]
                 Remove_[i]=1
                 
                 next
               }
               
               # This part allows to force the long position entrance when there is no position filled yet while Sigs_N indicates to enter both positions at the same time
               if(Detail_[i]=="BTO"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i]
                 
                 next
                 
               }else if(Detail_[i]=="STO"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i]
                 
                 next
               }
             },
             
             "FALSE"={
               if(Detail_[i]=="BTO"|
                  Detail_[i]=="STO"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 
                 next
               }
             })
      
      Net_Quantity_[i]=Net_Quantity_[i-1]
      
      Remove_[i]=1
      
      next
    }
  }
  
  
  return(
    list(
      Quantity=Quantity_,
      Net_Quantity=Net_Quantity_,
      Remove=Remove_
    )
  )
}