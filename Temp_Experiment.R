RSI_Threshold=seq(10, 90, by=5)
Consec_N=c(1:10)
Market_Open_Time="05:30:00"
Market_Close_Time="14:00:00"

Output=data.table(expand.grid(
  RSI_Threshold=RSI_Threshold,
  Consec_N=Consec_N,
  Percentage=NaN,
  Mean=NaN,
  N=NaN
))

BarData_Temp=copy(BarData)
Output[, Ind:=.I]

Trading_Dates=unique(as.Date(BarData_Temp$Time))
BarData_Temp=do.call(
  rbind,
  lapply(Trading_Dates,
         #x=Trading_Dates[3]
         function(x){
           x=as.Date(x)
           
           Temp=BarData_Temp[Time>=paste0(x-1, " ", Market_Close_Time)&
                               Time<paste0(x, " ", Market_Close_Time), ]
           Temp=Temp[!(Time>=paste0(x, " ", Market_Open_Time)&
                         Time<paste0(x, " ", Market_Close_Time)), ]
           
           if(nrow(Temp)>0){
             Temp[, RSI_:=RSI(Close)]
           }else{
             NULL
           }
         })
)

BarData_Temp[, Ind:=.I]
# compute informative quantities
for(ind in 1:nrow(Output)){
  
  if(Output[ind, RSI_Threshold]<50){
    BarData_Temp[, RSI_Bi:=(RSI_<Output$RSI_Threshold[ind])]
    BarData_Temp[, RSI_Signal:=c(rep(NA, length=Output$Consec_N[ind]-1),
                                 RcppRoll::roll_sum(RSI_Bi, Output$Consec_N[ind]))==Output$Consec_N[ind]]
    
    BarData_Temp[, Lagged_Close:=lag(Close)]
    BarData_Temp[, Close_Diff:=Close-Open]
    BarData_Temp[, High_Diff:=High-Open]
    BarData_Temp[, Low_Diff:=Low-Open]
    
    BarData_Temp[, Lagged_Open:=shift(Open, -1)]
    BarData_Temp[, Open_Diff:=Lagged_Open-Open]
    
    # first observation among consecutive observations that meet the criterion
    Temp=BarData_Temp[RSI_Signal==TRUE &
                        frollapply(frollapply(RSI_Signal, 2, sum, align="right"),
                                   2, sum, align="right")==1, ]
    
    # Percentage of RSI_Signal==TRUE
    Output[ind,
           Percentage:=mean(Temp[RSI_Signal==TRUE,
                                 Close_Diff]<0)]
    
    # Mean
    Output[ind,
           Mean:=mean(Temp[RSI_Signal==TRUE,
                           Close_Diff])]
    
    # N
    Output[ind,
           N:=nrow(Temp[RSI_Signal==TRUE, ])]
  }else if(Output[ind, RSI_Threshold]>=50){
    BarData_Temp[, RSI_Bi:=(RSI_>=Output$RSI_Threshold[ind])]
    BarData_Temp[, RSI_Signal:=c(rep(NA, length=Output$Consec_N[ind]-1),
                                 RcppRoll::roll_sum(RSI_Bi, Output$Consec_N[ind]))==Output$Consec_N[ind]]
    
    BarData_Temp[, Lagged_Close:=lag(Close)]
    BarData_Temp[, Close_Diff:=Close-Open]
    BarData_Temp[, High_Diff:=High-Open]
    BarData_Temp[, Low_Diff:=Low-Open]
    
    BarData_Temp[, Lagged_Open:=shift(Open, -1)]
    BarData_Temp[, Open_Diff:=Lagged_Open-Open]
    
    # first observation among consecutive observations that meet the criterion
    Temp=BarData_Temp[RSI_Signal==TRUE &
                        frollapply(frollapply(RSI_Signal, 2, sum, align="right"),
                                   2, sum, align="right")==1, ]
    
    
    # Percentage
    Output[ind,
           Percentage:=mean(Temp[RSI_Signal==TRUE,
                                 Close_Diff]>=0)]
    
    # Mean
    Output[ind,
           Mean:=mean(Temp[RSI_Signal==TRUE,
                           Close_Diff])]
    
    # N
    Output[ind,
           N:=nrow(Temp[RSI_Signal==TRUE, ])]
  }
}


library(plotly)
Output[is.na(Percentage), Percentage:=0]
Output[is.na(Mean), Mean:=0]
plot_ly(Output,
        x = ~RSI_Threshold,
        y = ~Consec_N, z = ~Percentage,
        color=~N)



Output[RSI_Threshold==30 &
         Consec_N==1, ]

#
RSI_Threshold=30
Consec_N=1

if(RSI_Threshold<50){
  BarData_Temp[, RSI_Bi:=(RSI_<RSI_Threshold)]
}else{
  BarData_Temp[, RSI_Bi:=(RSI_>=RSI_Threshold)]
}
BarData_Temp[, RSI_Signal:=c(rep(NA, length=Consec_N-1),
                             RcppRoll::roll_sum(RSI_Bi, Consec_N))==Consec_N]

BarData_Temp[, Lagged_Close:=lag(Close)]
BarData_Temp[, Close_Diff:=Close-Open]
BarData_Temp[, High_Diff:=High-Open]
BarData_Temp[, Low_Diff:=Low-Open]


# first observation among consecutive observations that meet the criteria
Temp=BarData_Temp[RSI_Signal==TRUE &
                    frollapply(frollapply(RSI_Signal, 2, sum, align="right"),
                               2, sum, align="right")==1, ]

nrow(Temp[RSI_Signal==TRUE, ])

#
mean(Temp[RSI_Signal==TRUE,
          Close_Diff])

hist(Temp[RSI_Signal==TRUE &
            Close_Diff> -100,
          Close_Diff],
     breaks=100)
hist(Temp[RSI_Signal==TRUE &
            Close_Diff> -100,
          High_Diff],
     breaks=50)
hist(Temp[RSI_Signal==TRUE &
            Close_Diff> -100,
          Low_Diff],
     breaks=50)



Temp_2=do.call(rbind,
               lapply(0:10,
                      function(x){
                        Temp_2=BarData_Temp[Ind%in%(Temp$Ind+x)]
                        
                        Temp_2[, Order_Ind:=x]
                      }))

Temp_2[, Normalized_Close:=unlist(lapply(0:10,
                                         function(x){
                                           mapply(
                                             function(a, b){
                                               b-a
                                             },
                                             BarData_Temp[Ind%in%(Temp$Ind), Close],
                                             BarData_Temp[Ind%in%(Temp$Ind+x), Close]
                                           )
                                         }))]


Temp_2[Order_Ind==0, ] # 12976.75
Temp_2[Order_Ind==1, ] # 12977.75
Temp_2[Order_Ind==2, ] # 12979.75

plot(Temp_2$Order_Ind, Temp_2$Normalized_Close)
boxplot(Normalized_Close~Order_Ind,
        Temp_2)
plot(Temp_2[, mean(Normalized_Close), by="Order_Ind"])
plot(Temp_2[, median(Normalized_Close), by="Order_Ind"])


rq(
  Normalized_Close~Order_Ind,
  data=Temp_2
)

lm(
  Normalized_Close~Order_Ind,
  data=Temp_2
)
