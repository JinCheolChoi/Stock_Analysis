n=52*1
weight=1/3 #
# b=1+0.113852
# p=0.93
sim_n=2
for(sim in 1:sim_n){
  Capital=10000
  
  min_adjusted_x=5
  max_adjusted_x=10
  
  Track=data.table()
  for(i in 1:n){
    out=F
    while(out==F){
      Bid_Price=abs(rnorm(1, sd=3))*0.9
      Commissions=rep(2, 1)
      Implied_p=runif(1, min=0.85, max=0.95)
      True_p=Implied_p*0.9
      
      True_p[True_p>=1]=1
      True_p[True_p<=0]=0
      
      # Maximum_Contracts_Num=min(floor(Capital/7000), 30)
      Maximum_Contracts_Num=1
      if(Maximum_Contracts_Num==0){break}
      x=(Capital*Bid_Price*Implied_p-Capital*(1-Implied_p)*(Commissions/(100*Maximum_Contracts_Num))-Commissions*Bid_Price)/(100*Bid_Price*Maximum_Contracts_Num+Capital*(1-Implied_p))-Bid_Price
      Stop_Price=(Bid_Price+x)
      Net_Return=Bid_Price*100*Maximum_Contracts_Num
      b=1+Net_Return/(Stop_Price*100*Maximum_Contracts_Num+Commissions)
      Kelly=(Implied_p)-(1-Implied_p)/(b-1)
      
      #
      adjusted_Kelly=Kelly*weight
      Bet=Capital*adjusted_Kelly
      
      adjusted_x=(Bet-Commissions)/100/Maximum_Contracts_Num-Bid_Price
      
      if(adjusted_Kelly>0 & adjusted_x>min_adjusted_x){
        out=T
        # print(paste0(sim, "_", i))
      }
    }
    
    #
    result=NA
    if(adjusted_Kelly>0 & adjusted_x>min_adjusted_x){
      if(adjusted_x>max_adjusted_x){
        adjusted_x=max_adjusted_x
        Bet=(adjusted_x+Bid_Price)*Maximum_Contracts_Num*100+Commissions
      }
      Prob=c(1-True_p, True_p)
      result=sample(c(0, 1), 1, replace = T, prob=Prob)
      if(result==1){
        Capital=Capital+Net_Return
      }else if(result==0){
        Capital=Capital-Bet
        if(Capital<=0){Capital=0}
      }
    }
    
    # track
    Track=rbind(Track,
                data.table(
                  x,
                  b,
                  Net_Return,
                  Bet,
                  Kelly,
                  adjusted_x,
                  Maximum_Contracts_Num,
                  Implied_p,
                  
                  Capital,
                  result
                ))
  }
  
  assign(paste0("Track_", sim),
         Track)
  
}

merged_sim=do.call(rbind,
                   lapply(1:sim_n, function(x) get(paste0("Track_", x))$Capital))
apply(merged_sim, 2, mean)
plot(apply(merged_sim, 2, mean),
     type='o')

