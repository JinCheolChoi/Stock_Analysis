#***************
# short strategy
#***************
# determine stop-price (stop-long)
#Put_Ask=59.15 # put option ask (long position)
Bid_Price=0.29 # call option ask (short position)
Strike=270 # strike
SP=200.53 # stock price when purchased
#SE=seq(150, 300, by=0.01) # stock price at expiration

Margin_per_Contract=Bid_Price*100+max(0.2*SP*100+abs(Strike-SP)*100, 0.1*SP*100)
Margin_per_Contract=7669

# calculate the stop price
# x must be larger than 0
Capital=100000
Maximum_Contracts_Num=floor(Capital/Margin_per_Contract)
# Maximum_Contracts_Num=10
Net_Return=Bid_Price*100*Maximum_Contracts_Num
p=0.93
Commissions=1.04*2*Maximum_Contracts_Num
x=(Capital*Bid_Price*p-Capital*(1-p)*(Commissions/(100*Maximum_Contracts_Num)))/(100*Bid_Price*Maximum_Contracts_Num+Capital*(1-p))-Bid_Price
Stop_Price=(Bid_Price+x)
b=1+Net_Return/(Stop_Price*100*Maximum_Contracts_Num+Commissions)
Kelly=(p)-(1-p)/(b-1)
Kelly


if(Stop_Price>Bid_Price){ # x > 0 equivalently translantes to Stop_Price > Return/100
  print(paste0("Bid_Price : ", round(Bid_Price, 2), " / Stop_Price : ", round(Stop_Price, 2), " / Kelly : ", round(Kelly, 4)))
  print(paste0("Net_Return : $", round(Net_Return, 2), " / Bet : $", round(Stop_Price*100*Maximum_Contracts_Num, 2), " / Loss (w/ Commissions): $", round(Stop_Price*100*Maximum_Contracts_Num+Commissions, 2)))
  print(paste0("Net_Return_Rate (w/ Commissions) : ", (round(b, 4)-1)*100, "%"))
}

# Kelly is sometimes risky, so let's take a certain proportion of the original kelly for satefy
Alpha=1/3
New_Kelly=Kelly*Alpha
New_Stop_Price=New_Kelly*Capital/100/Maximum_Contracts_Num
if(New_Stop_Price>Bid_Price){ # x > 0 equivalently translantes to New_Stop_Price > Return/100
  print(paste0("Bid_Price : ", round(Bid_Price, 2), " / New_Stop_Price : ", round(New_Stop_Price, 2), " / New_Kelly : ", round(New_Kelly, 4)))
  print(paste0("Net_Return : $", round(Net_Return, 2), " / Bet : $", round(New_Stop_Price*100*Maximum_Contracts_Num, 2), " / Loss (w/ Commissions): $", round(New_Stop_Price*100*Maximum_Contracts_Num+Commissions, 2)))
  print(paste0("Net_Return_Rate (w/ Commissions) : ", round((Net_Return/(New_Stop_Price*100*Maximum_Contracts_Num+Commissions))*100, 2), "%"))
}

round(Kelly, 5)==round((Stop_Price*100*Maximum_Contracts_Num)/Capital, 5)
round(Capital, 1)==round((Stop_Price*100)/Kelly*Maximum_Contracts_Num, 1)

round(New_Kelly, 5)==round((New_Stop_Price*100)/Capital*Maximum_Contracts_Num, 5)
round(Capital, 1)==round((New_Stop_Price*100)/New_Kelly*Maximum_Contracts_Num, 1)



#***************
# long strategy
#***************
# determine stop-price (stop-long)
A=0.06 # ask price
C=8500 # capital
Contracts_Num=1
Commissions=1.04*2*Contracts_Num
P=1-0.91 # 1 - max loss chance

# PP : profit price (short order)
PP=((1-P)*(A+Commissions/100/Contracts_Num))/(P-1/C*(100*A*Contracts_Num+Commissions))+(A+Commissions/100/Contracts_Num)

b=(100*PP*Contracts_Num)/(100*A*Contracts_Num+Commissions)
(P)-(1-P)/(b-1)
(Commissions+A*100*Contracts_Num)/C

#
Net_Return=100*(PP-A)*Contracts_Num-Commissions
if(PP>A){ # x > 0 equivalently translantes to Profit_price > Ask price
  print(paste0("Ask_Price : ", round(A, 2), " / Profit_Price : ", round(PP, 2), " / Kelly : ", round((P)-(1-P)/(b-1), 4), " / Bet : $", round(A*100*Contracts_Num, 2), " / Loss (w/ Commissions): $", round(A*100*Contracts_Num+Commissions, 2)))
  print(paste0("Return : $", round(Net_Return+A*100*Contracts_Num+Commissions, 2), " / Return_Rate (w/ Commissions) : ", round(((Net_Return+A*100*Contracts_Num+Commissions)/(A*100*Contracts_Num+Commissions))*100, 2), "%"))
  print(paste0("Net_Return : $", round(Net_Return, 2), " / Net_Return_Rate (w/ Commissions) : ", round((Net_Return/(A*100*Contracts_Num+Commissions))*100, 2), "%"))
  
}






