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
  Cut_Loss=10,
  Profit_Threshold=10
)





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
    Min_Sig_N=1) # minimum number of positive signals from models to transmit
)





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
  BuyToClose=list(rderType="MKT",
                  Quantity=1,
                  Min_Sig_N=1) # minimum number of positive signals from models to transmit
  
)





