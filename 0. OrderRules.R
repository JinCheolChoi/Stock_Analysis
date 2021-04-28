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
#*************************
OrderRules_Env$General=list(
  Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
  Position_Direction="both",
  Cut_Loss=10,
  Profit_Threshold=10
)





#***************
#
# BuyToOpen ----
#
#*************************
OrderRules_Env$BuyToOpen=list(
  OrderType="MKT",
  Quantity=1,
  Min_Sig_N=1 # minimum number of positive signals from models to transmit
)





#****************
#
# BuyToClose ----
#
#*************************
OrderRules_Env$BuyToClose=list(
  OrderType="MKT",
  Quantity=1,
  Min_Sig_N=1 # minimum number of positive signals from models to transmit
)





#****************
#
# SellToOpen ----
#
#*************************
OrderRules_Env$SellToOpen=list(
  OrderType="MKT",
  Quantity=1,
  Min_Sig_N=1 # minimum number of positive signals from models to transmit
)





#*****************
#
# SellToClose ----
#
#*************************
OrderRules_Env$SellToClose=list(
  OrderType="MKT",
  Quantity=1,
  Min_Sig_N=1 # minimum number of positive signals from models to transmit
)




