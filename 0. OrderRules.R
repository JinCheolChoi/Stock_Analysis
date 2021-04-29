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
  Position_Direction="both",
  Cut_Loss=10,
  Profit_Threshold=10
)





#***************
#
# BuyToOpen ----
#
#*****************************
OrderRules_Env$BuyToOpen=list(
  OrderType="MKT",
  Quantity=1,
  Min_Sig_N=1 # minimum number of positive signals from models to transmit
)
class(OrderRules_Env$BuyToOpen)="Long_Position"




#****************
#
# BuyToClose ----
#
#******************************
OrderRules_Env$BuyToClose=list(
  OrderType="MKT",
  Quantity=1,
  Min_Sig_N=1 # minimum number of positive signals from models to transmit
)
class(OrderRules_Env$BuyToClose)="Short_Position"




#****************
#
# SellToOpen ----
#
#******************************
OrderRules_Env$SellToOpen=list(
  OrderType="MKT",
  Quantity=1,
  Min_Sig_N=1 # minimum number of positive signals from models to transmit
)
class(OrderRules_Env$SellToOpen)="Short_Position"




#*****************
#
# SellToClose ----
#
#*******************************
OrderRules_Env$SellToClose=list(
  OrderType="MKT",
  Quantity=1,
  Min_Sig_N=1 # minimum number of positive signals from models to transmit
)
class(OrderRules_Env$SellToClose)="Long_Position"



