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
  Position_Direction="both"
)





#***************
#
# BuyToOpen ----
#
#*************************
OrderRules_Env$BuyToOpen=list(
  OrderType="MKT",
  Quantity=1
)





#****************
#
# BuyToClose ----
#
#*************************
OrderRules_Env$BuyToClose=list(
  OrderType="MKT",
  Quantity=1
)





#****************
#
# SellToOpen ----
#
#*************************
OrderRules_Env$SellToOpen=list(
  OrderType="MKT",
  Quantity=1
)





#*****************
#
# SellToClose ----
#
#*************************
OrderRules_Env$SellToClose=list(
  OrderType="MKT",
  Quantity=1
)




