#************************
#
# Required functions ---- 
#
#***********************************************************
source(paste0(working.dir, "0. Models.R"))
source(paste0(working.dir, "0. OrderRules.R"))



#*******************
#
# Live_Strategy ---- 
#
#*******************
# initiate a strategy called "Live_Strategy"
Init_Strategy(Name="Live_Strategy",
              Max_Rows=50) # the maximum number of rows in a temp dataset to parse


#**************
# add indicator
#**************
# Add_Indicator(Strategy="Live_Strategy",
#               Indicator="BBands",
#               IndicatorParams=list(n=20,
#                                    sd=2)) # default n=20, sd=2

Add_Indicator(Strategy="Live_Strategy",
              Indicator="RSI",
              IndicatorParams=list(n=4))

Add_Indicator(Strategy="Live_Strategy",
              Indicator="Close")


#********************************************************************************************
# add model (to run in combination with other included models to decide to transmit an order)
#********************************************************************************************
# Add_Model(Strategy="Live_Strategy",
#           Model="Simple_BBands_1",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_PctB=0.4,
#                            Short_PctB=Inf))
# Add_Model(Strategy="Live_Strategy",
#           Model="Simple_BBands_2",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_PctB=-Inf,
#                            Short_PctB=0.6))
Add_Model(Strategy="Live_Strategy",
          Model="Simple_RSI_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=0.4*100,
                           Short_RSI=Inf))
Add_Model(Strategy="Live_Strategy",
          Model="Simple_RSI_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=-Inf,
                           Short_RSI=0.6*100))
Add_Model(Strategy="Live_Strategy",
          Model="Trend",
          ModelParams=list(Interval=2,
                           Extent=0))
  
# Live_Strategy$Indicators$BBands
# Live_Strategy$Models$Simple_BBands
# Live_Strategy$Order_Rules$SellToClose
#***************
# add order rule
#***************
Add_OrderRule(Strategy="Live_Strategy",
              OrderRule="General",
              OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
                                   Scenario="Negative", # Positive : early profit is prioritized over loss cut
                                   Stop_Order=500000,
                                   Profit_Order=500000,
                                   Trend=TRUE))
Add_OrderRule(Strategy="Live_Strategy",
              OrderRule="Long",
              OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
                                                  Quantity=1,
                                                  Min_Sig_N=2),
                                   SellToClose=list(OrderType="MKT",
                                                    Quantity=1,
                                                    Min_Sig_N=2)))
Add_OrderRule(Strategy="Live_Strategy",
              OrderRule="Short",
              OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=2), # minimum number of positive signals from models to transmit
                                   BuyToClose=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=2))) # minimum number of positive signals from models to transmit


