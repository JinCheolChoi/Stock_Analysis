#************************
#
# Required functions ---- 
#
#***********************************************************
source(paste0(working.dir, "0. Models.R"))
source(paste0(working.dir, "0. OrderRules.R"))



#*******************
#
# Test_Strategy ---- 
#
#*******************
# initiate a strategy called "Test_Strategy"
Init_Strategy(Name="Test_Strategy",
              Max_Rows=50) # the maximum number of rows in a temp dataset to parse


#**************
# add indicator
#**************
Add_Indicator(Strategy="Test_Strategy",
              Indicator="BBands",
              IndicatorParams=list(n=20,
                                   sd=2)) # default n=20, sd=2

Add_Indicator(Strategy="Test_Strategy",
              Indicator="RSI",
              IndicatorParams=list(n=9))

Add_Indicator(Strategy="Test_Strategy",
              Indicator="Close")


#********************************************************************************************
# add model (to run in combination with other included models to decide to transmit an order)
#********************************************************************************************
Add_Model(Strategy="Test_Strategy",
          Model="Simple_BBands_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=0.25,
                           Short_PctB=Inf))
Add_Model(Strategy="Test_Strategy",
          Model="Simple_BBands_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=-Inf,
                           Short_PctB=0.7))
Add_Model(Strategy="Test_Strategy",
          Model="Simple_RSI_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=25,
                           Short_RSI=Inf))
Add_Model(Strategy="Test_Strategy",
          Model="Simple_RSI_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=-Inf,
                           Short_RSI=70))
Add_Model(Strategy="Test_Strategy",
          Model="Trend",
          ModelParams=list(Interval=5,
                           Extent=1))

# Test_Strategy$Indicators$BBands
# Test_Strategy$Models$Simple_BBands
# Test_Strategy$Order_Rules$SellToClose
#***************
# add order rule
#***************
Add_OrderRule(Strategy="Test_Strategy",
              OrderRule="General",
              OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
                                   Scenario="Negative", # Positive : early profit is prioritized over loss cut
                                   Stop_Order=300,
                                   Profit_Order=10,
                                   Trend=TRUE))
Add_OrderRule(Strategy="Test_Strategy",
              OrderRule="Long",
              OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
                                                  Quantity=1,
                                                  Min_Sig_N=3),
                                   SellToClose=list(OrderType="MKT",
                                                    Quantity=1,
                                                    Min_Sig_N=5)))
Add_OrderRule(Strategy="Test_Strategy",
              OrderRule="Short",
              OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=3), # minimum number of positive signals from models to transmit
                                   BuyToClose=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=5))) # minimum number of positive signals from models to transmit




#*******************
#
# Long_Strategy ---- 
#
#*******************
# initiate a strategy called "Long_Strategy"
Init_Strategy(Name="Long_Strategy",
              Max_Rows=50) # the maximum number of rows in a temp dataset to parse


#**************
# add indicator
#**************
Add_Indicator(Strategy="Long_Strategy",
              Indicator="BBands",
              IndicatorParams=list(n=20,
                                   sd=2)) # default n=20, sd=2

Add_Indicator(Strategy="Long_Strategy",
              Indicator="RSI",
              IndicatorParams=list(n=9))

Add_Indicator(Strategy="Long_Strategy",
              Indicator="Close")


#********************************************************************************************
# add model (to run in combination with other included models to decide to transmit an order)
#********************************************************************************************
# Add_Model(Strategy="Long_Strategy",
#           Model="Simple_BBands_1",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_PctB=0.25,
#                            Short_PctB=Inf))
Add_Model(Strategy="Long_Strategy",
          Model="Simple_BBands_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=-Inf,
                           Short_PctB=0.7))
# Add_Model(Strategy="Long_Strategy",
#           Model="Simple_RSI_1",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_RSI=25,
#                            Short_RSI=Inf))
Add_Model(Strategy="Long_Strategy",
          Model="Simple_RSI_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=-Inf,
                           Short_RSI=70))
Add_Model(Strategy="Long_Strategy",
          Model="Trend",
          ModelParams=list(Interval=5,
                           Extent=1))

# Long_Strategy$Indicators$BBands
# Long_Strategy$Models$Simple_BBands
# Long_Strategy$Order_Rules$SellToClose
#***************
# add order rule
#***************
Add_OrderRule(Strategy="Long_Strategy",
              OrderRule="General",
              OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
                                   Scenario="Negative", # Positive : early profit is prioritized over loss cut
                                   Stop_Order=10,
                                   Profit_Order=100,
                                   Trend=TRUE))
Add_OrderRule(Strategy="Long_Strategy",
              OrderRule="Long",
              OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
                                                  Quantity=1,
                                                  Min_Sig_N=3),
                                   SellToClose=list(OrderType="MKT",
                                                    Quantity=1,
                                                    Min_Sig_N=3)))
# Add_OrderRule(Strategy="Long_Strategy",
#               OrderRule="Short",
#               OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
#                                                    Quantity=1,
#                                                    Min_Sig_N=3), # minimum number of positive signals from models to transmit
#                                    BuyToClose=list(OrderType="MKT",
#                                                    Quantity=1,
#                                                    Min_Sig_N=3))) # minimum number of positive signals from models to transmit





#*******************
#
# Short_Strategy ---- 
#
#*******************
# initiate a strategy called "Short_Strategy"
Init_Strategy(Name="Short_Strategy",
              Max_Rows=50) # the maximum number of rows in a temp dataset to parse


#**************
# add indicator
#**************
Add_Indicator(Strategy="Short_Strategy",
              Indicator="BBands",
              IndicatorParams=list(n=20,
                                   sd=2)) # default n=20, sd=2

Add_Indicator(Strategy="Short_Strategy",
              Indicator="RSI",
              IndicatorParams=list(n=9))

Add_Indicator(Strategy="Short_Strategy",
              Indicator="Close")


#********************************************************************************************
# add model (to run in combination with other included models to decide to transmit an order)
#********************************************************************************************
Add_Model(Strategy="Short_Strategy",
          Model="Simple_BBands_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=0.25,
                           Short_PctB=Inf))
# Add_Model(Strategy="Short_Strategy",
#           Model="Simple_BBands_2",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_PctB=-Inf,
#                            Short_PctB=0.7))
Add_Model(Strategy="Short_Strategy",
          Model="Simple_RSI_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=25,
                           Short_RSI=Inf))
# Add_Model(Strategy="Short_Strategy",
#           Model="Simple_RSI_2",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_RSI=-Inf,
#                            Short_RSI=70))
Add_Model(Strategy="Short_Strategy",
          Model="Trend",
          ModelParams=list(Interval=5,
                           Extent=1))

# Short_Strategy$Indicators$BBands
# Short_Strategy$Models$Simple_BBands
# Short_Strategy$Order_Rules$SellToClose
#***************
# add order rule
#***************
Add_OrderRule(Strategy="Short_Strategy",
              OrderRule="General",
              OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
                                   Scenario="Negative", # Positive : early profit is prioritized over loss cut
                                   Stop_Order=10,
                                   Profit_Order=100,
                                   Trend=TRUE))
# Add_OrderRule(Strategy="Short_Strategy",
#               OrderRule="Long",
#               OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
#                                                   Quantity=1,
#                                                   Min_Sig_N=3),
#                                    SellToClose=list(OrderType="MKT",
#                                                     Quantity=1,
#                                                     Min_Sig_N=3)))
Add_OrderRule(Strategy="Short_Strategy",
              OrderRule="Short",
              OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=3), # minimum number of positive signals from models to transmit
                                   BuyToClose=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=3))) # minimum number of positive signals from models to transmit






#*******************
#
# Best_Strategy ---- 
#
#*******************
# initiate a strategy called "Best_Strategy"
Init_Strategy(Name="Best_Strategy",
              Max_Rows=50) # the maximum number of rows in a temp dataset to parse


#**************
# add indicator
#**************
Add_Indicator(Strategy="Best_Strategy",
              Indicator="BBands",
              IndicatorParams=list(n=20,
                                   sd=2)) # default n=20, sd=2

Add_Indicator(Strategy="Best_Strategy",
              Indicator="RSI",
              IndicatorParams=list(n=9))

Add_Indicator(Strategy="Best_Strategy",
              Indicator="Close")


#********************************************************************************************
# add model (to run in combination with other included models to decide to transmit an order)
#********************************************************************************************
Add_Model(Strategy="Best_Strategy",
          Model="Simple_BBands_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=0.25,
                           Short_PctB=Inf))
Add_Model(Strategy="Best_Strategy",
          Model="Simple_BBands_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=-Inf,
                           Short_PctB=0.7))
Add_Model(Strategy="Best_Strategy",
          Model="Simple_RSI_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=25,
                           Short_RSI=Inf))
Add_Model(Strategy="Best_Strategy",
          Model="Simple_RSI_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=-Inf,
                           Short_RSI=70))
Add_Model(Strategy="Best_Strategy",
          Model="Trend",
          ModelParams=list(Interval=5,
                           Extent=1))

# Best_Strategy$Indicators$BBands
# Best_Strategy$Models$Simple_BBands
# Best_Strategy$Order_Rules$SellToClose
#***************
# add order rule
#***************
Add_OrderRule(Strategy="Best_Strategy",
              OrderRule="General",
              OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
                                   Scenario="Negative", # Positive : early profit is prioritized over loss cut
                                   Stop_Order=10,
                                   Profit_Order=100,
                                   Trend=TRUE))
Add_OrderRule(Strategy="Best_Strategy",
              OrderRule="Long",
              OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
                                                  Quantity=1,
                                                  Min_Sig_N=3),
                                   SellToClose=list(OrderType="MKT",
                                                    Quantity=1,
                                                    Min_Sig_N=5)))
Add_OrderRule(Strategy="Best_Strategy",
              OrderRule="Short",
              OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=3), # minimum number of positive signals from models to transmit
                                   BuyToClose=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=5))) # minimum number of positive signals from models to transmit



