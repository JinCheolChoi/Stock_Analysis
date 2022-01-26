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
                           Long_PctB=Params$Simple_BBands_1_Long_PctB[i],
                           Short_PctB=Inf))
Add_Model(Strategy="Test_Strategy",
          Model="Simple_BBands_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=-Inf,
                           Short_PctB=Params$Simple_BBands_2_Short_PctB[i]))
Add_Model(Strategy="Test_Strategy",
          Model="Simple_RSI_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=Params$Simple_BBands_1_Long_PctB[i]*100,
                           Short_RSI=Inf))
Add_Model(Strategy="Test_Strategy",
          Model="Simple_RSI_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=-Inf,
                           Short_RSI=Params$Simple_BBands_2_Short_PctB[i]*100))
Add_Model(Strategy="Test_Strategy",
          Model="Trend",
          ModelParams=list(Interval=5,
                           Extent=5))

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
                                   Stop_Order=Params$Stop_Order[i],
                                   Profit_Order=Params$Profit_Order[i],
                                   Reverse=TRUE))
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
Add_Model(Strategy="Long_Strategy",
          Model="Simple_BBands_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=Params$Simple_BBands_1_Long_PctB[i],
                           Short_PctB=Inf))
Add_Model(Strategy="Long_Strategy",
          Model="Simple_BBands_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=-Inf,
                           Short_PctB=Params$Simple_BBands_2_Short_PctB[i]))
Add_Model(Strategy="Long_Strategy",
          Model="Simple_RSI_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=Params$Simple_BBands_1_Long_PctB[i]*100,
                           Short_RSI=Inf))
Add_Model(Strategy="Long_Strategy",
          Model="Simple_RSI_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=-Inf,
                           Short_RSI=Params$Simple_BBands_2_Short_PctB[i]*100))
Add_Model(Strategy="Long_Strategy",
          Model="Trend",
          ModelParams=list(Interval=5,
                           Extent=5))

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
                                   Stop_Order=Params$Stop_Order[i],
                                   Profit_Order=Params$Profit_Order[i],
                                   Reverse=TRUE))
Add_OrderRule(Strategy="Long_Strategy",
              OrderRule="Long",
              OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
                                                  Quantity=1,
                                                  Min_Sig_N=3),
                                   SellToClose=list(OrderType="MKT",
                                                    Quantity=1,
                                                    Min_Sig_N=5)))
# Add_OrderRule(Strategy="Long_Strategy",
#               OrderRule="Short",
#               OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
#                                                    Quantity=1,
#                                                    Min_Sig_N=3), # minimum number of positive signals from models to transmit
#                                    BuyToClose=list(OrderType="MKT",
#                                                    Quantity=1,
#                                                    Min_Sig_N=5))) # minimum number of positive signals from models to transmit





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
                           Long_PctB=Params$Simple_BBands_1_Long_PctB[i],
                           Short_PctB=Inf))
Add_Model(Strategy="Short_Strategy",
          Model="Simple_BBands_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=-Inf,
                           Short_PctB=Params$Simple_BBands_2_Short_PctB[i]))
Add_Model(Strategy="Short_Strategy",
          Model="Simple_RSI_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=Params$Simple_BBands_1_Long_PctB[i]*100,
                           Short_RSI=Inf))
Add_Model(Strategy="Short_Strategy",
          Model="Simple_RSI_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=-Inf,
                           Short_RSI=Params$Simple_BBands_2_Short_PctB[i]*100))
Add_Model(Strategy="Short_Strategy",
          Model="Trend",
          ModelParams=list(Interval=5,
                           Extent=5))

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
                                   Stop_Order=Params$Stop_Order[i],
                                   Profit_Order=Params$Profit_Order[i],
                                   Reverse=TRUE))
# Add_OrderRule(Strategy="Short_Strategy",
#               OrderRule="Long",
#               OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
#                                                   Quantity=1,
#                                                   Min_Sig_N=3),
#                                    SellToClose=list(OrderType="MKT",
#                                                     Quantity=1,
#                                                     Min_Sig_N=5)))
Add_OrderRule(Strategy="Short_Strategy",
              OrderRule="Short",
              OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=3), # minimum number of positive signals from models to transmit
                                   BuyToClose=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=5))) # minimum number of positive signals from models to transmit



#*******************
#
# Test_Strategy_1 ---- 
#
#*******************
# initiate a strategy called "Test_Strategy_1"
Init_Strategy(Name="Test_Strategy_1",
              Max_Rows=50) # the maximum number of rows in a temp dataset to parse


#**************
# add indicator
#**************
Add_Indicator(Strategy="Test_Strategy_1",
              Indicator="BBands",
              IndicatorParams=list(n=20,
                                   sd=2)) # default n=20, sd=2

Add_Indicator(Strategy="Test_Strategy_1",
              Indicator="RSI",
              IndicatorParams=list(n=9))

Add_Indicator(Strategy="Test_Strategy_1",
              Indicator="Close")


#********************************************************************************************
# add model (to run in combination with other included models to decide to transmit an order)
#********************************************************************************************
Add_Model(Strategy="Test_Strategy_1",
          Model="Simple_BBands_1",
          ModelParams=list(Long_Consec_Times=2,
                           Short_Consec_Times=2,
                           Long_PctB=Params$Simple_BBands_1_Long_PctB[i],
                           Short_PctB=Params$Simple_BBands_1_Short_PctB[i],
                           Simulation_Trading=FALSE))
Add_Model(Strategy="Test_Strategy_1",
          Model="Simple_BBands_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=Params$Simple_BBands_2_Long_PctB[i],
                           Short_PctB=Params$Simple_BBands_2_Short_PctB[i],
                           Simulation_Trading=FALSE))
Add_Model(Strategy="Test_Strategy_1",
          Model="Simple_RSI_1",
          ModelParams=list(Long_Consec_Times=2,
                           Short_Consec_Times=2,
                           Long_RSI=Params$Simple_BBands_1_Long_PctB[i]*100,
                           Short_RSI=Params$Simple_BBands_1_Short_PctB[i]*100,
                           Simulation_Trading=FALSE))
Add_Model(Strategy="Test_Strategy_1",
          Model="Simple_RSI_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=Params$Simple_BBands_2_Long_PctB[i]*100,
                           Short_RSI=Params$Simple_BBands_2_Short_PctB[i]*100,
                           Simulation_Trading=FALSE))
# Add_Model(Strategy="Test_Strategy_1",
#           Model="Trend",
#           ModelParams=list(Interval=5,
#                            Extent=10,
#                            Simulation_Trading=FALSE))

# Test_Strategy_1$Indicators$BBands
# Test_Strategy_1$Models$Simple_BBands
# Test_Strategy_1$Order_Rules$SellToClose
#***************
# add order rule
#***************
Add_OrderRule(Strategy="Test_Strategy_1",
              OrderRule="General",
              OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
                                   Scenario="Negative", # Positive : early profit is prioritized over loss cut
                                   Stop_Order=10000000,
                                   Profit_Order=10000000,
                                   Reverse=FALSE)) # Opposite actions are made if Reverse=TRUE (haven't been applied yet)
Add_OrderRule(Strategy="Test_Strategy_1",
              OrderRule="Long",
              OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
                                                  Quantity=1,
                                                  Min_Sig_N=4),
                                   SellToClose=list(OrderType="MKT",
                                                    Quantity=1,
                                                    Min_Sig_N=1)))
Add_OrderRule(Strategy="Test_Strategy_1",
              OrderRule="Short",
              OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=4), # minimum number of positive signals from models to transmit
                                   BuyToClose=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=1))) # minimum number of positive signals from models to transmit


#*******************
#
# Test_Strategy_2 ---- 
#
#*******************
# initiate a strategy called "Test_Strategy_2"
Init_Strategy(Name="Test_Strategy_2",
              Max_Rows=50) # the maximum number of rows in a temp dataset to parse


#**************
# add indicator
#**************
Add_Indicator(Strategy="Test_Strategy_2",
              Indicator="BBands",
              IndicatorParams=list(n=20,
                                   sd=2)) # default n=20, sd=2

Add_Indicator(Strategy="Test_Strategy_2",
              Indicator="RSI",
              IndicatorParams=list(n=9))

Add_Indicator(Strategy="Test_Strategy_2",
              Indicator="Close")


#********************************************************************************************
# add model (to run in combination with other included models to decide to transmit an order)
#********************************************************************************************
Add_Model(Strategy="Test_Strategy_2",
          Model="Simple_BBands_1",
          ModelParams=list(Long_Consec_Times=2,
                           Short_Consec_Times=2,
                           Long_PctB=Params$Simple_BBands_1_Long_PctB[i],
                           Short_PctB=Params$Simple_BBands_1_Short_PctB[i],
                           Simulation_Trading=TRUE))
Add_Model(Strategy="Test_Strategy_2",
          Model="Simple_BBands_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=Params$Simple_BBands_2_Long_PctB[i],
                           Short_PctB=Params$Simple_BBands_2_Short_PctB[i],
                           Simulation_Trading=TRUE))
Add_Model(Strategy="Test_Strategy_2",
          Model="Simple_RSI_1",
          ModelParams=list(Long_Consec_Times=2,
                           Short_Consec_Times=2,
                           Long_RSI=Params$Simple_BBands_1_Long_PctB[i]*100,
                           Short_RSI=Params$Simple_BBands_1_Short_PctB[i]*100,
                           Simulation_Trading=TRUE))
Add_Model(Strategy="Test_Strategy_2",
          Model="Simple_RSI_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=Params$Simple_BBands_2_Long_PctB[i]*100,
                           Short_RSI=Params$Simple_BBands_2_Short_PctB[i]*100,
                           Simulation_Trading=TRUE))
# Add_Model(Strategy="Test_Strategy_2",
#           Model="Trend",
#           ModelParams=list(Interval=5,
#                            Extent=10,
#                            Simulation_Trading=TRUE))

# Test_Strategy_2$Indicators$BBands
# Test_Strategy_2$Models$Simple_BBands
# Test_Strategy_2$Order_Rules$SellToClose
#***************
# add order rule
#***************
Add_OrderRule(Strategy="Test_Strategy_2",
              OrderRule="General",
              OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
                                   Scenario="Negative", # Positive : early profit is prioritized over loss cut
                                   Stop_Order=10000000,
                                   Profit_Order=10000000,
                                   Reverse=FALSE)) # Opposite actions are made if Reverse=TRUE (haven't been applied yet)
Add_OrderRule(Strategy="Test_Strategy_2",
              OrderRule="Long",
              OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
                                                  Quantity=1,
                                                  Min_Sig_N=4),
                                   SellToClose=list(OrderType="MKT",
                                                    Quantity=1,
                                                    Min_Sig_N=1)))
Add_OrderRule(Strategy="Test_Strategy_2",
              OrderRule="Short",
              OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=4), # minimum number of positive signals from models to transmit
                                   BuyToClose=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=1))) # minimum number of positive signals from models to transmit


