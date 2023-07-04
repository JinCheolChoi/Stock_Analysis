#************************
#
# Required functions ---- 
#
#***********************************************************
source(paste0(working.dir, "0. Models.R"))
source(paste0(working.dir, "0. OrderRules.R"))


#************************
#
# Combined_Strategy ----
#
#*************************
# initiate a strategy called "Combined_Strategy"
Init_Strategy(Name="Combined_Strategy",
              Max_Rows=50) # the maximum number of rows in a temp dataset to parse


#**************
# add indicator
#**************
Add_Indicator(Strategy="Combined_Strategy",
              Indicator="BBands",
              IndicatorParams=list(n=20,
                                   sd=2)) # default n=20, sd=2

Add_Indicator(Strategy="Combined_Strategy",
              Indicator="RSI",
              IndicatorParams=list(n=10))

Add_Indicator(Strategy="Combined_Strategy",
              Indicator="Close")


#********************************************************************************************
# add model (to run in combination with other included models to decide to transmit an order)
#********************************************************************************************
Add_Model(Strategy="Combined_Strategy",
          Model="RSI_Averages_Band",
          Model_Name="RSI_Averages_Band_1",
          ModelParams=list(MA_Length=10,
                           RSI_RSI_MA_Diff_Min=0,
                           RSI_RSI_MA_Diff_Max=Inf,
                           Early_Execution_Gap=Inf,
                           Live_Trading=Live_Trading,
                           Reverse=Params$Reverse[i]))
Add_Model(Strategy="Combined_Strategy",
          Model="Simple_BBands",
          Model_Name="Simple_BBands_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=Params$Simple_BBands_1_Long_PctB[i],
                           Short_PctB=Params$Simple_BBands_1_Short_PctB[i],
                           Live_Trading=Live_Trading,
                           Reverse=Params$Reverse[i]))
Add_Model(Strategy="Combined_Strategy",
          Model="Simple_RSI",
          Model_Name="Simple_RSI_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=Params$Simple_BBands_1_Long_PctB[i]*Params$Multiplier[i],
                           Short_RSI=Params$Simple_BBands_1_Short_PctB[i]*Params$Multiplier[i],
                           Live_Trading=Live_Trading,
                           Reverse=Params$Reverse[i]))


#***************
# add order rule
#***************
Add_OrderRule(Strategy="Combined_Strategy",
              OrderRule="General",
              OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
                                   Scenario="Negative", # Positive : early profit is prioritized over loss cut
                                   Stop_Order=Inf,
                                   Profit_Order=Inf,
                                   Maximum_Elapsed_Time=Inf,
                                   Commission=0.62,
                                   Tick_Size=0.25, # the minimum tick size
                                   Penalty=1,# the number of ticks away from the spot price when MKT is filled
                                   Tick_Value=0.5, # value per tick
                                   Market_Time=Params$Market_Time[i]))
Add_OrderRule(Strategy="Combined_Strategy",
              OrderRule="Long",
              OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
                                                  Quantity=1,
                                                  Min_Sig_N=3),
                                   SellToClose=list(OrderType="MKT",
                                                    Quantity=1,
                                                    Min_Sig_N=1)))
Add_OrderRule(Strategy="Combined_Strategy",
              OrderRule="Short",
              OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=3), # minimum number of positive signals from models to transmit
                                   BuyToClose=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=1))) # minimum number of positive signals from models to transmit



# #************************
# #
# # Long_Short_Strategy ----
# #
# #*************************
# # initiate a strategy called "Long_Short_Strategy"
# Init_Strategy(Name="Long_Short_Strategy",
#               Max_Rows=50) # the maximum number of rows in a temp dataset to parse
# 
# 
# #**************
# # add indicator
# #**************
# Add_Indicator(Strategy="Long_Short_Strategy",
#               Indicator="BBands",
#               IndicatorParams=list(n=20,
#                                    sd=2)) # default n=20, sd=2
# 
# Add_Indicator(Strategy="Long_Short_Strategy",
#               Indicator="RSI",
#               IndicatorParams=list(n=10))
# 
# Add_Indicator(Strategy="Long_Short_Strategy",
#               Indicator="Close")
# 
# 
# #********************************************************************************************
# # add model (to run in combination with other included models to decide to transmit an order)
# #********************************************************************************************
# Add_Model(Strategy="Long_Short_Strategy",
#           Model="Simple_BBands",
#           Model_Name="Simple_BBands_1",
#           ModelParams=list(Long_Consec_Times=Params$Open_Long_Consec_Times[i],
#                            Short_Consec_Times=Params$Open_Short_Consec_Times[i],
#                            Long_PctB=Params$Simple_BBands_1_Long_PctB[i],
#                            Short_PctB=Params$Simple_BBands_1_Short_PctB[i],
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="Long_Short_Strategy",
#           Model="Simple_BBands",
#           Model_Name="Simple_BBands_2",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_PctB=Params$Simple_BBands_2_Long_PctB[i],
#                            Short_PctB=Params$Simple_BBands_2_Short_PctB[i],
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="Long_Short_Strategy",
#           Model="Simple_RSI",
#           Model_Name="Simple_RSI_1",
#           ModelParams=list(Long_Consec_Times=Params$Open_Long_Consec_Times[i],
#                            Short_Consec_Times=Params$Open_Short_Consec_Times[i],
#                            Long_RSI=Params$Simple_BBands_1_Long_PctB[i]*Params$Multiplier[i],
#                            Short_RSI=Params$Simple_BBands_1_Short_PctB[i]*Params$Multiplier[i],
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="Long_Short_Strategy",
#           Model="Simple_RSI",
#           Model_Name="Simple_RSI_2",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_RSI=Params$Simple_BBands_2_Long_PctB[i]*Params$Multiplier[i],
#                            Short_RSI=Params$Simple_BBands_2_Short_PctB[i]*Params$Multiplier[i],
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# # Add_Model(Strategy="Long_Short_Strategy",
# #           Model="Trend",
# #           ModelParams=list(Interval=5,
# #                            Extent=10,
# #                            Live_Trading=FALSE))
# 
# # Long_Short_Strategy$Indicators$BBands
# # Long_Short_Strategy$Models$Simple_BBands
# # Long_Short_Strategy$Order_Rules$SellToClose
# #***************
# # add order rule
# #***************
# Add_OrderRule(Strategy="Long_Short_Strategy",
#               OrderRule="General",
#               OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
#                                    Scenario="Negative", # Positive : early profit is prioritized over loss cut
#                                    Stop_Order=Inf,
#                                    Profit_Order=Inf,
#                                    Maximum_Elapsed_Time=Inf,
#                                    Commission=0.62,
#                                    Tick_Size=0.25, # the minimum tick size
#                                    Penalty=1,# the number of ticks away from the spot price when MKT is filled
#                                    Tick_Value=0.5, # value per tick
#                                    Market_Time=Params$Market_Time[i]))
# Add_OrderRule(Strategy="Long_Short_Strategy",
#               OrderRule="Long",
#               OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
#                                                   Quantity=1,
#                                                   Min_Sig_N=4),
#                                    SellToClose=list(OrderType="MKT",
#                                                     Quantity=1,
#                                                     Min_Sig_N=1)))
# Add_OrderRule(Strategy="Long_Short_Strategy",
#               OrderRule="Short",
#               OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
#                                                    Quantity=1,
#                                                    Min_Sig_N=4), # minimum number of positive signals from models to transmit
#                                    BuyToClose=list(OrderType="MKT",
#                                                    Quantity=1,
#                                                    Min_Sig_N=1))) # minimum number of positive signals from models to transmit
# 
# 
# 
# 
# 
# #*******************
# #
# # Long_Strategy ----
# #
# #*******************
# # initiate a strategy called "Long_Strategy"
# Init_Strategy(Name="Long_Strategy",
#               Max_Rows=50) # the maximum number of rows in a temp dataset to parse
# 
# 
# #**************
# # add indicator
# #**************
# Add_Indicator(Strategy="Long_Strategy",
#               Indicator="BBands",
#               IndicatorParams=list(n=20,
#                                    sd=2)) # default n=20, sd=2
# 
# Add_Indicator(Strategy="Long_Strategy",
#               Indicator="RSI",
#               IndicatorParams=list(n=9))
# 
# Add_Indicator(Strategy="Long_Strategy",
#               Indicator="Close")
# 
# 
# #********************************************************************************************
# # add model (to run in combination with other included models to decide to transmit an order)
# #********************************************************************************************
# Add_Model(Strategy="Long_Strategy",
#           Model="Simple_BBands",
#           Model_Name="Simple_BBands_1",
#           ModelParams=list(Long_Consec_Times=4,
#                            Short_Consec_Times=4,
#                            Long_PctB=Params$Simple_BBands_1_Long_PctB[i],
#                            Short_PctB=Params$Simple_BBands_2_Short_PctB[i],
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="Long_Strategy",
#           Model="Simple_BBands",
#           Model_Name="Simple_BBands_2",
#           ModelParams=list(Long_Consec_Times=4,
#                            Short_Consec_Times=4,
#                            Long_PctB=-Inf,
#                            Short_PctB=Params$Simple_BBands_2_Short_PctB[i],
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="Long_Strategy",
#           Model="Simple_RSI",
#           Model_Name="Simple_RSI_1",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_RSI=Params$Simple_BBands_1_Long_PctB[i]*100,
#                            Short_RSI=Inf,
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="Long_Strategy",
#           Model="Simple_RSI",
#           Model_Name="Simple_RSI_2",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_RSI=-Inf,
#                            Short_RSI=Params$Simple_BBands_2_Short_PctB[i]*100,
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# # Add_Model(Strategy="Long_Strategy",
# #           Model="Trend",
# #           ModelParams=list(Interval=5,
# #                            Extent=5,
# #                            Live_Trading=Live_Trading))
# 
# # Long_Strategy$Indicators$BBands
# # Long_Strategy$Models$Simple_BBands
# # Long_Strategy$Order_Rules$SellToClose
# #***************
# # add order rule
# #***************
# Add_OrderRule(Strategy="Long_Strategy",
#               OrderRule="General",
#               OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
#                                    Scenario="Negative", # Positive : early profit is prioritized over loss cut
#                                    Stop_Order=Inf,
#                                    Profit_Order=Inf,
#                                    Maximum_Elapsed_Time=Inf,
#                                    Commission=0.62))
# Add_OrderRule(Strategy="Long_Strategy",
#               OrderRule="Long",
#               OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
#                                                   Quantity=1,
#                                                   Min_Sig_N=2),
#                                    SellToClose=list(OrderType="MKT",
#                                                     Quantity=1,
#                                                     Min_Sig_N=1)))
# # Add_OrderRule(Strategy="Long_Strategy",
# #               OrderRule="Short",
# #               OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
# #                                                    Quantity=1,
# #                                                    Min_Sig_N=3), # minimum number of positive signals from models to transmit
# #                                    BuyToClose=list(OrderType="MKT",
# #                                                    Quantity=1,
# #                                                    Min_Sig_N=5))) # minimum number of positive signals from models to transmit
# 
# 
# 
# 
# #*******************
# #
# # Short_Strategy ----
# #
# #*******************
# # initiate a strategy called "Short_Strategy"
# Init_Strategy(Name="Short_Strategy",
#               Max_Rows=50) # the maximum number of rows in a temp dataset to parse
# 
# 
# #**************
# # add indicator
# #**************
# Add_Indicator(Strategy="Short_Strategy",
#               Indicator="BBands",
#               IndicatorParams=list(n=20,
#                                    sd=2)) # default n=20, sd=2
# 
# Add_Indicator(Strategy="Short_Strategy",
#               Indicator="RSI",
#               IndicatorParams=list(n=9))
# 
# Add_Indicator(Strategy="Short_Strategy",
#               Indicator="Close")
# 
# 
# #********************************************************************************************
# # add model (to run in combination with other included models to decide to transmit an order)
# #********************************************************************************************
# Add_Model(Strategy="Short_Strategy",
#           Model="Simple_BBands",
#           Model_Name="Simple_BBands_1",
#           ModelParams=list(Long_Consec_Times=4,
#                            Short_Consec_Times=4,
#                            Long_PctB=Params$Simple_BBands_1_Long_PctB[i],
#                            Short_PctB=Params$Simple_BBands_2_Short_PctB[i],
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="Short_Strategy",
#           Model="Simple_BBands",
#           Model_Name="Simple_BBands_2",
#           ModelParams=list(Long_Consec_Times=4,
#                            Short_Consec_Times=4,
#                            Long_PctB=-Inf,
#                            Short_PctB=Params$Simple_BBands_2_Short_PctB[i],
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="Short_Strategy",
#           Model="Simple_RSI",
#           Model_Name="Simple_RSI_1",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_RSI=Params$Simple_BBands_1_Long_PctB[i]*100,
#                            Short_RSI=Inf,
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="Short_Strategy",
#           Model="Simple_RSI",
#           Model_Name="Simple_RSI_2",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_RSI=-Inf,
#                            Short_RSI=Params$Simple_BBands_2_Short_PctB[i]*100,
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# # Add_Model(Strategy="Short_Strategy",
# #           Model="Trend",
# #           ModelParams=list(Interval=5,
# #                            Extent=5,
# #                            Live_Trading=Live_Trading))
# 
# # Short_Strategy$Indicators$BBands
# # Short_Strategy$Models$Simple_BBands
# # Short_Strategy$Order_Rules$SellToClose
# #***************
# # add order rule
# #***************
# Add_OrderRule(Strategy="Short_Strategy",
#               OrderRule="General",
#               OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
#                                    Scenario="Negative", # Positive : early profit is prioritized over loss cut
#                                    Stop_Order=Inf,
#                                    Profit_Order=Inf,
#                                    Maximum_Elapsed_Time=Inf,
#                                    Commission=0.62))
# # Add_OrderRule(Strategy="Short_Strategy",
# #               OrderRule="Long",
# #               OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
# #                                                   Quantity=1,
# #                                                   Min_Sig_N=3),
# #                                    SellToClose=list(OrderType="MKT",
# #                                                     Quantity=1,
# #                                                     Min_Sig_N=5)))
# Add_OrderRule(Strategy="Short_Strategy",
#               OrderRule="Short",
#               OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
#                                                    Quantity=1,
#                                                    Min_Sig_N=2), # minimum number of positive signals from models to transmit
#                                    BuyToClose=list(OrderType="MKT",
#                                                    Quantity=1,
#                                                    Min_Sig_N=1))) # minimum number of positive signals from models to transmit
# 
# 
# 
# 
# #*******************
# #
# # Live_Strategy ----
# #
# #*******************
# # initiate a strategy called "Live_Strategy"
# Init_Strategy(Name="Live_Strategy",
#               Max_Rows=50) # the maximum number of rows in a temp dataset to parse
# 
# 
# #**************
# # add indicator
# #**************
# # Add_Indicator(Strategy="Live_Strategy",
# #               Indicator="BBands",
# #               IndicatorParams=list(n=20,
# #                                    sd=2)) # default n=20, sd=2
# 
# Add_Indicator(Strategy="Live_Strategy",
#               Indicator="RSI",
#               IndicatorParams=list(n=9))
# 
# Add_Indicator(Strategy="Live_Strategy",
#               Indicator="Close")
# 
# 
# #********************************************************************************************
# # add model (to run in combination with other included models to decide to transmit an order)
# #********************************************************************************************
# # Add_Model(Strategy="Live_Strategy",
# #           Model="Simple_BBands_1",
# #           ModelParams=list(Long_Consec_Times=1,
# #                            Short_Consec_Times=1,
# #                            Long_PctB=0.4,
# #                            Short_PctB=Inf))
# # Add_Model(Strategy="Live_Strategy",
# #           Model="Simple_BBands_2",
# #           ModelParams=list(Long_Consec_Times=1,
# #                            Short_Consec_Times=1,
# #                            Long_PctB=-Inf,
# #                            Short_PctB=0.6))
# Add_Model(Strategy="Live_Strategy",
#           Model="Simple_RSI",
#           Model_Name="Simple_RSI_1",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_RSI=0.30*100,
#                            Short_RSI=0.70*100,
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="Live_Strategy",
#           Model="Simple_RSI",
#           Model_Name="Simple_RSI_2",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_RSI=0.40*100,
#                            Short_RSI=0.60*100,
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# # Add_Model(Strategy="Live_Strategy",
# #           Model="Trend",
# #           ModelParams=list(Interval=5,
# #                            Extent=1))
# 
# # Live_Strategy$Indicators$BBands
# # Live_Strategy$Models$Simple_BBands
# # Live_Strategy$Order_Rules$SellToClose
# #***************
# # add order rule
# #***************
# Add_OrderRule(Strategy="Live_Strategy",
#               OrderRule="General",
#               OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
#                                    Scenario="Negative", # Positive : early profit is prioritized over loss cut
#                                    Stop_Order=Inf,
#                                    Profit_Order=Inf,
#                                    Maximum_Elapsed_Time=Inf,
#                                    Commission=0.62))
# Add_OrderRule(Strategy="Live_Strategy",
#               OrderRule="Long",
#               OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
#                                                   Quantity=1,
#                                                   Min_Sig_N=2),
#                                    SellToClose=list(OrderType="MKT",
#                                                     Quantity=1,
#                                                     Min_Sig_N=1)))
# Add_OrderRule(Strategy="Live_Strategy",
#               OrderRule="Short",
#               OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
#                                                    Quantity=1,
#                                                    Min_Sig_N=2), # minimum number of positive signals from models to transmit
#                                    BuyToClose=list(OrderType="MKT",
#                                                    Quantity=1,
#                                                    Min_Sig_N=1))) # minimum number of positive signals from models to transmit
# 
# 
# #********************************
# #
# # RSI_Averages_Band_Strategy ----
# #
# #********************************
# # initiate a strategy called "RSI_Averages_Band_Strategy"
# Init_Strategy(Name="RSI_Averages_Band_Strategy",
#               Max_Rows=50) # the maximum number of rows in a temp dataset to parse
# 
# 
# #**************
# # add indicator
# #**************
# # Add_Indicator(Strategy="RSI_Averages_Band_Strategy",
# #               Indicator="BBands",
# #               IndicatorParams=list(n=20,
# #                                    sd=2)) # default n=20, sd=2
# 
# Add_Indicator(Strategy="RSI_Averages_Band_Strategy",
#               Indicator="RSI",
#               IndicatorParams=list(n=10))
# 
# Add_Indicator(Strategy="RSI_Averages_Band_Strategy",
#               Indicator="Close")
# 
# 
# #********************************************************************************************
# # add model (to run in combination with other included models to decide to transmit an order)
# #********************************************************************************************
# Add_Model(Strategy="RSI_Averages_Band_Strategy",
#           Model="RSI_Averages_Band",
#           Model_Name="RSI_Averages_Band_1",
#           ModelParams=list(MA_Length=16,
#                            RSI_RSI_MA_Diff_Min=Params$RSI_RSI_MA_Diff_Min[i],
#                            RSI_RSI_MA_Diff_Max=Params$RSI_RSI_MA_Diff_Max[i],
#                            Early_Execution_Gap=Params$Early_Execution_Gap[i],
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="RSI_Averages_Band_Strategy",
#           Model="RSI_Averages_Band",
#           Model_Name="RSI_Averages_Band_2",
#           ModelParams=list(MA_Length=24,
#                            RSI_RSI_MA_Diff_Min=0,
#                            RSI_RSI_MA_Diff_Max=Inf,
#                            Early_Execution_Gap=Inf,
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="RSI_Averages_Band_Strategy",
#           Model="RSI_Averages_Band",
#           Model_Name="RSI_Averages_Band_3",
#           ModelParams=list(MA_Length=32,
#                            RSI_RSI_MA_Diff_Min=0,
#                            RSI_RSI_MA_Diff_Max=Inf,
#                            Early_Execution_Gap=Inf,
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="RSI_Averages_Band_Strategy",
#           Model="RSI_Averages_Band",
#           Model_Name="RSI_Averages_Band_4",
#           ModelParams=list(MA_Length=40,
#                            RSI_RSI_MA_Diff_Min=0,
#                            RSI_RSI_MA_Diff_Max=Inf,
#                            Early_Execution_Gap=Inf,
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# Add_Model(Strategy="RSI_Averages_Band_Strategy",
#           Model="RSI_Averages_Band",
#           Model_Name="RSI_Averages_Band_5",
#           ModelParams=list(MA_Length=48,
#                            RSI_RSI_MA_Diff_Min=Params$RSI_RSI_MA_Diff_Min[i],
#                            RSI_RSI_MA_Diff_Max=Params$RSI_RSI_MA_Diff_Max[i],
#                            Early_Execution_Gap=Params$Early_Execution_Gap[i],
#                            Live_Trading=Live_Trading,
#                            Reverse=Params$Reverse[i]))
# 
# 
# 
# #***************
# # add order rule
# #***************
# Add_OrderRule(Strategy="RSI_Averages_Band_Strategy",
#               OrderRule="General",
#               OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
#                                    Scenario="Negative", # Positive : early profit is prioritized over loss cut
#                                    Stop_Order=Inf,
#                                    Profit_Order=Inf,
#                                    Maximum_Elapsed_Time=Inf,
#                                    Commission=0.62,
#                                    Tick_Size=0.25, # the minimum tick size
#                                    Penalty=1,# the number of ticks away from the spot price when MKT is filled
#                                    Tick_Value=0.5, # value per tick
#                                    Market_Time=Params$Market_Time[i]))
# Add_OrderRule(Strategy="RSI_Averages_Band_Strategy",
#               OrderRule="Long",
#               OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
#                                                   Quantity=1,
#                                                   Min_Sig_N=Params$Open_N[i]),
#                                    SellToClose=list(OrderType="MKT",
#                                                     Quantity=1,
#                                                     Min_Sig_N=Params$Close_N[i])))
# Add_OrderRule(Strategy="RSI_Averages_Band_Strategy",
#               OrderRule="Short",
#               OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
#                                                    Quantity=1,
#                                                    Min_Sig_N=Params$Open_N[i]), # minimum number of positive signals from models to transmit
#                                    BuyToClose=list(OrderType="MKT",
#                                                    Quantity=1,
#                                                    Min_Sig_N=Params$Close_N[i]))) # minimum number of positive signals from models to transmit
