#************************
#
# Required functions ---- 
#
#***********************************************************
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))
source(paste0(working.dir, "0. Models.R"))
source(paste0(working.dir, "0. OrderRules.R"))


#****************************
#
# Strategy_Simple_BBands ---- 
#
#****************************
# initiate a strategy called "Strategy_Simple_BBands"
Init_Strategy(Name="Strategy_Simple_BBands",
              Max_Rows=50) # the maximum number of rows in a temp dataset to parse


#**************
# add indicator
#**************
Add_Indicator(Strategy="Strategy_Simple_BBands",
              Indicator="BBands",
              IndicatorParams=list(n=20,
                                   sd=2)) # default n=20, sd=2

Add_Indicator(Strategy="Strategy_Simple_BBands",
              Indicator="RSI",
              IndicatorParams=list(n=14))


#********************************************************************************************
# add model (to run in combination with other included models to decide to transmit an order)
#********************************************************************************************
Add_Model(Strategy="Strategy_Simple_BBands",
          Model="Simple_BBands_1",
          ModelParams=list(Long_Consec_Times=2,
                           Short_Consec_Times=1,
                           Long_PctB=0.1,
                           Short_PctB=0.9))
Add_Model(Strategy="Strategy_Simple_BBands",
          Model="Simple_BBands_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=2,
                           Long_PctB=0.1,
                           Short_PctB=0.9))

# Add_Model(Strategy="Strategy_Simple_BBands",
#           Model="Simple_RSI",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_PctB=0.2,
#                            Short_PctB=0.8))

# Add_Model(Strategy="Strategy_Simple_BBands",
#           Model="Simple_Test",
#           ModelParams=list(x=0.2,
#                            y=0.8))

# Strategy_Simple_BBands$Indicators$BBands
# Strategy_Simple_BBands$Models$Simple_BBands
# Strategy_Simple_BBands$Order_Rules$SellToClose


#***************
# add order rule
#***************
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="General",
              OrderRuleParams=list(Max_Orders=1,
                                   Cut_Loss=10,
                                   Profit_Threshold=10)) # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="Long",
              OrderRuleParams=list(BuyToOpen=list(OrderType="MKT",
                                                  Quantity=1,
                                                  Min_Sig_N=2),
                                   SellToClose=list(OrderType="MKT",
                                                    Quantity=1,
                                                    Min_Sig_N=1)))
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="Short",
              OrderRuleParams=list(SellToOpen=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=2), # minimum number of positive signals from models to transmit
                                   BuyToClose=list(OrderType="MKT",
                                                   Quantity=1,
                                                   Min_Sig_N=1))) # minimum number of positive signals from models to transmit



