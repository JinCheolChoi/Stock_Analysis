#****************************
#
# Strategy_Simple_BBands ---- 
#
#****************************
# initiate a strategy called "Strategy_Simple_BBands"
Init_Strategy(Name="Strategy_Simple_BBands",
              Max_Rows=50) # the maximum number of rows in a temp dataset to parse


#***************
# add order rule
#***************
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="General",
              OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
                                   Position_Direction="both")) # direction of position ("both", "long", "short")
                                   
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="BuyToOpen",
              OrderRuleParams=list(OrderType="MKT"))
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="BuyToClose",
              OrderRuleParams=list(OrderType="MKT"))
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="SellToOpen",
              OrderRuleParams=list(OrderType="MKT"))
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="SellToClose",
              OrderRuleParams=list(OrderType="MKT"))


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
          Model="Simple_BBands",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=0,
                           Short_PctB=1))

# Add_Model(Strategy="Strategy_Simple_BBands",
#           Model="Simple_RSI",
#           ModelParams=list(Long_Consec_Times=1,
#                            Short_Consec_Times=1,
#                            Long_PctB=0.2,
#                            Short_PctB=0.8))

Add_Model(Strategy="Strategy_Simple_BBands",
          Model="Simple_Test",
          ModelParams=list(x=0.2,
                           y=0.8))

# Strategy_Simple_BBands$Indicators$BBands
# Strategy_Simple_BBands$Models$Simple_BBands

