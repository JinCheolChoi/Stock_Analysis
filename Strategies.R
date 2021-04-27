#****************************
#
# Strategy_Simple_BBands ---- 
#
#****************************
# initiate a strategy called "Strategy_Simple_BBands"
Init_Strategy(Name="Strategy_Simple_BBands") # the maximum number of rows in a temp dataset to parse

#***************
# add order rule
#***************
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="General",
              OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
                                   OrderType="MKT", # "LMT"
                                   Position_Direction="both", # direction of position ("both", "long", "short")
                                   Parsed_Data_Max_Rows=50))
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="BuyToOpen",
              OrderRuleParams=list())
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="BuyToClose",
              OrderRuleParams=list())
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="SellToOpen",
              OrderRuleParams=list())
Add_OrderRule(Strategy="Strategy_Simple_BBands",
              OrderRule="SellToClose",
              OrderRuleParams=list())

#**************
# add indicator
#**************
Add_Indicator(Strategy="Strategy_Simple_BBands",
              Indicator="BBands",
              IndicatorParams=list(BBands_N=20,
                                   BBands_SD=2))

Add_Indicator(Strategy="Strategy_Simple_BBands",
              Indicator="RSI",
              IndicatorParams=list(RSI_N=14))


#********************************************************************************************
# add model (to run in combination with other included models to decide to transmit an order)
#********************************************************************************************
Add_Model(Strategy="Strategy_Simple_BBands",
          Model="Simple_BBands",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=0,
                           Short_PctB=1))
Add_Model(Strategy="Strategy_Simple_BBands",
          Model="Simple_RSI",
          ModelParams=list())








