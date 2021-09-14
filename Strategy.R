#************************
#
# Required functions ---- 
#
#***********************************************************
source(paste0(working.dir, "0. Models.R"))
source(paste0(working.dir, "0. OrderRules.R"))



#****************
#
# Strategy_1 ---- 
#
#*******************
# initiate a strategy called "Strategy_1"
Init_Strategy(Name="Strategy_1",
              Max_Rows=50) # the maximum number of rows in a temp dataset to parse


#**************
# add indicator
#**************
Add_Indicator(Strategy="Strategy_1",
              Indicator="BBands",
              IndicatorParams=list(n=20,
                                   sd=2)) # default n=20, sd=2

Add_Indicator(Strategy="Strategy_1",
              Indicator="RSI",
              IndicatorParams=list(n=9))

Add_Indicator(Strategy="Strategy_1",
              Indicator="Close")


#********************************************************************************************
# add model (to run in combination with other included models to decide to transmit an order)
#********************************************************************************************
Add_Model(Strategy="Strategy_1",
          Model="Simple_BBands_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=0.6,
                           Short_PctB=Inf))
Add_Model(Strategy="Strategy_1",
          Model="Simple_BBands_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_PctB=-Inf,
                           Short_PctB=0.4))
Add_Model(Strategy="Strategy_1",
          Model="Simple_RSI_1",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=60,
                           Short_RSI=Inf))
Add_Model(Strategy="Strategy_1",
          Model="Simple_RSI_2",
          ModelParams=list(Long_Consec_Times=1,
                           Short_Consec_Times=1,
                           Long_RSI=-Inf,
                           Short_RSI=40))
Add_Model(Strategy="Strategy_1",
          Model="Trend",
          ModelParams=list(Interval=5,
                           Extent=1))

# Strategy_1$Indicators$BBands
# Strategy_1$Models$Simple_BBands
# Strategy_1$Order_Rules$SellToClose
#***************
# add order rule
#***************
Add_OrderRule(Strategy="Strategy_1",
              OrderRule="General",
              OrderRuleParams=list(Max_Orders=1, # the maximum number of orders to hold to average dollar cost (not optimized yet except for 1)
                                   Scenario="Negative", # Positive : early profit is prioritized over loss cut
                                   Stop_Order=20,
                                   Profit_Order=10,
                                   Trend=TRUE))
Add_OrderRule(Strategy="Strategy_1",
              OrderRule="Long",
              OrderRuleParams=list(BuyToOpen=list(OrderType="LMT",
                                                  Quantity=1,
                                                  Min_Sig_N=1),
                                   SellToClose=list(OrderType="LMT",
                                                    Quantity=1,
                                                    Min_Sig_N=5)))
Add_OrderRule(Strategy="Strategy_1",
              OrderRule="Short",
              OrderRuleParams=list(SellToOpen=list(OrderType="LMT",
                                                   Quantity=1,
                                                   Min_Sig_N=1), # minimum number of positive signals from models to transmit
                                   BuyToClose=list(OrderType="LMT",
                                                   Quantity=1,
                                                   Min_Sig_N=5))) # minimum number of positive signals from models to transmit







#*****************************
# parameters from the strategy
#*****************************
Max_Rows=Strategy_1[["Max_Rows"]]
Order_Rules=Strategy_1[["Order_Rules"]]
Indicators=Strategy_1[["Indicators"]]
Models=Strategy_1[["Models"]]

Max_Orders=as.numeric(Order_Rules[["General"]][["Max_Orders"]])
Scenario=Order_Rules[["General"]][["Scenario"]]
Trend=Order_Rules[["General"]][["Trend"]]
Stop_Order=as.numeric(Order_Rules[["General"]][["Stop_Order"]])
Profit_Order=as.numeric(Order_Rules[["General"]][["Profit_Order"]])
Strategy_Indicators=names(Indicators)
Strategy_Models=names(Models)

Strategy_Rules=names(Order_Rules)[names(Order_Rules)!="General"]


Live_Data_Temp=data.table(Symbol="",
                          Time=Sys.time(),
                          Open=0,
                          High=0,
                          Low=0,
                          Close=0,
                          Volume=0,
                          Count=0)
Live_Data_Temp[, `:=`(Symbol=NULL, Time=NULL, Open=NULL,
                      High=NULL, Low=NULL, Close=NULL,
                      Volume=NULL, Count=NULL)]

Orders_Transmitted=data.table(Symbol="",
                              Submit_Time=Sys.time(),
                              Filled_Time=Sys.time(),
                              Action="",
                              Detail="",
                              TotalQuantity=0,
                              OrderType="MKT",
                              LmtPrice=0,
                              Filled=0,
                              Sigs_N=0)
Orders_Transmitted=Orders_Transmitted[-1,]

#
Old_Positions=0
Transmitted=0




