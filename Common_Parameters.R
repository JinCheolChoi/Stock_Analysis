Strategy=get(paste0(Strategy_Name))

Max_Rows=Strategy[["Max_Rows"]]
Order_Rules=Strategy[["Order_Rules"]]
Indicators=Strategy[["Indicators"]]
Models=Strategy[["Models"]]

Max_Orders=as.numeric(Order_Rules[["General"]][["Max_Orders"]])
Scenario=Order_Rules[["General"]][["Scenario"]]
Stop_Order=as.numeric(Order_Rules[["General"]][["Stop_Order"]])
Profit_Order=as.numeric(Order_Rules[["General"]][["Profit_Order"]])
Strategy_Indicators=names(Indicators)
Strategy_Models=names(Models)
Strategy_Models_Class=unlist(lapply(Models, class))
General_Strategy="General"

# Commission (US$) per transition
Commission=as.numeric(Order_Rules[["General"]][["Commission"]])

# the minimum tick size
Tick_Size=as.numeric(Order_Rules[["General"]][["Tick_Size"]])

# the number of ticks away from the spot price when MKT is filled
Penalty=as.numeric(Order_Rules[["General"]][["Penalty"]])

# value per tick
Tick_Value=as.numeric(Order_Rules[["General"]][["Tick_Value"]])

# Market_Time
Market_Time=as.numeric(Order_Rules[["General"]][["Market_Time"]])

#******************
# preliminary steps
#******************
# if(Position_Direction=="both"){
#   Max_Long_Orders=Max_Short_Orders=Max_Orders-1
# }else if(Position_Direction=="long"){
#   Max_Long_Orders=Max_Orders-1
#   Max_Short_Orders=-1
# }else if(Position_Direction=="short"){
#   Max_Long_Orders=-1
#   Max_Short_Orders=Max_Orders-1
# }
Position_Names=names(Order_Rules)[names(Order_Rules)!=General_Strategy]

# remove Strategy
rm(Strategy)