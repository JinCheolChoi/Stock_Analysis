# parameter
Max_Orders=3
Scenario
Reverse
# Stop_Order
# Profit_Order
# Maximum_Elapsed_Time

microbenchmark::microbenchmark(Order_Filled_C(Which_Signals, Max_Orders),
                               Order_Filled_R(Which_Signals, Max_Orders))


Order_Filled_Results_C=Order_Filled_C(Which_Signals, Max_Orders)
Order_Filled_Results_R=Order_Filled_R(Which_Signals, Max_Orders)


Which_Signals_C=copy(Which_Signals)
Which_Signals_R=copy(Which_Signals)

Which_Signals_C[, Quantity:=Order_Filled_Results_C$Quantity]
Which_Signals_C[, Net_Quantity:=Order_Filled_Results_C$Net_Quantity]
Which_Signals_C[, Remove:=Order_Filled_Results_C$Remove]

Which_Signals_R[, Quantity:=Order_Filled_Results_R$Quantity]
Which_Signals_R[, Net_Quantity:=Order_Filled_Results_R$Net_Quantity]
# Which_Signals_R[, Remove:=Order_Filled_Results_R$Remove]

Which_Signals_C
Order_Filled_Results_R


identical(Which_Signals_C[Remove==0, ],
          Which_Signals_R[Remove==0, ])
identical(Which_Signals_C$Ind,
          Which_Signals_R$Ind)
identical(Which_Signals_C$Signals,
          Which_Signals_R$Signals)
identical(Which_Signals_C$Action,
          Which_Signals_R$Action)
identical(Which_Signals_C$Detail,
          Which_Signals_R$Detail)

identical(Which_Signals_C$Quantity,
          Which_Signals_R$Quantity)
sum(Which_Signals_C$Quantity==Which_Signals_R$Quantity)

identical(Which_Signals_C$Both_Direction,
          Which_Signals_R$Both_Direction)

identical(Which_Signals_C$Net_Quantity,
          Which_Signals_R$Net_Quantity)
sum(Which_Signals_C$Net_Quantity==Which_Signals_R$Net_Quantity)

identical(Which_Signals_C$Remove,
          Which_Signals_R$Remove)
sum(Which_Signals_C$Remove==Which_Signals_R$Remove)






