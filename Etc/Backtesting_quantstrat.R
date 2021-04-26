#********************
#
# empty the workspace
#
#********************
rm(list=ls())


#***********
#
# parameters
#
#***********
working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/" # desktop
#working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/", # laptop
Symbol="MNQ"
First_Date="2021-01-20"
Last_Date=as.Date(format(Sys.time(), tz="PST8PDT"))
BarSize=60*30 # secs (30 mins bar size seems to need a touch up in the code)

# account
# margin account="U4524665"
# paper trading account="DU2656942"
Account_Code="DU2656942"

# port
Port=7497 # tws : 7497, IB gateway : 4002


#*****************
#
# preliminary step
#
# required functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))
source(paste0(working.dir, "/Echos/Echo_Daily_Hist_Data_Save.R"))

# import packages
for(Package in c("IBrokers",
                 "TTR",
                 "data.table",
                 "dplyr",
                 "PerformanceAnalytics",
                 "quantmod",
                 "DescTools", # candle chart
                 
                 "quantstrat",
                 "DT",
                 "ggplot2",
                 "htmltools",
                 "htmlwidgets",
                 "knitr",
                 "lattice",
                 "pander",
                 "tidyr",
                 "webshot"
)){
  checkpackages(Package)
}


#************
# import data
#************
# collapse data to the chosen-sized bar data
Collapsed_BarData.Original=Get_Data(Symbol,
                                    BarSize,
                                    First_Date, 
                                    Last_Date)

Collapsed_BarData=Collapsed_BarData.Original[, -1] %>% as.xts.data.table()
MNQ=Collapsed_BarData

#***********************
# quantstrat backtesting
#***********************
# https://timtrice.github.io/backtesting-strategies/index.html
currency('USD')
#



# #
# basic_symbols <- function() {
#   symbols <- c(
#     "IWM", # iShares Russell 2000 Index ETF
#     "QQQ", # PowerShares QQQ TRust, Series 1 ETF
#     "SPY" # SPDR S&P 500 ETF Trust
#   )
# }
# 
# symbols <- basic_symbols()
# 
# init_date <- "2007-12-31"
# start_date <- "2008-01-01"
# end_date <- "2009-12-31"
# init_equity <- 1e4 # $10,000
# adjustment <- TRUE
# 
# 
# getSymbols(Symbols = symbols,
#            src = "yahoo",
#            index.class = "POSIXct",
#            from = start_date,
#            to = end_date,
#            adjust = adjustment)
# 
# 
# IWM=as.data.table(IWM)
# QQQ=as.data.table(QQQ)
# SPY=as.data.table(SPY)
# 
# # dates
# Dates=seq(Sys.Date(), # minimum Date
#           Sys.Date(), # maximum Date
#           by="day")
# 
# # time intervals
# Times=as.ITime(seq(as.POSIXct(paste0(as.Date(format(Sys.time()))-1, " 00:00:00")),
#                    as.POSIXct(paste0(as.Date(format(Sys.time())), " 00:00:00"))-60,
#                    by=60))
# 
# # Date_Time_From
# Time_Intervals=data.table(
#   Date_Time_From=as.POSIXct(strptime(paste(rep(Dates, each = length(Times)), Times, sep = " "),
#                                      "%Y-%m-%d %H:%M:%S"))
# )
# IWM[, index:=Time_Intervals$Date_Time_From[1:nrow(IWM)]]
# QQQ[, index:=Time_Intervals$Date_Time_From[1:nrow(QQQ)]]
# SPY[, index:=Time_Intervals$Date_Time_From[1:nrow(SPY)]]
# 
# IWM=as.xts.data.table(IWM)
# QQQ=as.xts.data.table(QQQ)
# SPY=as.xts.data.table(SPY)

symbols="MNQ"
init_date <- Collapsed_BarData.Original$Time[1]
start_date <- Collapsed_BarData.Original$Time[100]
end_date <- tail(Collapsed_BarData.Original$Time, 1)
init_equity <- 1e4 # $10,000
adjustment <- TRUE

stock(symbols,
      currency = "USD",
      multiplier = 1)

portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)


initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)

initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)


strategy(strategy.st, store = TRUE)


#***************
# add indicators
#***************
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), 
                               n = 10),
              label = "nFast")

add.indicator(strategy = strategy.st, 
              name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), 
                               n = 30), 
              label = "nSlow")


#************
# add signals
#************
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")


#**********
# add rules
#**********
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = 100,
                          ordertype = "stoplimit",
                          orderside = "long", 
                          threshold = 0.0005,
                          prefer = "High", 
                          TxnFees = -10, 
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -100,
                          ordertype = "stoplimit",
                          threshold = -0.005, 
                          orderside = "short", 
                          replace = FALSE, 
                          TxnFees = -10, 
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")

add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "short", 
                          sigval = TRUE, 
                          orderside = "long", 
                          ordertype = "market", 
                          orderqty = "all", 
                          TxnFees = -10, 
                          replace = TRUE), 
         type = "exit", 
         label = "Exit2SHORT")

add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "long", 
                          sigval = TRUE, 
                          orderside = "short", 
                          ordertype = "market", 
                          orderqty = "all", 
                          TxnFees = -10, 
                          replace = TRUE), 
         type = "exit", 
         label = "Exit2LONG")



# results_file <- paste("C:/Users/JinCheol Choi/Desktop/Temp/results", strategy.st, "RData", sep = ".")
# if( file.exists(results_file) ) {
#   load(results_file)
# } else {
#   results <- applyStrategy(strategy.st, portfolios = portfolio.st)
#   updatePortf(portfolio.st)
#   updateAcct(account.st)
#   updateEndEq(account.st)
#   if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
#     save(list = "results", file = results_file)
#     save.strategy(strategy.st)
#   }
# }


# getInstrument("MNQ")


# Examining Trades
# rm.strat(portfolio.st)
# rm.strat(account.st)
# symbols <- basic_symbols()
# getSymbols(Symbols = "SPY", src = "yahoo", index.class = "POSIXct", 
#            from = start_date, to = end_date, adjust = adjustment)
# initPortf(name = portfolio.st, symbols = symbols, initDate = init_date)
# initAcct(name = account.st, portfolios = portfolio.st, initDate = init_date, 
#          initEq = init_equity)
# initOrders(portfolio = portfolio.st, symbols = symbols, initDate = init_date)
applyStrategy(strategy.st, portfolios = portfolio.st)
checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)


chart.Posn(portfolio.st, Symbol = "MNQ", 
           TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")


#
ls(envir = .blotter)
ls(envir = .strategy)
MNQ_account=get("account.Acct.Luxor", envir = .blotter)
MNQ_portfolio=get("portfolio.Port.Luxor", envir = .blotter)
MNQ_Strategies=get("Strat.Luxor", envir = .strategy)

MNQ_account$summary



