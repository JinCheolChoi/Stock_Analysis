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




#*********************
#
#*********************
#
currency('USD')

init_date <- "2007-12-31"
start_date <- "2008-01-01"
end_date <- "2009-12-31"
init_equity <- 1e4 # $10,000
adjustment <- TRUE


#
basic_symbols <- function() {
  symbols <- c(
    "IWM", # iShares Russell 2000 Index ETF
    "QQQ", # PowerShares QQQ TRust, Series 1 ETF
    "SPY" # SPDR S&P 500 ETF Trust
  )
}


#
enhanced_symbols <- function() {
  symbols <- c(
    basic_symbols(), 
    "TLT", # iShares Barclays 20+ Yr Treas. Bond ETF
    "XLB", # Materials Select Sector SPDR ETF
    "XLE", # Energy Select Sector SPDR ETF
    "XLF", # Financial Select Sector SPDR ETF
    "XLI", # Industrials Select Sector SPDR ETF
    "XLK", # Technology  Select Sector SPDR ETF
    "XLP", # Consumer Staples  Select Sector SPDR ETF
    "XLU", # Utilities  Select Sector SPDR ETF
    "XLV", # Health Care  Select Sector SPDR ETF
    "XLY" # Consumer Discretionary  Select Sector SPDR ETF
  )
}


#
global_symbols <- function() {
  symbols <- c(
    enhanced_symbols(), 
    "EFA", # iShares EAFE
    "EPP", # iShares Pacific Ex Japan
    "EWA", # iShares Australia
    "EWC", # iShares Canada
    "EWG", # iShares Germany
    "EWH", # iShares Hong Kong
    "EWJ", # iShares Japan
    "EWS", # iShares Singapore
    "EWT", # iShares Taiwan
    "EWU", # iShares UK
    "EWY", # iShares South Korea
    "EWZ", # iShares Brazil
    "EZU", # iShares MSCI EMU ETF
    "IGE", # iShares North American Natural Resources
    "IYR", # iShares U.S. Real Estate
    "IYZ", # iShares U.S. Telecom
    "LQD", # iShares Investment Grade Corporate Bonds
    "SHY" # iShares 42372 year TBonds
  )
}


#
checkBlotterUpdate <- function(port.st = portfolio.st, 
                               account.st = account.st, 
                               verbose = TRUE) {
  
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(
    sapply(
      syms, 
      FUN = function(x) eval(
        parse(
          text = paste("sum(p$symbols", 
                       x, 
                       "posPL.USD$Net.Trading.PL)", 
                       sep = "$")))))
  
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  
  if(!isTRUE(all.equal(port.tot, port.sum.tot))) {
    ok <- FALSE
    if(verbose) print("portfolio P&L doesn't match sum of symbols P&L")
  }
  
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  
  if(!isTRUE(all.equal(port.tot, endEq - initEq)) ) {
    ok <- FALSE
    if(verbose) print("portfolio P&L doesn't match account P&L")
  }
  
  if(sum(duplicated(index(p$summary)))) {
    ok <- FALSE
    if(verbose)print("duplicate timestamps in portfolio summary")
    
  }
  
  if(sum(duplicated(index(a$summary)))) {
    ok <- FALSE
    if(verbose) print("duplicate timestamps in account summary")
  }
  return(ok)
}


#
symbols <- basic_symbols()

getSymbols(Symbols = symbols, 
           src = "yahoo", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)

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


#**********
# add rules
#**********
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



results_file <- paste("C:/Users/JinCheol Choi/Desktop/Temp/results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  load(results_file)
} else {
  results <- applyStrategy(strategy.st, portfolios = portfolio.st)
  updatePortf(portfolio.st)
  updateAcct(account.st)
  updateEndEq(account.st)
  if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save(list = "results", file = results_file)
    save.strategy(strategy.st)
  }
}





# Examining Trades
rm.strat(portfolio.st)
rm.strat(account.st)
symbols <- basic_symbols()
getSymbols(Symbols = symbols, src = "yahoo", index.class = "POSIXct", 
           from = start_date, to = end_date, adjust = adjustment)
initPortf(name = portfolio.st, symbols = symbols, initDate = init_date)
initAcct(name = account.st, portfolios = portfolio.st, initDate = init_date, 
         initEq = init_equity)
initOrders(portfolio = portfolio.st, symbols = symbols, initDate = init_date)
applyStrategy(strategy.st, portfolios = portfolio.st)
checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)


chart.Posn(portfolio.st, Symbol = "SPY", Dates="2008-01-01::2008-07-01", 
           TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")




