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
Last_Date=as.Date(format(Sys.time(), tz="America/Los_Angeles"))
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
                 "DescTools" # candle chart
                 
                 # "quantstrat",
                 # "DT",
                 # "ggplot2",
                 # "htmltools",
                 # "htmlwidgets",
                 # "knitr",
                 # "lattice",
                 # "pander",
                 # "tidyr",
                 # "webshot"
                 )){
  checkpackages(Package)
}


#************
# import data
#************
# output : `5SecsBarHistData`
Import_HistData(Location=paste0(working.dir, "Data/"),
                Symbol=Symbol,
                First_Date=First_Date,
                Last_Date=Last_Date,
                Convert_Tz=T)

# collapse data to the chosen-sized bar data
Collapsed_BarData.Original=Collapse_5SecsBarData(`5SecsBarHistData`,
                                                 BarSize=60*30,
                                                 Convert_Tz=T)

# convert xts.Collapsed_BarData
xts.Collapsed_BarData=as.xts.data.table(Collapsed_BarData.Original[, -1])

# create a chart
system.time({
  chartSeries(xts.Collapsed_BarData,
              name=Symbol,
              theme="white",
              TA=c("addMACD()"))
})

# performance summary chart
charts.PerformanceSummary(ROC(xts.Collapsed_BarData$Close))



symbols <- basic_symbols()
getSymbols(Symbols = symbols, 
           src = "yahoo", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)




basic_symbols()
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long", 
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "exit",
         label = "Exit2LONG")



