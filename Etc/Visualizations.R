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
Symbols="MNQ"
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
# collapse data to the chosen-sized bar data
Get_Data(Symbols,
         BarSize,
         First_Date, 
         Last_Date)

# convert xts.Collapsed_BarData
xts.Collapsed_BarData=as.xts.data.table(MNQ[, -1])

# create a chart
system.time({
  chartSeries(xts.Collapsed_BarData,
              name=Symbols,
              theme="white",
              TA=c("addMACD()"))
})

# performance summary chart
charts.PerformanceSummary(ROC(xts.Collapsed_BarData$Close))



