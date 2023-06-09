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
#******************
# working directory
#******************
working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/" # desktop
data.dir="E:/Stock_Data/" # upper folder that has a folder storing stock data
# working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/" # laptop
# data.dir="C:/Users/jchoi02/Desktop/Data/" # upper folder that has a folder storing stock data


#****************
# data parameters
#****************
Symbols=c("MNQ")


#*****************
#
# preliminary step
#
#*******************
# load functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))

# import libraries
for(pack in c("IBrokers",
              "TTR",
              "data.table",
              "dplyr",
              "DescTools")){ # candle chart
  lapply(pack, checkpackages)
}

# import data
Get_Data(Symbols=list("MNQ"),
         Data_Dir=data.dir,
         BarSize=5,
         Convert_Tz=T,
         Filter=T)

# 5 secs bar data
BarData_5=MNQ %>% as.data.frame() %>% as.data.table()
MNQ[, Wap:=0]
MNQ_Col_Names=colnames(MNQ)

BarData_5[, RSI:=RSI(Close, n=9)]
colnames(BarData_5)=paste0(c(MNQ_Col_Names[MNQ_Col_Names!="Wap"], "RSI"), "_5")


# 5 mins bar data
BarData_300=Collapse_5SecsBarData(`5SecsBarData`=MNQ,
                                  BarSize=60*5,
                                  Convert_Tz=T)
BarData_300[, RSI:=RSI(Close, n=9)]
colnames(BarData_300)=paste0(c(MNQ_Col_Names[MNQ_Col_Names!="Wap"], "RSI"), "_300")


# 15 mins bar data
BarData_900=Collapse_5SecsBarData(`5SecsBarData`=MNQ,
                                  BarSize=60*15,
                                  Convert_Tz=T)
BarData_900[, RSI:=RSI(Close, n=9)]
colnames(BarData_900)=paste0(c(MNQ_Col_Names[MNQ_Col_Names!="Wap"], "RSI"), "_900")


#*****************************************
# combine bar data of different timeframes
#*****************************************
BarData_300[, Time_300_From:=Time_300]
BarData_300[, Time_300_To:=Time_300_From+300]
BarData_300=BarData_300[BarData_5, on=c("Time_300_From<=Time_5", "Time_300_To>Time_5"),
                        nomatch=0]
BarData_300[, `:=`(Time_5=Time_300_To,
                   Time_300_From=NULL)]



BarData_900[, Time_900_From:=Time_900]
BarData_900[, Time_900_To:=Time_900_From+900]
BarData_900=BarData_900[BarData_300, on=c("Time_900_From<=Time_5", "Time_900_To>Time_5"),
                        nomatch=0]

BarData_900[, `:=`(Time_5=Time_900_To,
                   Time_900_From=NULL)]
plot(unique(BarData_900[, .SD, .SDcols=c("RSI_300", "RSI_900")]),
     xlim=c(-10, 110),
     ylim=c(-10, 110))
plot(unique(BarData_900[Time_5>="2021-03-01"&
                          Time_5<"2021-05-01", .SD, .SDcols=c("RSI_300", "RSI_900")]),
     xlim=c(-10, 110),
     ylim=c(-10, 110))
plot(unique(BarData_900[Time_5>="2021-05-01"&
                          Time_5<"2021-07-01", .SD, .SDcols=c("RSI_300", "RSI_900")]),
     xlim=c(-10, 110),
     ylim=c(-10, 110))



