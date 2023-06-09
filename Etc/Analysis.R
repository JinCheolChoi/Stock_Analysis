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
              "lubridate",
              "DescTools")){ # candle chart
  lapply(pack, checkpackages)
}

#************
#
# import data
#
#************
# 5 seconds
Get_Data(Symbols=list("MNQ"),
         Data_Dir=data.dir,
         BarSize=5,
         Convert_Tz=T,
         Filter=T)
BarData=MNQ
BarData[, RSI:=RSI(Close)]
BarData[, Close:=Close]
BarData[, Shift_Close:=shift(Close, -1)]
BarData[, Time:=Time]
BarData[, Change:=Shift_Close-Close]
BarData[Change>0, Change_Sign:=1]
BarData[Change==0, Change_Sign:=0]
BarData[Change<0, Change_Sign:=-1]


#
BarData[, Time_5:=floor_date(Time, unit="5 minutes")]
BarData[, Time_15:=floor_date(Time, unit="15 minutes")]

#**********
# 5 minutes
Get_Data(Symbols=list("MNQ"),
         Data_Dir=data.dir,
         BarSize=60*5,
         Convert_Tz=T,
         Filter=T)
BarData_5=MNQ
BarData_5[, RSI_5:=RSI(Close)]
BarData_5[, Close_5:=Close]
BarData_5[, Shift_Close_5:=shift(Close_5, -1)]
BarData_5[, Time_5:=Time]
BarData_5[, Change_5:=Shift_Close_5-Close_5]
BarData_5[Change_5>0, Change_5_Sign:=1]
BarData_5[Change_5==0, Change_5_Sign:=0]
BarData_5[Change_5<0, Change_5_Sign:=-1]

#***********
# 15 minutes
Get_Data(Symbols=list("MNQ"),
         Data_Dir=data.dir,
         BarSize=60*15,
         Convert_Tz=T,
         Filter=T)
BarData_15=MNQ
BarData_15[, RSI_15:=RSI(Close)]
BarData_15[, Close_15:=Close]
BarData_15[, Shift_Close_15:=shift(Close_15, -1)]
BarData_15[, Time_15:=Time]
BarData_15[, Change_15:=Shift_Close_15-Close_15]
BarData_15[Change_15>0, Change_15_Sign:=1]
BarData_15[Change_15==0, Change_15_Sign:=0]
BarData_15[Change_15<0, Change_15_Sign:=-1]

#
BarData_5_combined=BarData_5[, .SD, .SDcols=c("Time_5", "RSI_5", "Change_5", "Change_5_Sign")][BarData, on=c("Time_5")]
BarData_15_combined=BarData_15[, .SD, .SDcols=c("Time_15", "RSI_15", "Change_15", "Change_15_Sign")][BarData_5_combined, on=c("Time_15")]

#
setcolorder(BarData_15_combined, c("Symbol", "Time", "Open", "High", "Low", "Close", "Volume", "Count", "Net_Volume", "RSI", "RSI_5", "RSI_15", "Change", "Change_5", "Change_15", "Change_Sign", "Change_5_Sign", "Change_15_Sign"))

#
plot(unique(na.omit(BarData_15_combined[, c("RSI_5", "Change_15")])))
cor(unique(na.omit(BarData_15_combined[, c("RSI_5", "Change_15")])))

summary(lm(Change_15~RSI+RSI_5+RSI_15, BarData_15_combined))

#
unique(na.omit(BarData_15_combined[, c("RSI_15", "RSI_5")])) %>% plot

#
unique(na.omit(BarData_15_combined[RSI_15>=39.5&
                                     RSI_15<=40.5, c("RSI_5")])) %>% summary


BarData_5_combined=unique(BarData_15_combined[, .SD, .SDcols=c("Time_5", "RSI_5", "RSI_15", "Change_5", "Change_5_Sign")])



BarData_15_combined[RSI_15>=39.5&
                     RSI_15<=40.5&
                     RSI_5<=37, Change_5_Sign] %>% table
BarData_15_combined[RSI_15>=39.5&
                     RSI_15<=40.5&
                     RSI_5<=37, Change_5] %>% summary




