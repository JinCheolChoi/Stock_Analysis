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
# working directory
working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/" # desktop
#working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/" # laptop


#*****************
#
# preliminary step
#
#*******************
# required functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))
source(paste0(working.dir, "/Echos/Echo_Stop_Live_Trading.R"))


#*****************
#
# preliminary step
#
#*********************
# import packages
for(Package in
    c("data.table",
      "taskscheduleR")){
  checkpackages(Package)
}


#***************
#
# stop schedules
#
#******************
# add a part to clear all positions possessed


# stop live trading
taskscheduler_stop("Run_Live_Trading")

# a break during periods of market close time
Rerun_Live_Trading=System_Break(Log=T)


#*****************
#
# re-run schedules
#
#*****************
if(Rerun_Live_Trading==1){
  taskscheduler_runnow("Run_Live_Trading")
}


#
Log=fread(paste0(working.dir, "Stop_Live_Trading_Log.csv"))
Log[, Time:=as.POSIXct(format(as.POSIXct(Time),
                              tz="PST8PDT"),
                       tz="PST8PDT")]


