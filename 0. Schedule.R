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

# import taskscheduleR
lapply(
  c(
    "taskscheduleR"
  ), 
  checkpackages)


#*****************
#
# create schedules
#
#***********************
# !!! important note !!!
#***********************
# manually remove a quote string that creates a log text file in Actions in Task Scheduler as belows
# before removal : /c C:/PROGRA~1/R/R-4.0.5/bin/Rscript.exe "C:/Users/jchoi02/Desktop/R/Stock_Analysis/Stop_Live_Trading.R"  >> "C:/Users/jchoi02/Desktop/R/Stock_Analysis/Stop_Live_Trading.log" 2>&1
# after removal : /c C:/PROGRA~1/R/R-4.0.5/bin/Rscript.exe "C:/Users/jchoi02/Desktop/R/Stock_Analysis/Stop_Live_Trading.R"
#*************************************************************************************************************************
taskscheduler_create(taskname="Run_Live_Trading",
                     rscript=paste0(working.dir, "Live_Trading.R"), 
                     schedule="ONCE"
                     #starttime=format(Sys.time() + 20, "%H:%M:%S")
)


# manually set the triggers to
# (1)
# at 13:10:00
# every "Monday", "Tuesday", "Wednesday", "Thursday", and "Friday",
# 
# (2)
# at 13:50:00
# every "Monday", "Tuesday", "Wednesday", "Thursday", and "Friday"; and
# 
# (3)
# at 23:40:00
# every "Sunday", "Monday", "Tuesday", "Wednesday", and "Thursday"
taskscheduler_create(taskname="Stop_Live_Trading",
                     rscript=paste0(working.dir, "Stop_Live_Trading.R"), 
                     starttime="13:10:00",
                     days=c("MON", "TUE", "WED", "THU", "FRI"))


# manually set the triggers to
# at 15:00:00
# every "Monday", "Tuesday", "Wednesday", "Thursday", and "Friday"
taskscheduler_create(taskname="Daily_Hist_Data_Save",
                     rscript=paste0(working.dir, "Daily_Hist_Data_Save.R"), 
                     starttime="15:00:00",
                     days=c("MON", "TUE", "WED", "THU", "FRI"))




#
taskscheduler_runnow("Run_Live_Trading")
taskscheduler_runnow("Stop_Live_Trading")
taskscheduler_runnow("Daily_Hist_Data_Save")

#
# taskscheduler_delete("Stop_Live_Trading_2")
# taskscheduler_delete("Stop_Live_Trading_3")
# lapply(c("Run_Live_Trading",
#          "Stop_Live_Trading"), taskscheduler_delete)






