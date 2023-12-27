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
Device="laptop" # "laptop" or "desktop"

if(Device=="desktop"){
  # desktop
  working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/"
  data.dir="E:/Stock_Data/" # upper folder that has a folder storing stock data
  rdata.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/"
  
  # daily.data.dir
  # separately stored due to the slack token that is not supposed to be posted publicly on the internet (e.g. GitHub)
  daily.data.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/"
  
  source(paste0("C:/Users/JinCheol Choi/Desktop/R/Functions/Functions.R"))
}else if(Device=="laptop"){
  # laptop
  working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/"
  data.dir="C:/Users/jchoi02/Desktop/Data/" # upper folder that has a folder storing stock data
  rdata.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis_Daily_Data/Rdata/"
  
  source(paste0("C:/Users/jchoi02/Desktop/R/Functions/Functions.R"))
}


#*****************
#
# preliminary step
#
#*******************
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
# -- 1
# manually remove a quote string that creates a log text file in Actions in Task Scheduler as belows
# before removal : /c C:/PROGRA~1/R/R-4.0.5/bin/Rscript.exe "C:/Users/jchoi02/Desktop/R/Stock_Analysis/Stop_Live_Trading.R"  >> "C:/Users/jchoi02/Desktop/R/Stock_Analysis/Stop_Live_Trading.log" 2>&1
# after removal : /c C:/PROGRA~1/R/R-4.0.5/bin/Rscript.exe "C:/Users/jchoi02/Desktop/R/Stock_Analysis/Stop_Live_Trading.R"
#*************************************************************************************************************************
# -- 2
# check out the version of R
#
# As an example, change from
#    from C:/PROGRA~1/R/R-4.0.5/bin/Rscript.exe "C:/Users/jchoi02/Desktop/R/Stock_Analysis/Stop_Live_Trading.R"
#    to C:/PROGRA~1/R/R-41.1.2/bin/Rscript.exe "C:/Users/jchoi02/Desktop/R/Stock_Analysis/Stop_Live_Trading.R"
# The numbers following R-*** appears to be the version of R
#***********************************************************
# taskscheduler_create(taskname="Run_Live_Trading",
#                      rscript=paste0(working.dir, "Live_Trading.R"),
#                      schedule="ONCE"
#                      #starttime=format(Sys.time() + 20, "%H:%M:%S")
# )
# 
# 
# # manually set the triggers to
# # (1) - this is no longer applicable since the futures are still in trading between 13:15:00 to 14:00:00
# # at 13:14:00
# # every "Monday", "Tuesday", "Wednesday", "Thursday", and "Friday",
# #
# # (2)
# # at 13:54:00
# # every "Monday", "Tuesday", "Wednesday", "Thursday", and "Friday"; and
# taskscheduler_create(taskname="Stop_Live_Trading",
#                      rscript=paste0(working.dir, "Stop_Live_Trading.R"),
#                      starttime="13:10:00",
#                      days=c("MON", "TUE", "WED", "THU", "FRI"))
# 
# 
# # manually set the triggers to
# # at 14:20:00
# # every "Monday", "Tuesday", "Wednesday", "Thursday", and "Friday"
# taskscheduler_create(taskname="Daily_Hist_Data_Save",
#                      rscript=paste0(daily.data.dir, "Daily_Hist_Data_Save.R"), # separately stored due to the slack token that is not supposed to be posted publicly on the internet (e.g. GitHub)
#                      starttime="15:00:00",
#                      days=c("MON", "TUE", "WED", "THU", "FRI"))


#
taskscheduler_runnow("Run_Live_Trading")
taskscheduler_runnow("Stop_Live_Trading")
taskscheduler_runnow("Daily_Hist_Data_Save")

#
# taskscheduler_delete("Stop_Live_Trading_2")
# taskscheduler_delete("Stop_Live_Trading_3")
# lapply(c("Run_Live_Trading",
#          "Stop_Live_Trading"), taskscheduler_delete)
