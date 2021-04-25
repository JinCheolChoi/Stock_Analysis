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
BarSize=60 # secs (30 mins bar size seems to need a touch up in the code)

#*****************
#
# preliminary step
#
#*******************
# required functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))



#*********************
#
# ReqRealTimeBars ----
#
#**************************
# request realtime bar data
#*******************************************
# output : BarData in the global environment
# return New_Data=1 if the new data is derived; and 0 if not
ReqRealTimeBars_After=function(BarSize=5, i, Log=F){
  # New_Data : 1 if RealTimeBarData is the new data; and 0 if not
  New_Data=0
  
  # if BarSize is not a multiple of 5
  if(BarSize%%5!=0){
    message("BarSize must be a multiple of 5.")
    # round off BarSize to an integer
    BarSize<<-round(BarSize, -1)
    message(paste0("So, it is rounded off ", get("BarSize", envir=.GlobalEnv), "."))
    
    return(New_Data)
  }
  
  #****************************************************************
  # RealTimeBarData is stored temporarily in the global environment
  #*************************************************************************************************
  # -> once RealTimeBarData is available, reqRealTimeBars is executed every 5 seconds automatically, 
  # which I think is a built-in functionality in twsCALLBACK_cust
  # -> nope... sometimes data is extracted faster than every 5 seconds
  # -> so, the code is modified to echo out the new data only when it is added
  #***************************************************************************
  RealTimeBarData<<-`5SecsBarHistData`[i, ]
  
  # if it fails to create RealTimeBarData
  if(!exists("RealTimeBarData")){
    Sys.sleep(0.5) # suspend execution for a while to prevent the system from breaking
    return(New_Data) # terminate the algorithm by retunning New_Data
  }
  
  # initial Recent_RealTimeBarData
  if(!exists("Recent_RealTimeBarData")){
    Recent_RealTimeBarData=RealTimeBarData
  }
  
  # remove Wap (redundant variable)
  #RealTimeBarData[, Wap:=NULL]
  
  # if BarSize=5, no additional process is required
  if(BarSize==5){
    # if RealTimeBarData is not the new data
    if(!is.null(BarData) & sum(Recent_RealTimeBarData!=RealTimeBarData)==0){
      # remove RealTimeBarData at the end of everytime iteration
      rm(RealTimeBarData, envir=.GlobalEnv)
      
      return(New_Data) # if the new data is not derived, terminate the algorithm by retunning New_Data
    }
    
    BarData<<-rbind(BarData, RealTimeBarData)
    
    New_Data=1
  }
  
  # if BarSize>5 and it is a multiple of 5
  if(BarSize>5 & BarSize%%5==0){
    # if RealTimeBarData is not the new data
    if(exists("Archiv") & sum(Recent_RealTimeBarData!=RealTimeBarData)==0){
      # remove RealTimeBarData at the end of everytime iteration
      rm(RealTimeBarData, envir=.GlobalEnv)
      
      return(New_Data) # if the new data is not derived, terminate the algorithm by retunning New_Data
    }
    
    # initiate archiving RealTimeBarData info once the remainder of time/BarSize is 0
    if(as.numeric(RealTimeBarData$Time)%%BarSize==0){
      Archiv<<-1
    }
    
    # if no need to archive RealTimeBarData
    if(!exists("Archiv", envir=.GlobalEnv)){
      return(New_Data)
    }
    
    # archive RealTimeBarData
    # open info
    if(as.numeric(RealTimeBarData$Time)%%BarSize==0){
      Symbol<<-RealTimeBarData$Symbol
      Time<<-RealTimeBarData$Time
      Open<<-RealTimeBarData$Open
      High<<-RealTimeBarData$High
      Low<<-RealTimeBarData$Low
      Volume<<-RealTimeBarData$Volume
      Count<<-RealTimeBarData$Count
    }
    
    # interim into
    if(as.numeric(RealTimeBarData$Time)%%BarSize>0){
      High<<-max(High, RealTimeBarData$High)
      Low<<-min(Low, RealTimeBarData$Low)
      Volume<<-Volume+RealTimeBarData$Volume
      Count<<-Count+RealTimeBarData$Count
    }
    
    # close info
    if(as.numeric(RealTimeBarData$Time)%%BarSize==(BarSize-5)){
      # remove the Archive indicator
      rm(Archiv, envir=.GlobalEnv)
      
      Close<<-RealTimeBarData$Close
      
      BarData<<-unique(rbind(BarData, 
                             data.table(
                               Symbol=Symbol,
                               Time=Time,
                               Open=Open,
                               High=High,
                               Low=Low,
                               Close=Close,
                               Volume=Volume,
                               Count=Count
                             )))
      
      # remove values in the global environment after generating a data point
      rm(Symbol, envir=.GlobalEnv)
      rm(Time, envir=.GlobalEnv)
      rm(Open, envir=.GlobalEnv)
      rm(High, envir=.GlobalEnv)
      rm(Low, envir=.GlobalEnv)
      rm(Close, envir=.GlobalEnv)
      rm(Volume, envir=.GlobalEnv)
      rm(Count, envir=.GlobalEnv)
      
      New_Data=1
      
    }
    
  }
  
  # if the new data is added
  if(New_Data==1){
    # echo the updated data
    # print(tail(BarData, 1))
    
    # write log everytime new data is added
    if(Log==T){
      if(file.exists(paste0(working.dir, "/Log/Live_Trading_Log.csv"))){
        Log=data.table(Time=Sys.time())
        Log=rbind(Log,
                  fread(paste0(working.dir, "/Log/Live_Trading_Log.csv")))
        fwrite(Log, paste0(working.dir, "/Log/Live_Trading_Log.csv"))
      }else{
        Log=data.table(Time=Sys.time())
        fwrite(Log, paste0(working.dir, "/Log/Live_Trading_Log.csv"))
      }
    }
  }
  
  # save Recent_RealTimeBarData
  Recent_RealTimeBarData<<-RealTimeBarData
  
  # remove RealTimeBarData at the end of everytime iteration
  rm(RealTimeBarData, envir=.GlobalEnv)
  
  return(New_Data) # terminate the algorithm by retunning New_Data
  
}











#
ReqRealTimeBars_Before=function(BarSize=5, i, Log=F){
  # RealTimeBarData is stored temporarily in the global environment
  #*************************************************************************************************
  # -> once RealTimeBarData is available, reqRealTimeBars is executed every 5 seconds automatically, 
  # which I think is a built-in functionality in twsCALLBACK_cust
  # -> nope... sometimes data is extracted faster than every 5 seconds
  # -> so, the code is modified to echo out the new data only when it is added
  #***************************************************************************
  RealTimeBarData<<-`5SecsBarHistData`[i, ]
  
  
  # New_Data : 1 if RealTimeBarData is the new data; and 0 if not
  New_Data=0
  
  # if it fails to create RealTimeBarData, suspend execution for a while to prevent the system from breaking
  if(!exists("RealTimeBarData")){
    Sys.sleep(0.5)
    return(New_Data) # if the new data is not derived, terminate the algorithm by retunning New_Data
    
  }else if(exists("RealTimeBarData")){
    # remove Wap (redundant variable)
    RealTimeBarData[, Wap:=NULL]
    
    # generate BarData
    if(BarSize==5){ # if BarSize=5, no additional process is required
      if(!is.null(BarData) & (sum(tail(BarData, 1)==RealTimeBarData)==ncol(RealTimeBarData))){ # if RealTimeBarData is not the new data
        # remove RealTimeBarData at the end of everytime iteration
        rm(RealTimeBarData, envir=.GlobalEnv)
        
        return(New_Data) # if the new data is not derived, terminate the algorithm by retunning New_Data
        
      }else{ # if RealTimeBarData is the new data
        BarData<<-unique(rbind(BarData, RealTimeBarData))
        
        New_Data=1
      }
      
    }else if(BarSize>5 & BarSize%%5==0){ # additional process given that BarSize>5 and it is a multiple of 5
      if(as.numeric(RealTimeBarData$Time)%%BarSize==0){Init<<-1} # initiate when the remainder is 0
      if(exists("Init", envir=.GlobalEnv)){ # main process part
        if(as.numeric(RealTimeBarData$Time)%%BarSize==0){ # open info
          Symbol<<-RealTimeBarData$Symbol
          Time<<-RealTimeBarData$Time
          Open<<-RealTimeBarData$Open
          High<<-RealTimeBarData$High
          Low<<-RealTimeBarData$Low
          Volume<<-RealTimeBarData$Volume
          Count<<-RealTimeBarData$Count
        }else if(as.numeric(RealTimeBarData$Time)%%BarSize>0){
          High<<-max(High, RealTimeBarData$High)
          Low<<-min(Low, RealTimeBarData$Low)
          Volume<<-Volume+RealTimeBarData$Volume
          Count<<-Count+RealTimeBarData$Count
        }
        
        # close info
        if(as.numeric(RealTimeBarData$Time)%%BarSize==(BarSize-5)){
          if(!is.null(BarData) & (sum(tail(BarData, 1)==RealTimeBarData)==ncol(RealTimeBarData))){ # if RealTimeBarData is not the new data
            # remove RealTimeBarData at the end of everytime iteration
            rm(RealTimeBarData, envir=.GlobalEnv)
            
            return(New_Data) # if the new data is not derived, terminate the algorithm by retunning New_Data
            
          }else{ # if RealTimeBarData is the new data
            BarData<<-unique(rbind(BarData, RealTimeBarData))
            
            Close<<-RealTimeBarData$Close
            
            BarData<<-unique(rbind(BarData, 
                                   data.table(
                                     Symbol=Symbol,
                                     Time=Time,
                                     Open=Open,
                                     High=High,
                                     Low=Low,
                                     Close=Close,
                                     Volume=Volume,
                                     Count=Count
                                   )))
            
            # remove values in the global environment after generating a data point
            rm(Init, envir=.GlobalEnv)
            rm(Symbol, envir=.GlobalEnv)
            rm(Time, envir=.GlobalEnv)
            rm(Open, envir=.GlobalEnv)
            rm(High, envir=.GlobalEnv)
            rm(Low, envir=.GlobalEnv)
            rm(Close, envir=.GlobalEnv)
            rm(Volume, envir=.GlobalEnv)
            rm(Count, envir=.GlobalEnv)
            
            New_Data=1
          }
        }
      }
    }else{
      message("BarSize must be a multiple of 5.")
      # round off BarSize to an integer
      BarSize<<-round(BarSize, -1)
      message(paste0("So, it is rounded off from ", BarSize, " to ", get("BarSize", envir=.GlobalEnv), "."))
      
      # remove RealTimeBarData at the end of everytime iteration
      rm(RealTimeBarData, envir=.GlobalEnv)
      
      return(New_Data) # if the new data is not derived, terminate the algorithm by retunning New_Data
    }
    
    # if the new data is added
    if(New_Data==1){
      # echo the updated data
      # print(tail(BarData, 1))
      
      # write log everytime new data is added
      if(Log==T){
        if(file.exists(paste0(working.dir, "/Log/Live_Trading_Log.csv"))){
          Log=data.table(Time=Sys.time())
          Log=rbind(Log,
                    fread(paste0(working.dir, "/Log/Live_Trading_Log.csv")))
          fwrite(Log, paste0(working.dir, "/Log/Live_Trading_Log.csv"))
        }else{
          Log=data.table(Time=Sys.time())
          fwrite(Log, paste0(working.dir, "/Log/Live_Trading_Log.csv"))
        }
      }
    }
    
    # remove RealTimeBarData at the end of everytime iteration
    rm(RealTimeBarData, envir=.GlobalEnv)
    
    return(New_Data) # terminate the algorithm by retunning New_Data
  }
}




#************
# import data
#************
# output : `5SecsBarHistData`
Import_HistData(Location=paste0(working.dir, "Data/"),
                Symbol=Symbol,
                First_Date=First_Date,
                Last_Date=Last_Date)

# collapse data to the chosen-sized bar data
Collapsed_BarData=Collapse_5SecsBarData(`5SecsBarHistData`,
                                        BarSize=BarSize,
                                        Convert_Tz=T)



#****************
#
# Test algorithm
#
#****************
# BarData5Secs=c()
T1=system.time({
  BarData=c()
  for(i in 1:nrow(`5SecsBarHistData`)){
    # connect to tws
    
    # request realtime bar data
    # output : BarData
    if(!ReqRealTimeBars_Before(BarSize, i, Log=F)){ # skip to the next iteration if the new data is not derived (New_Data==0)
      next
    }
    
    # candle chart
    #Candle_Chart(BarData)
    
    # determine an action
    
    # place an order
  }
})


T2=system.time({
  BarData=c()
  for(i in 1:nrow(`5SecsBarHistData`)){
    # connect to tws
    
    # request realtime bar data
    # output : BarData
    if(!ReqRealTimeBars_After(BarSize, i, Log=F)){ # skip to the next iteration if the new data is not derived (New_Data==0)
      next
    }
    
    # candle chart
    #Candle_Chart(BarData)
    
    # determine an action
    
    # place an order
  }
})



