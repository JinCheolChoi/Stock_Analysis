#**************************************
#
# [ --- Operational functions --- ] ----
#
#*******************
# checkpackages
# Example
#********
# lapply(c("geepack"), checkpackages)
checkpackages=function(package){
  # Checking the Availability of packages 
  # Installs them.  
  # Example usage: checkpackages("gtools")
  if (!package %in% installed.packages()){
    install.packages(package)
  }
  library(package, character.only=T)
}





#*************
# System_Break
#***********************************************
# a break during the temporary market close time
#******************************************************************************
# Output : return Rerun_Live_Trading=1 (1) after break during market closure or 
#                                      (2) after a temporary break (for test) during the market open time
System_Break=function(Rerun_Trading=0,
                      Log=F){
  
  #**************************
  # No break (market is open)
  # re-run indicator
  Rerun=1
  Duration=60
  
  # the market closes temporarily on weekdays
  ToDay=weekdays(as.Date(format(Sys.time(), tz="America/Los_Angeles")))
  CurrentTime=as.ITime(format(Sys.time(), tz="America/Los_Angeles")) # time zone : PDT
  
  #**********************
  # Daily temporary break
  # # if time is between 13:14:00 and 13:15:00 PDT
  # if(ToDay%in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")&
  #    (CurrentTime>=(as.ITime("13:14:00"))&
  #     CurrentTime<=(as.ITime("13:15:00")))){
  #   
  #   # (1) for 16 mins from 13:14:00 to 13:30:00 PDT (market closed : 13:15:00 to 13:30:00 PDT)
  #   Duration=60*16
  #   
  #   # put the system to sleep
  #   message("market closed : 13:15:00 to 13:30:00 PDT")
  #   
  # }
  
  # if time is between 13:59:00 and 14:00:00 PDT
  if(ToDay%in%c("Monday", "Tuesday", "Wednesday", "Thursday")&
     (CurrentTime>=(as.ITime("13:59:00"))&
      CurrentTime<=(as.ITime("14:00:00")))){
    
    # (2) for 62 mins from 13:59:00 to 15:00:00 PDT (market closed : 14:00:00 to 15:00:00 PDT)
    Duration=60*61
    
    # put the system to sleep
    message("market closed : 14:00:00 to 15:00:00 PDT")
    
  }
  
  # # if time is between 23:40:00 and 23:45:00 PDT
  # if(ToDay%in%c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday")&
  #          (CurrentTime>=(as.ITime("23:40:00"))& 
  #           CurrentTime<=(as.ITime("23:45:00")))){
  # 
  #   # (3) for 20 mins from 23:40:00 to 24:00:00 PDT (TWS automatic log-off)
  #   Duration=60*20
  # 
  #   # put the system to sleep
  #   message("TWS automatic log-off and restart")
  # 
  # }
  
  #***********
  # Long break
  if((ToDay=="Friday" & CurrentTime>=(as.ITime("13:59:00"))) | # the market closes at 14:00:00 PDT on Friday
     (ToDay=="Saturday") | # the market closes on Saturday
     (ToDay=="Sunday" & CurrentTime<(as.ITime("15:01:00")))){ # the market opens at 15:00:00 PDT on Sunday
    
    # no need to re-run the live trading algorithm during weekends
    Rerun=0
    Duration=600
    
    # put the system to sleep
    message("weekend closure")
  }
  
  # write log everytime System_Break is run
  if(Log==T){
    if(file.exists(paste0(working.dir, "Log/Stop_Live_Trading_Log.csv"))){
      Log=data.table(Time=Sys.time())
      Log=rbind(Log,
                fread(paste0(working.dir, "Log/Stop_Live_Trading_Log.csv")))
      fwrite(Log, paste0(working.dir, "Log/Stop_Live_Trading_Log.csv"))
    }else{
      Log=data.table(Time=Sys.time())
      fwrite(Log, paste0(working.dir, "Log/Stop_Live_Trading_Log.csv"))
    }
  }
  
  # echo re-run message
  if(Rerun==1){
    for(Secs_Left in 1:(Duration-1)){
      message(paste0("Re-run live trading in ", Duration-Secs_Left+1, " seconds"))
      Sys.sleep(1)
    }
    message(paste0("Re-run live trading in : 0 seconds"))
  }
  
  return(Rerun)
}





#*********************
# Daily_Hist_Data_Save
#*************************************************************
# execute a daily save of 5 second bar data at 15:00:00 pm PDT
Daily_Hist_Data_Save=function(Contract,
                              Data_Dir,
                              Working_Dir,
                              Force=T,
                              Log=F,
                              Market="Futures",
                              N_Days=0){
  
  # the market closes at 14:00:00 PDT on Friday; and
  # at 15:00:00 PDT on the other weekdays
  ToDay=weekdays(as.Date(format(Sys.time(), tz="America/Los_Angeles")))
  CurrentTime=as.ITime(format(Sys.time(), tz="America/Los_Angeles")) # time zone : PDT
  File_Exist=file.exists(paste0(Data_Dir, Contract$symbol, "/", Contract$symbol, "_", as.Date(format(Sys.time(), tz="America/Los_Angeles")), ".csv"))
  
  # # if time is after 15:00:00 PDT on weekdays, proceed ot extract historical data
  # if(!ToDay%in%c("Saturday", "Sunday") &
  #    CurrentTime>=(as.ITime("14:00:00"))){
  #   
  #   
  # }else{
  #   message("No new data to save yet.") # echo Message in terminal
  # }
  
  if(!File_Exist| # if 5 second bar has not been saved yet, or
     Force==T){ # Force==1 (execute the saving process by force (overwrite the data))
    
    # request historical data of 5 seconds bar
    HistData=as.data.table(reqHistoricalData(tws, Contract, barSize="5 secs", duration=paste0(N_Days+2, " D"), useRTH="0")) # useRTH="0" : not limited to regular trading hours
    colnames(HistData)=c("Time", "Open", "High", "Low", "Close", "Volume", "Wap", "hasGaps", "Count")
    
    HistData[, hasGaps:=NULL] # hasGaps is redundant
    
    HistData=data.table(Symbol=Contract$symbol,
                        HistData)
    
    # HistData=as.data.table(reqHistoricalData(tws, Contract, barSize="5 secs", duration="1 M", useRTH="0")) # useRTH="0" : not limited to regular trading hours
    # colnames(HistData)=c("Time", "Open", "High", "Low", "Close", "Volume", "Wap", "hasGaps", "Count")
    #
    # HistData[, hasGaps:=NULL] # hasGaps is redundant
    #
    # HistData=data.table(Symbol=Contract$symbol,
    #                     HistData)
    #
    # # "for statement" to get and save bar data day-by-day
    # for(Date in seq(as.Date("2021-03-15"), as.Date(format(Sys.time(), tz="America/Los_Angeles")), by="day")){
    #   if(weekdays.Date(as.Date(Date))=="Saturday"|
    #      weekdays.Date(as.Date(Date))=="Sunday"){
    #     next
    #   }
    #
    #   Time_Cutoff=as.POSIXct(paste0(as.Date(Date), " 15:00:00"), tz="America/Los_Angeles")
    #
    #
    #   HistData[Time>=(Time_Cutoff-60*60*24)&
    #              Time<Time_Cutoff, ]
    #
    #   fwrite(HistData[Time>=(Time_Cutoff-60*60*24)&
    #                     Time<Time_Cutoff, ],
    #          paste0(Data_Dir, Contract$symbol, "_", as.Date(Date), ".csv"))
    # }
    
    #****************
    # N_Days days ago
    # remove redundant data
    # different time zone examples : "GMT", "America/Los_Angeles", "Europe/London"
    for(N_Day in N_Days:0){
      if(Market=="Futures"){
        Time_From=as.POSIXct(paste0(as.Date(format(Sys.time(), tz="America/Los_Angeles"))-(N_Day+1), " 15:00:00"), tz="America/Los_Angeles")
        Time_To=as.POSIXct(paste0(as.Date(format(Sys.time(), tz="America/Los_Angeles"))-N_Day, " 15:00:00"), tz="America/Los_Angeles")
      }else if(Market=="Stock"){
        Time_From=as.POSIXct(paste0(as.Date(format(Sys.time(), tz="America/Los_Angeles"))-(N_Day+1), " 17:00:00"), tz="America/Los_Angeles")
        Time_To=as.POSIXct(paste0(as.Date(format(Sys.time(), tz="America/Los_Angeles"))-N_Day, " 17:00:00"), tz="America/Los_Angeles")
      }
      
      HistData_Temp=HistData[Time>=Time_From &
                               Time<Time_To, ]
      
      HistData_Temp[, Time:=as.POSIXct(format(as.POSIXct(Time), 
                                              tz="America/Los_Angeles"), 
                                       tz="America/Los_Angeles")]
      
      # save historical data up to today's market closed at 15:00:00 pm PDT
      fwrite(HistData_Temp,
             paste0(Data_Dir, Contract$symbol, "/", Contract$symbol, "_", as.Date(format(Sys.time(), tz="America/Los_Angeles"))-N_Day, ".csv"))
    }
  }
  
  # create a folder if not exist
  if(!dir.exists(paste0(Data_Dir, Contract$symbol))){
    dir.create(paste0(Data_Dir, Contract$symbol))
  }
  
  # write log everytime historical data is extracted and saved
  if(Log==T){
    if(!dir.exists(paste0(Working_Dir, "Log"))){
      dir.create(paste0(Working_Dir, "Log"))
    }
    
    if(file.exists(paste0(Working_Dir, "Log/Daily_Hist_Data_Save.csv"))){
      Log=data.table(Symbol=Contract$symbol,
                     Time=Sys.time())
      Log=rbind(Log,
                fread(paste0(Working_Dir, "Log/Daily_Hist_Data_Save.csv")))
      fwrite(Log, paste0(Working_Dir, "Log/Daily_Hist_Data_Save.csv"))
    }else{
      Log=data.table(Symbol=Contract$symbol,
                     Time=Sys.time())
      fwrite(Log, paste0(Working_Dir, "Log/Daily_Hist_Data_Save.csv"))
    }
  }
}





#*****************************
#
# [ --- Simultation --- ] ----
#
#*****************************
# Get_Data
#**********************************************************************************
# get import historical data sets of specified symbols saved in a repository folder
#**********************************************************************************
Get_Data=function(Symbols,
                  Data_Dir,
                  BarSize=60*30,
                  First_Date="2021-01-20",
                  Last_Date=as.Date(format(Sys.time(), tz="America/Los_Angeles")),
                  Convert_Tz=F,
                  Filter=TRUE){
  #************
  # import data
  #************
  # output : `5SecsBarHistData`
  for(Symbol in Symbols){
    Get_5SecsBarHistData(Symbol=Symbol,
                         Data_Dir=Data_Dir,
                         First_Date=First_Date,
                         Last_Date=Last_Date,
                         Convert_Tz=F,
                         Filter=Filter)
    
    # collapse data to the chosen-sized bar data
    assign(Symbol, 
           Collapse_5SecsBarData(`5SecsBarHistData`,
                                 BarSize=BarSize,
                                 Convert_Tz=Convert_Tz),
           envir=.GlobalEnv)
    
    # remove `5SecsBarHistData`
    rm(`5SecsBarHistData`, envir=.GlobalEnv)
  }
}





#*********************
# Get_5SecsBarHistData
#****************************************************
# import historical data saved in a repository folder
#******************************************************
# output : `5SecsBarHistData` in the global environment
Get_5SecsBarHistData=function(Symbol,
                              Data_Dir,
                              First_Date,
                              Last_Date,
                              Convert_Tz=F,
                              Filter=T){
  # data table
  lapply("data.table", checkpackages)
  
  # remove `5SecsBarHistData` in the global environment
  if(exists("5SecsBarHistData")){rm(`5SecsBarHistData`, envir=.GlobalEnv)}
  
  # import
  for(Date in as.character(seq(as.Date(First_Date), as.Date(Last_Date), by="day"))){
    File_name=paste0(Symbol, "_", Date, ".csv")
    if(!file.exists(paste0(Data_Dir, Symbol, "/", File_name))){
      next
    }
    
    if(!exists("5SecsBarHistData", envir=.GlobalEnv)){
      `5SecsBarHistData`<<-fread(paste0(Data_Dir, Symbol, "/", File_name))
    }else{
      `5SecsBarHistData`<<-rbind(`5SecsBarHistData`,
                                 fread(paste0(Data_Dir, Symbol, "/", File_name)))
    }
  }
  
  # convert time zone
  # this process of converting time to the PDT time zone can be skipped as needed for less processing time
  if(Convert_Tz==T){
    `5SecsBarHistData`[, Time:=as.POSIXct(format(as.POSIXct(Time),
                                                 tz="America/Los_Angeles"),
                                          tz="America/Los_Angeles")]
  }
  
  # remove data while system is halted during temporary market close times
  if(Filter==T){
    #(1) (not applicable) for 17 mins from 13:14:00 to 13:31:00 PDT (market closed : 13:15:00 to 13:30:00 PDT)
    #(2) for 63 mins from 13:59:00 to 15:00:00 PDT (market closed : 14:00:00 to 15:00:00 PDT)
    # (ToDay=="Friday" & CurrentTime>=(as.ITime("13:50:00"))) | # the market closes at 14:00:00 PDT on Friday
    #   (ToDay=="Saturday") | # the market closes on Saturday
    #   (ToDay=="Sunday" & CurrentTime<(as.ITime("15:05:00")))
    `5SecsBarHistData`[, Daily_Time:=strftime(Time, format="%H:%M:%S", tz="America/Los_Angeles")]
    # `5SecsBarHistData`<<-`5SecsBarHistData`[!(Daily_Time>="13:10:00" &
    #                                             Daily_Time<"13:35:00"), ]
    `5SecsBarHistData`<<-`5SecsBarHistData`[!(Daily_Time>="13:59:00" &
                                                Daily_Time<"15:00:00"), ]
    `5SecsBarHistData`[, Daily_Time:=NULL]
  }
}





#**********************
#
# Collapse_5SecsBarData
#
#***************************
# collapse 5 seconds bar data to a larger-sized bar data (maximum size is one day)
#*********************************************************************************
# # verify the collapsed bar data is generated properly by making a comparison with the historical data requested from Interactive Brokers TWS
# HistData=as.data.table(reqHistoricalData(tws, contract, barSize="5 mins", duration="2 D", useRTH="0")) # useRTH="0" : not limited to regular trading hours
# colnames(HistData)=c("Time", "Open", "High", "Low", "Close", "Volume", "Wap", "hasGaps", "Count")
# HistData[, hasGaps:=NULL] # hasGaps is redundant
# HistData=data.table(Symbol=contract$symbol,
#                     HistData)
# Merged_Data=HistData %>% 
#   left_join(Collapsed_BarData,
#             by=c("Symbol", "Time"))
# Merged_Data=Merged_Data[Time%in%(Collapsed_BarData$Time),]
# 
# #
# Merged_Data[Open.x!=Open.y, ]
# Merged_Data[High.x!=High.y, ]
# Merged_Data[Low.x!=Low.y, ]
# Merged_Data[Close.x!=Close.y, ]
# Merged_Data[Volume.x!=Volume.y, ]
# Merged_Data[Count.x!=Count.y, ]
# 
# Merged_Data[is.na(Open.y), ]
# Merged_Data[is.na(High.y), ]
# Merged_Data[is.na(Low.y), ]
# Merged_Data[is.na(Close.y), ]
# Merged_Data[is.na(Volume.y), ]
# Merged_Data[is.na(Count.y), ]
Collapse_5SecsBarData=function(`5SecsBarData`,
                               BarSize,
                               Convert_Tz=F){
  # `5SecsBarData`=`5SecsBarHistData`
  if(BarSize%%5!=0){
    return(message("BarSize must be a multiple of 5"))
  }
  
  # if BarSize=5, no additional process is required
  if(BarSize==5){
    Collapsed_BarData=`5SecsBarData` %>% as.data.frame() %>% as.data.table()
    Collapsed_BarData[, Wap:=NULL]
    
    # Net_Volume
    Collapsed_BarData[Open<Close, Net_Volume:=Volume]
    Collapsed_BarData[Open==Close, Net_Volume:=Volume]
    Collapsed_BarData[Open>Close, Net_Volume:=-Volume]
  }
  
  # if BarSize>5 and it is a multiple of 5 (BarSize%%5==0 is already taken into account in advance)
  if(BarSize>5){
    
    # dates
    Dates=seq(as.Date(min(`5SecsBarData`$Time)), # minimum Date
              as.Date(max(`5SecsBarData`$Time)), # maximum Date
              by="day")
    
    # time intervals
    Times=as.ITime(seq(as.POSIXct(paste0(as.Date(format(Sys.time()))-1, " 00:00:00")),
                       as.POSIXct(paste0(as.Date(format(Sys.time())), " 00:00:00"))-BarSize,
                       by=BarSize))
    
    # Date_Time_From
    Time_Intervals=data.table(
      Date_Time_From=as.POSIXct(strptime(paste(rep(Dates, each = length(Times)), Times, sep = " "), 
                                         "%Y-%m-%d %H:%M:%S"), 
                                tz=attr(`5SecsBarData`$Time, "tzone"))
    )
    
    # Date_Time_To
    Time_Intervals[, Date_Time_To:=shift(Time_Intervals$Date_Time_From, -1)]
    
    # Net_Volume
    `5SecsBarData`[Open<Close, Net_Volume:=Volume]
    `5SecsBarData`[Open==Close, Net_Volume:=Volume]
    `5SecsBarData`[Open>Close, Net_Volume:=-Volume]
    
    # non-equal left_join Time_Intervals to `5SecsBarData`
    Time_Intervals=`5SecsBarData`[Time_Intervals, on=c("Time>=Date_Time_From", "Time<Date_Time_To"),
                                  nomatch=0,
                                  .(Symbol, Date_Time_From=Time, Open, High, Low, Close, Volume, Wap, Count, Net_Volume, Date_Time_To)]
    
    # Collapsed_BarData
    Collapsed_BarData=Time_Intervals[, .(Symbol=unique(Symbol),
                                         Open=Open[1], # first row by group (Date_Time_From)
                                         High=max(High),
                                         Low=min(Low), # row with minimum Low price by group
                                         Close=Close[.N], # last row by group
                                         Volume=sum(Volume),
                                         Count=sum(Count),
                                         Net_Volume=sum(Net_Volume)),
                                     by="Date_Time_From"]
    
    # rename Date_Time_From to Time
    setnames(Collapsed_BarData, "Date_Time_From", "Time")
    
    # switch the order "Symbol" and "Date_Time_From"
    setcolorder(Collapsed_BarData, c("Symbol", "Time"))
    
    # `5SecsBarData`[, Group:=NULL]
  }
  
  # convert time zone
  # this process of converting time to the PDT time zone can be skipped as needed for less processing time
  if(Convert_Tz==T){
    Collapsed_BarData[, Time:=as.POSIXct(format(as.POSIXct(Time),
                                                tz="America/Los_Angeles"),
                                         tz="America/Los_Angeles")]
  }
  
  return(as.data.table(Collapsed_BarData))
}





#*************
# Candle_Chart
#******************
Candle_Chart=function(BarData){
  if(!is.null(BarData)){
    Temp_BarData=as.matrix(BarData[, 3:6])
    rownames(Temp_BarData)=as.character(as.Date(BarData$Time)+(0:(nrow(Temp_BarData)-1)))
    
    # PlotCandlestick from "DescTools"
    PlotCandlestick(x=as.Date(rownames(Temp_BarData)),
                    y=Temp_BarData,
                    border=NA,
                    las=1,
                    ylab="",
                    xaxt="n")
    
    # x-axis labels
    # year
    j=Year(as.Date(BarData$Time))
    j[!c(1, diff(j))]=NA
    mtext(side=1, at=as.Date(rownames(Temp_BarData)), text=j, cex=1, line=0)
    
    # month
    j=Month(as.Date(BarData$Time))
    j[!c(1, diff(j))]=NA
    mtext(side=1, at=as.Date(rownames(Temp_BarData)), text=month.name[j], cex=0.9, line=0.9)
    
    # day
    j=Day(as.Date(BarData$Time))
    j[!c(1, diff(j))]=NA
    mtext(side=1, at=as.Date(rownames(Temp_BarData)), text=j, cex=0.8, line=1.7)
    
    # hour-min-sec
    mtext(side=1,
          at=as.Date(rownames(Temp_BarData)),
          text=as.character(as.ITime(BarData$Time)),
          cex=0.7,
          line=2.4)
  }
}





# #***********
# # BBands_Sim
# #**************************************************
# # Fast simulation based on bollinger bands strategy
# #**************************************************
# BBands_Sim=function(Consec_Times,
#                     Long_PctB,
#                     Short_PctB,
#                     Commision=0.52){
#   # Long_Pos_Ind : indice of rows to which a long position is filled
#   # fill long positions if PctB is below Long_PctB for consecutive times (Consec_Times) in the recent bar data
#   if(Consec_Times==1){
#     Long_Pos_Ind=which(shift(Collapsed_BarData$PctB, 0)<Long_PctB)
#   }else{
#     Long_Pos_Ind=which(Reduce("+", lapply(shift(Collapsed_BarData$PctB, 0:(Consec_Times-1)),
#                                           function(x) x<Long_PctB))==Consec_Times)
#   }
#   Short_Pos_Ind=which(Collapsed_BarData$PctB>Short_PctB) # a short position must be filled after a long position
#   
#   #
#   Tradings=data.table(
#     Collapsed_BarData[c(Long_Pos_Ind+1), 
#                       .SD,
#                       .SDcols=c("Time", "Open")],
#     Collapsed_BarData[sapply(Long_Pos_Ind,
#                              function(x) Short_Pos_Ind[which(x<Short_Pos_Ind)[1]])+1,
#                       .SD,
#                       .SDcols=c("Time", "Open")]
#   )
#   colnames(Tradings)=c("Long_Time", "Long_Price", "Short_Time", "Short_Price")
#   
#   # remove rows with duplicated short positions
#   Tradings=Tradings[!duplicated(Tradings[,
#                                          .SD,
#                                          .SDcols=c("Short_Time", "Short_Price")])&
#                       !is.na(Short_Price), ]
#   
#   # output
#   Out=c()
#   
#   Out$Tradings=Tradings
#   Out$Profit=sum(Tradings$Short_Price-Tradings$Long_Price, na.rm=T)*4*0.5
#   Out$Commision=2*Commision*nrow(Tradings)
#   Out$Net_Profit=Out$Profit-Out$Commision
#   
#   return(Out)
# }





#**********************
# Live_Trading_Imitator
#*********************************************
# run an algorithm under the realistic setting
#************************************************
Live_Trading_Imitator=function(BarData,
                               Strategy){
  Max_Rows=Strategy[["Max_Rows"]]
  Order_Rules=Strategy[["Order_Rules"]]
  Indicators=Strategy[["Indicators"]]
  Models=Strategy[["Models"]]
  
  #****************
  # import packages
  #****************
  lapply(c("IBrokers",
           "TTR",
           "data.table",
           "dplyr"),
         checkpackages)
  
  #************************
  # assign local parameters
  #************************
  Max_Orders=as.numeric(Order_Rules[["General"]][["Max_Orders"]])
  Scenario=Order_Rules[["General"]][["Scenario"]]
  Reverse=Order_Rules[["General"]][["Reverse"]]
  Stop_Order=as.numeric(Order_Rules[["General"]][["Stop_Order"]])
  Profit_Order=as.numeric(Order_Rules[["General"]][["Profit_Order"]])
  Maximum_Elapsed_Time=as.numeric(Order_Rules[["General"]][["Maximum_Elapsed_Time"]])
  Strategy_Indicators=names(Indicators)
  Strategy_Models=names(Models)
  Strategy_Models_Class=unlist(lapply(Models, class))
  General_Strategy="General"
  
  #******************
  # preliminary steps
  #******************
  # if(Position_Direction=="both"){
  #   Max_Long_Orders=Max_Short_Orders=Max_Orders-1
  # }else if(Position_Direction=="long"){
  #   Max_Long_Orders=Max_Orders-1
  #   Max_Short_Orders=-1
  # }else if(Position_Direction=="short"){
  #   Max_Long_Orders=-1
  #   Max_Short_Orders=Max_Orders-1
  # }
  Position_Names=names(Order_Rules)[names(Order_Rules)!=General_Strategy]
  
  #*********************
  # simulation algorithm
  #*********************
  # define Live_Data
  Live_Data=BarData[1, ]
  # define Orders_Transmitted
  Orders_Transmitted=data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                                Submit_Time=tail(Live_Data, 1)[, Time],
                                Filled_Time=tail(Live_Data, 1)[, Time],
                                Action="",
                                Detail="",
                                TotalQuantity=0,
                                OrderType="MKT",
                                Price=0,
                                Filled=0,
                                Sigs_N=0)
  Orders_Transmitted=Orders_Transmitted[-1,]
  Live_Data[, `:=`(Symbol=NULL, Time=NULL, Open=NULL,
                   High=NULL, Low=NULL, Close=NULL,
                   Volume=NULL, Net_Volume=NULL, Count=NULL)]
  Time_Unit=BarData$Time[2]-BarData$Time[1]
  Early_Order_Transmit_Proceeded="No"
  Count=0
  for(i in 1:(nrow(BarData)-1)){
    # i=23
    Live_Data=rbind(Live_Data, BarData[i, ], fill=T) %>% tail(Max_Rows)
    
    # if Early_Order_Transmit_Proceeded was done at i-1, skip
    if(Early_Order_Transmit_Proceeded=="Yes"){
      Early_Order_Transmit_Proceeded="No"
      next
    }
    
    #***********************************************
    #
    # transmit order if there are long/short signals
    #
    #***********************************************
    # Orders_Transmitted
    # Order_Rules[[General_Strategy]]
    # OrderRules_Env[[]]
    # tail(Live_Data, 1)
    # proceed if there is no transmitted order(s) to fill
    # proceed if the number of filled orders is smaller than Max_Orders
    if(sum(Orders_Transmitted[["Filled"]]==0)<Max_Orders
       # abs(nrow(Orders_Transmitted[Filled==1 & Action=="Buy", ])-
       #     nrow(Orders_Transmitted[Filled==1 & Action=="Sell", ]))<Max_Orders
    ){
      #*********************
      # calculate indicators
      #*********************
      Calculated_Indicators=lapply(Strategy_Indicators,
                                   function(x)
                                     if(x=="Close"){
                                       Live_Data[["Close"]]
                                     }else{
                                       if(x=="BBands" & nrow(Live_Data)>=Indicators[[x]][['n']]+1){ # BBands : n-1, RSI : n+1
                                         do.call(x, 
                                                 c(list(Live_Data[["Close"]]), # for now only using "Close price", additional work would be required in the future if the indicator does not depend on "Close price"
                                                   Indicators[[x]]))
                                       }else if(x!="BBands" & nrow(Live_Data)>Indicators[[x]][['n']]+1){
                                         do.call(x, 
                                                 c(list(Live_Data[["Close"]]), # for now only using "Close price", additional work would be required in the future if the indicator does not depend on "Close price"
                                                   Indicators[[x]]))
                                       }
                                     }
      )
      # Calculated_Indicators=Calculated_Indicators[-which(sapply(Calculated_Indicators, is.null))]
      names(Calculated_Indicators)=Strategy_Indicators
      
      # if there is an indicator that hasn't been computed, skip the iteration
      if(sum(unlist(lapply(Calculated_Indicators,
                           function(x){
                             is.null(x)
                           })))>0){
        next
      }
      
      #***********
      # fit models
      #***********
      Signals=as.data.table(sapply(Strategy_Models,
                                   function(x){
                                     Model_Info=Models_Env[[Strategy_Models_Class[x]]] # variables and functions defined for the model object
                                     Calculated_Indicators_Combined=do.call(cbind, Calculated_Indicators) # combined Calculated_Indicators
                                     Calculated_Indicators_Names=names(Calculated_Indicators)[unlist(lapply(Calculated_Indicators, function(x) !is.null(x)))] #
                                     if(sum(!Model_Info[["Essential_Indicators"]]%in%Calculated_Indicators_Names)==0){ # if none of essential indicators hasn't been calculated in Calculated_Indicators, proceed to run the model
                                       do.call(Model_Info[["Function"]],
                                               c(list(Calculated_Indicators_Combined),
                                                 Models[[x]]))}
                                   }))
      
      #***************
      # transmit order
      #***************
      if(nrow(Signals)>0){
        # - N of models <= Sigs_N <= N of models
        Sigs_N=apply(Signals, 1, sum)
        
        # number of orders held (+:more long, -:more short)
        N_Orders_held=sum(Orders_Transmitted[["Action"]]=="Buy")-
          sum(Orders_Transmitted[["Action"]]=="Sell")
        
        # Position_Names_Temp
        # This part allows to force the long position entrance when there is no position filled yet while Sigs_N indicates to enter both positions at the same time
        # Also, it makes sure to transmit only once when N_Orders_held=1 & Max_Orders=1 (or N_Orders_held=-1 & Max_Orders=1).
        # If N_Orders_held>0, Short_Function is ignored and Long_Function is considered to take care of N_Orders_held (and vice versa).
        if(N_Orders_held>0){
          Position_Names_Temp=sort(Position_Names, decreasing=T)
        }else{
          Position_Names_Temp=sort(Position_Names, decreasing=F)
        }
        
        # Order_to_Transmit
        Order_to_Transmit=lapply(Position_Names_Temp,
                                 function(x){
                                   do.call(OrderRules_Env[[paste0(x, "_Function")]],
                                           c(list(Live_Data=Live_Data,
                                                  Time_Unit=Time_Unit,
                                                  Max_Orders=Max_Orders,
                                                  Sigs_N=Sigs_N,
                                                  N_Orders_held=N_Orders_held),
                                             Params=list(Order_Rules[[x]])))
                                 })
        
        # remove Signals
        rm(Signals)
      }
    }
    
    if(exists("Order_to_Transmit")){
      if(!is.null(do.call(rbind, Order_to_Transmit))){
        # Count=Count+1
        # print(paste0("Count : ", Count, " / i : ", i))
        # add Order_to_Transmit to Orders_Transmitted
        Orders_Transmitted=rbind(Orders_Transmitted,
                                 do.call(rbind, Order_to_Transmit),
                                 fill=T)
        # remove Orders_Transmitted
        rm(Order_to_Transmit)
        # print(paste0("Transmit order / i : ", i, " / action : ", tail(Orders_Transmitted[["Detail"]], 1)))
      }
    }
    
    #***********
    # fill order
    #***********
    # buy
    if(sum(Orders_Transmitted[["Action"]]=="Buy"&
           Orders_Transmitted[["Filled"]]==0)>0){ # if there is any transmitted buy order that has not been filled yet
      
      Unfilled_Buy_Position_Times=Orders_Transmitted[Action=="Buy"&Filled==0, Submit_Time]
      Unfilled_Buy_Position_Prices=Orders_Transmitted[Submit_Time%in%Unfilled_Buy_Position_Times&Filled==0, Price]
      Which_Buy_Position_to_Fill=which(BarData[["Low"]][i+1]<Unfilled_Buy_Position_Prices)[1] # fill the oldest order that have met the price criterion
      
      Orders_Transmitted[Submit_Time==Unfilled_Buy_Position_Times[Which_Buy_Position_to_Fill],
                         `:=`(Filled_Time=BarData[i+1, Time],
                              Filled=1)]
      
      # if not filled, just cancel the transmit Maximum_Elapsed_Time seconds later
      if(sum(Orders_Transmitted[["Filled"]]==0)){
        if((as.numeric(BarData[i+1, Time])-as.numeric(Orders_Transmitted[Filled==0, Submit_Time]))>Maximum_Elapsed_Time){
          Orders_Transmitted=Orders_Transmitted[Filled!=0, ]
          #print(paste0("Transmit order / i : ", i, " / action : Cancelled"))
        }
      }else{
        # print(paste0("buy fill", "___", tail(Orders_Transmitted$Detail, 1)))
      }
      
    }
    
    # sell
    if(sum(Orders_Transmitted[["Action"]]=="Sell"&
           Orders_Transmitted[["Filled"]]==0)>0){ # if there is any transmitted sell order that has not been filled yet
      
      Unfilled_Sell_Position_Times=Orders_Transmitted[Action=="Sell"&Filled==0, Submit_Time]
      Unfilled_Sell_Position_Prices=Orders_Transmitted[Submit_Time%in%Unfilled_Sell_Position_Times&Filled==0, Price]
      
      Which_Sell_Position_to_Fill=which(BarData[["High"]][i+1]>Unfilled_Sell_Position_Prices)[1] # fill the oldest order that have met the price criterion
      
      Orders_Transmitted[Submit_Time==Unfilled_Sell_Position_Times[Which_Sell_Position_to_Fill],
                         `:=`(Filled_Time=BarData[i+1, Time],
                              Filled=1)]
      
      # if not filled, just cancel the transmit Maximum_Elapsed_Time seconds later
      if(sum(Orders_Transmitted[["Filled"]]==0)){
        if((as.numeric(BarData[i+1, Time])-as.numeric(Orders_Transmitted[Filled==0, Submit_Time]))>Maximum_Elapsed_Time){
          Orders_Transmitted=Orders_Transmitted[Filled!=0, ]
          #print(paste0("Transmit order / i : ", i, " / action : Cancelled"))
        }
      }else{
        # print(paste0("sell fill", "___", tail(Orders_Transmitted$Detail, 1)))
      }
    }
    
    # skip if currently on no position & there is no order transmitted
    if(nrow(Orders_Transmitted[Filled==1 & Action=="Buy", ])!=nrow(Orders_Transmitted[Filled==1 & Action=="Sell", ])){ # orders are all balanced
      #*********************
      # stop or early profit
      #*********************
      # Orders_Transmitted
      # BarData
      # Scenario
      Early_Order_Transmit=Profit_Loss_Cut_Transmitted(Orders_Transmitted=Orders_Transmitted,
                                                       Next_BarData=BarData[i+1, ],
                                                       Profit_Order=Profit_Order,
                                                       Stop_Order=Stop_Order)
      
      if(!is.null(do.call(rbind, Early_Order_Transmit))){
        Early_Order_Transmit_Proceeded="Yes"
        
        Orders_Transmitted=Orders_Transmitted[Filled!=0, ]
        Order_to_Transmit=Early_Order_Transmit
        if(Scenario=="Positive"){
          Orders_Transmitted=rbind(Orders_Transmitted,
                                   Order_to_Transmit[["Profit_Transmitted"]],
                                   fill=T)
          
          # recalculate N_Remaining_Orders
          N_Remaining_Orders=sum(Orders_Transmitted[["Action"]]=="Buy"&Orders_Transmitted[["Filled"]]==1)-
            sum(Orders_Transmitted[["Action"]]=="Sell"&Orders_Transmitted[["Filled"]]==1)
          if(N_Remaining_Orders!=0){
            Orders_Transmitted=rbind(Orders_Transmitted,
                                     Order_to_Transmit[["Stop_Transmitted"]],
                                     fill=T)
          }
        }
        if(Scenario=="Negative"){
          Orders_Transmitted=rbind(Orders_Transmitted,
                                   Order_to_Transmit[["Stop_Transmitted"]],
                                   fill=T)
          # recalculate N_Remaining_Orders
          N_Remaining_Orders=sum(Orders_Transmitted[["Action"]]=="Buy"&Orders_Transmitted[["Filled"]]==1)-
            sum(Orders_Transmitted[["Action"]]=="Sell"&Orders_Transmitted[["Filled"]]==1)
          if(N_Remaining_Orders!=0){
            Orders_Transmitted=rbind(Orders_Transmitted,
                                     Order_to_Transmit[["Profit_Transmitted"]],
                                     fill=T)
          }
        }
        #print(paste0("Transmit order / i : ", i, " / action : ", tail(Orders_Transmitted[["Detail"]], 1)))
        
        # remove Orders_Transmitted
        rm(Order_to_Transmit)
      }
    }
    
  }
  
  #**********************
  # calculate the balance
  #**********************
  # navigate the last order fill that balances out between opening and closing positions to 0
  Orders_Transmitted[, TotalQuantity_Adj:=TotalQuantity]
  Orders_Transmitted[Action%in%c("Sell"), TotalQuantity_Adj:=-TotalQuantity] # remove outstanding orders
  Orders_Transmitted=Orders_Transmitted[1:Orders_Transmitted[, max(.I[cumsum(TotalQuantity_Adj)==0])], ]
  
  #
  Collapse_Orders_Transmitted=cbind(Orders_Transmitted[Action=="Buy", 
                                                       c("Filled_Time", "Price")],
                                    Orders_Transmitted[Action=="Sell", 
                                                       c("Filled_Time", "Price")])
  colnames(Collapse_Orders_Transmitted)=c("Buy_Time", "Buy_Price", "Sell_Time", "Sell_Price")
  
  if(nrow(Collapse_Orders_Transmitted)>0){
    Duplicated_Row=unique(c(which(duplicated(Collapse_Orders_Transmitted[, c("Buy_Time", "Buy_Price")])), 
                            which(duplicated(Collapse_Orders_Transmitted[, c("Sell_Time", "Sell_Price")]))))
    if(length(Duplicated_Row)>0){
      Collapse_Orders_Transmitted=Collapse_Orders_Transmitted[-Duplicated_Row, ]
    }
    
    Collapse_Orders_Transmitted[, Profit:=2*(Sell_Price-Buy_Price)-2*0.52]
    Collapse_Orders_Transmitted[, Cum_Profit:=cumsum(Profit)]
    Collapse_Orders_Transmitted[, Time:=as.POSIXct(format(as.POSIXct(max(Buy_Time, Sell_Time)),
                                                          tz="America/Los_Angeles")), by=1:nrow(Collapse_Orders_Transmitted)]
    Collapse_Orders_Transmitted[, Date:=as.Date(Time, tz="America/Los_Angeles")]
    Collapse_Orders_Transmitted[, Daily_Cum_Profit:=Cum_Profit[Time==max(Time)], by="Date"]
    Collapse_Orders_Transmitted[, Daily_Profit:=sum(Profit), by="Date"]
    
    Ind_Profit=Collapse_Orders_Transmitted[, .SD, .SDcols=c("Time", "Date", "Profit", "Daily_Profit", "Cum_Profit", "Daily_Cum_Profit")]
    Net_Profit=tail(Collapse_Orders_Transmitted$Cum_Profit, 1)
  }else{
    Ind_Profit=-Inf
    Net_Profit=-Inf
  }
  
  return(list(BarData=BarData,
              Orders_Transmitted=Orders_Transmitted,
              Ind_Profit=Ind_Profit,
              Net_Profit=Net_Profit))
  
}





#************
# Backtesting
#***********************************************************
# run a fast version of backtesting algorithm for simulation
#***********************************************************
Backtesting=function(BarData,
                     Strategy_Name,
                     Working_Dir){
  # BarData_Include_Last_Time
  BarData_Include_Last_Time=copy(BarData)
  
  # remove the last time time point
  # this allows the second last time point to be the last possible order to be transmitted
  BarData=BarData[-nrow(BarData), ]
  
  # BarData is not allowed to be empty
  if(nrow(BarData)==0){
    return(NULL)
  }
  
  #****************
  # import packages
  #****************
  lapply(c("IBrokers",
           "TTR",
           "data.table",
           "dplyr"),
         checkpackages)
  
  #*****************
  # local parameters
  #*****************
  source(paste0(Working_Dir, "/Common_Parameters.R"))
  
  #*********************
  # simulation algorithm
  #*********************
  # define Orders_Transmitted
  Orders_Transmitted=data.table(Symbol=tail(BarData, 1)[, Symbol],
                                Submit_Time=tail(BarData, 1)[, Time],
                                Filled_Time=tail(BarData, 1)[, Time],
                                Action="",
                                Detail="",
                                TotalQuantity=0,
                                OrderType="MKT",
                                Price=0,
                                Filled=0,
                                Sigs_N=0)
  Orders_Transmitted=Orders_Transmitted[-1,]
  Time_Unit=BarData$Time[2]-BarData$Time[1]
  
  #*********************
  # calculate indicators
  #*********************
  Calculated_Indicators=Indicator_Calculator(BarData=BarData,
                                             Strategy_Indicators=Strategy_Indicators,
                                             Indicators=Indicators)
  
  #***********
  # fit models
  #***********
  Signals=Signal_Obtainer(Strategy_Models=Strategy_Models,
                          Models_Env=Models_Env,
                          Models=Models,
                          Strategy_Models_Class=Strategy_Models_Class,
                          Calculated_Indicators=Calculated_Indicators)
  
  Long_Signals=as.data.table(sapply(Strategy_Models,
                                    function(x){
                                      Signals[[x]][[1]]
                                    }))
  # Long_Signals[nrow(Long_Signals), ]=FALSE # this part is to not transfer orders at the very last time
  Short_Signals=as.data.table(sapply(Strategy_Models,
                                     function(x){
                                       Signals[[x]][[2]]
                                     }))
  # Short_Signals[nrow(Short_Signals), ]=FALSE # this part is to not transfer orders at the very last time
  
  Long_Signals=Long_Signals[, lapply(.SD, as.numeric)]
  Short_Signals=Short_Signals[, lapply(.SD, as.numeric)]
  Long_Signals=Long_Signals[, lapply(.SD, function(x){ifelse(is.na(x), 0, x)})]
  Short_Signals=Short_Signals[, lapply(.SD, function(x){ifelse(is.na(x), 0, x)})]
  
  Long_Signals_Sums=apply_row_sum_C(as.matrix(Long_Signals))
  Short_Signals_Sums=apply_row_sum_C(as.matrix(Short_Signals))
  
  #****************************
  # transmit order & fill order
  #****************************
  Long_Which_Signals=c()
  Long_Which_Signals_BTO=c()
  Long_Which_Signals_STC=c()
  
  Short_Which_Signals=c()
  Short_Which_Signals_STO=c()
  Short_Which_Signals_BTC=c()
  
  BuyToOpen_Signals=c()
  SellToOpen_Signals=c()
  SellToClose_Signals=c()
  BuyToClose_Signals=c()
  if("Long"%in%Position_Names){
    BuyToOpen_Min_Sig_N=as.numeric(Order_Rules[["Long"]][["BuyToOpen"]][["Min_Sig_N"]])
    # BuyToOpen_Min_Sig_N=1
    SellToClose_Min_Sig_N=as.numeric(Order_Rules[["Long"]][["SellToClose"]][["Min_Sig_N"]])
    
    BuyToOpen_Signals=Long_Signals_Sums>=BuyToOpen_Min_Sig_N
    BuyToOpen_Signals[length(BuyToOpen_Signals)]=FALSE # this part is to not transfer open orders at the very last time
    SellToClose_Signals=Short_Signals_Sums>=SellToClose_Min_Sig_N
    
    if(sum(BuyToOpen_Signals)>0){
      Long_Which_Signals_BTO=data.table(
        Ind=which(BuyToOpen_Signals),
        Time=BarData[BuyToOpen_Signals, Time],
        Signals=Long_Signals_Sums[which(BuyToOpen_Signals)],
        Action="Buy",
        Detail="BTO",
        Quantity=as.numeric(Order_Rules[["Long"]][["BuyToOpen"]][["Quantity"]])
      )
    }
    
    if(sum(SellToClose_Signals)>0){
      Long_Which_Signals_STC=data.table(
        Ind=which(SellToClose_Signals)[which(SellToClose_Signals)>=min(which(BuyToOpen_Signals))],
        Time=BarData[which(SellToClose_Signals)[which(SellToClose_Signals)>=min(which(BuyToOpen_Signals))], Time],
        Signals=Short_Signals_Sums[which(SellToClose_Signals)[which(SellToClose_Signals)>=min(which(BuyToOpen_Signals))]],
        Action="Sell",
        Detail="STC",
        Quantity=-as.numeric(Order_Rules[["Long"]][["SellToClose"]][["Quantity"]])
      )
    }
    
    Long_Which_Signals=rbind(Long_Which_Signals_BTO,
                             Long_Which_Signals_STC)
    
  }
  if("Short"%in%Position_Names){
    SellToOpen_Min_Sig_N=as.numeric(Order_Rules[["Short"]][["SellToOpen"]][["Min_Sig_N"]])
    # SellToOpen_Min_Sig_N=1
    BuyToClose_Min_Sig_N=as.numeric(Order_Rules[["Short"]][["BuyToClose"]][["Min_Sig_N"]])
    
    SellToOpen_Signals=Short_Signals_Sums>=SellToOpen_Min_Sig_N
    SellToOpen_Signals[length(SellToOpen_Signals)]=FALSE # this part is to not transfer open orders at the very last time
    BuyToClose_Signals=Long_Signals_Sums>=BuyToClose_Min_Sig_N
    
    if(sum(SellToOpen_Signals)>0){
      Short_Which_Signals_STO=data.table(
        Ind=which(SellToOpen_Signals),
        Time=BarData[SellToOpen_Signals, Time],
        Signals=Short_Signals_Sums[which(SellToOpen_Signals)],
        Action="Sell",
        Detail="STO",
        Quantity=-as.numeric(Order_Rules[["Short"]][["SellToOpen"]][["Quantity"]])
      )
    }
    
    if(sum(SellToOpen_Signals)>0 & sum(BuyToClose_Signals)>0){
      Short_Which_Signals_BTC=data.table(
        Ind=which(BuyToClose_Signals)[which(BuyToClose_Signals)>=min(which(SellToOpen_Signals))],
        Time=BarData[which(BuyToClose_Signals)[which(BuyToClose_Signals)>=min(which(SellToOpen_Signals))], Time],
        Signals=Long_Signals_Sums[which(BuyToClose_Signals)[which(BuyToClose_Signals)>=min(which(SellToOpen_Signals))]],
        Action="Buy",
        Detail="BTC",
        Quantity=as.numeric(Order_Rules[["Short"]][["BuyToClose"]][["Quantity"]])
      )
    }
    
    Short_Which_Signals=rbind(Short_Which_Signals_STO,
                              Short_Which_Signals_BTC)
    
  }
  
  if(sum(BuyToOpen_Signals)==0 &
     sum(SellToOpen_Signals)==0){
    return(list(Orders_Transmitted=NA,
                Ind_Profit=NA,
                Net_Profit=NA))
  }
  # if(sum(SellToClose_Signals)==0 &
  #    sum(BuyToClose_Signals)==0){
  #   return(list(Orders_Transmitted=NA,
  #               Ind_Profit=NA,
  #               Net_Profit=NA))
  # }
  
  Which_Signals=rbind(
    Long_Which_Signals,
    Short_Which_Signals
  )
  
  if(is.null(Which_Signals)){
    return(list(Orders_Transmitted=NA,
                Ind_Profit=NA,
                Net_Profit=NA))
  }
  
  Which_Signals[, Both_Direction:=duplicated(Which_Signals[["Ind"]], fromLast=T)|duplicated(Which_Signals[["Ind"]], fromLast=F)]
  Which_Signals=Which_Signals[order(Ind, Detail)] # make sure BTO comes ahead of STO given Both_Direction==TRUE
  
  #************
  # Market_Time
  # 1: both regular and pre-market trading time, 2: only regular trading time, 3: only pre-market trading time
  # last trading time prior to market close
  Which_Signals[, Submit_Time:=Time+Time_Unit]
  Which_Signals[, Submit_Time:=format(Submit_Time, tz="America/Los_Angeles")]
  Which_Signals[, Trading_Time:=format(as.POSIXct(Submit_Time), format="%H:%M:%S")]
  
  Last_Trading_Time=format(as.POSIXct(paste0("1970-01-01 ", Market_Close_Time))-Time_Unit, format="%H:%M:%S")
  
  if(length(Which_Signals[Detail=="BTO" |
                          Detail=="STO", Ind])==0){
    return(list(Orders_Transmitted=NA,
                Ind_Profit=NA,
                Net_Profit=NA))
  }
  Which_Signals=Which_Signals[Ind>=min(Which_Signals[Detail=="BTO" |
                                                       Detail=="STO", Ind]), ]
  
  # the first row must be either BTO or STO
  while(Which_Signals[1, Detail]=="BTC"|
        Which_Signals[1, Detail]=="STC"){
    Which_Signals=Which_Signals[-1, ]
  }
  
  # order by Time and Action such that BTO is prioritized over STO
  Which_Signals=Which_Signals[order(Time, Action)]
  
  #***************
  # Order_Filled_C
  Order_Filled_Results=Order_Filled_C(Which_Signals=Which_Signals,
                                      Max_Orders=Max_Orders)
  
  Which_Signals[, `:=`(Quantity=Order_Filled_Results$Quantity,
                       Net_Quantity=Order_Filled_Results$Net_Quantity,
                       Remove=Order_Filled_Results$Remove)]
  
  Which_Signals=Which_Signals[Remove==0, ]
  
  #*******************
  # Orders_Transmitter
  Orders_Transmitted=Orders_Transmitter(BarData,
                                        BarData_Include_Last_Time,
                                        Time_Unit,
                                        Penalty,
                                        Tick_Size,
                                        Which_Signals,
                                        Long_Signals_Sums,
                                        Short_Signals_Sums)
  
  # if there are orders that haven't been closed at the end, submit the closing positions at the end
  if(tail(Which_Signals[["Net_Quantity"]], 1)>0){
    Orders_Transmitted=rbind(Orders_Transmitted,
                             tail(Orders_Transmitted, 1))
    
    Orders_Transmitted[nrow(Orders_Transmitted), Submit_Time:=tail(BarData[["Time"]], 1)+Time_Unit]
    Orders_Transmitted[nrow(Orders_Transmitted), Filled_Time:=tail(BarData[["Time"]], 1)+Time_Unit]
    Orders_Transmitted[nrow(Orders_Transmitted), Action:="Sell"]
    Orders_Transmitted[nrow(Orders_Transmitted), Detail:="STC"]
    Orders_Transmitted[nrow(Orders_Transmitted), TotalQuantity:=abs(tail(Which_Signals[["Net_Quantity"]], 1))]
    Orders_Transmitted[nrow(Orders_Transmitted), Price:=tail(BarData_Include_Last_Time[["Open"]], 1)-Penalty*Tick_Size]
    Orders_Transmitted[nrow(Orders_Transmitted), Signs_N:=0] # this implies that Signs_N==0 is automatically filled to get rid of outstanding positions
  }else if(tail(Which_Signals[["Net_Quantity"]], 1)<0){
    Orders_Transmitted=rbind(Orders_Transmitted,
                             tail(Orders_Transmitted, 1))
    
    Orders_Transmitted[nrow(Orders_Transmitted), Submit_Time:=tail(BarData[["Time"]], 1)+Time_Unit]
    Orders_Transmitted[nrow(Orders_Transmitted), Filled_Time:=tail(BarData[["Time"]], 1)+Time_Unit]
    Orders_Transmitted[nrow(Orders_Transmitted), Action:="Buy"]
    Orders_Transmitted[nrow(Orders_Transmitted), Detail:="BTC"]
    Orders_Transmitted[nrow(Orders_Transmitted), TotalQuantity:=abs(tail(Which_Signals[["Net_Quantity"]], 1))]
    Orders_Transmitted[nrow(Orders_Transmitted), Price:=tail(BarData_Include_Last_Time[["Open"]], 1)+Penalty*Tick_Size]
    Orders_Transmitted[nrow(Orders_Transmitted), Signs_N:=0] # this implies that Signs_N==0 is automatically filled to get rid of outstanding positions
  }
  
  #**********************
  # calculate the balance
  #**********************
  Balance=Balance_Calculator(Orders_Transmitted)
  
  return(Balance)
}



# #*******************
# # checkBlotterUpdate
# #****************************************************************
# # a function in 'quantstrat' to check the update state of Blotter
# checkBlotterUpdate <- function(port.st = portfolio.st, 
#                                account.st = account.st, 
#                                verbose = TRUE) {
#   
#   ok <- TRUE
#   p <- getPortfolio(port.st)
#   a <- getAccount(account.st)
#   syms <- names(p$symbols)
#   port.tot <- sum(
#     sapply(
#       syms, 
#       FUN = function(x) eval(
#         parse(
#           text = paste("sum(p$symbols", 
#                        x, 
#                        "posPL.USD$Net.Trading.PL)", 
#                        sep = "$")))))
#   
#   port.sum.tot <- sum(p$summary$Net.Trading.PL)
#   
#   if(!isTRUE(all.equal(port.tot, port.sum.tot))) {
#     ok <- FALSE
#     if(verbose) print("portfolio P&L doesn't match sum of symbols P&L")
#   }
#   
#   initEq <- as.numeric(first(a$summary$End.Eq))
#   endEq <- as.numeric(last(a$summary$End.Eq))
#   
#   if(!isTRUE(all.equal(port.tot, endEq - initEq)) ) {
#     ok <- FALSE
#     if(verbose) print("portfolio P&L doesn't match account P&L")
#   }
#   
#   if(sum(duplicated(index(p$summary)))) {
#     ok <- FALSE
#     if(verbose)print("duplicate timestamps in portfolio summary")
#     
#   }
#   
#   if(sum(duplicated(index(a$summary)))) {
#     ok <- FALSE
#     if(verbose) print("duplicate timestamps in account summary")
#   }
#   return(ok)
# }


#****************
# Run_Backtesting
#************************************
# run Backtesting() given Market_Time
#************************************
Run_Backtesting=function(Market_Time,
                         BarData,
                         Trading_Dates,
                         Strategy_Name,
                         Working_Dir){
  
  switch(
    as.character(Market_Time),
    
    "1"={
      # both regular and pre-market trading time
      Time_Elapsed=system.time({
        Results=lapply(Trading_Dates,
                       #x=Trading_Dates[3]
                       function(x){
                         x=as.Date(x)
                         
                         BarData=BarData[Time>=paste0(x-1, " ", Market_Close_Time)&
                                           Time<paste0(x, " ", Market_Close_Time), ]
                         
                         BarData[, Ind:=.I]
                         
                         Backtesting(BarData=BarData,
                                     Strategy_Name=Strategy_Name,
                                     Working_Dir=Working_Dir)
                         
                       })
      })
    },
    
    "2"={
      # only regular trading time
      Time_Elapsed=system.time({
        Results=lapply(Trading_Dates,
                       #x=Trading_Dates[3]
                       function(x){
                         x=as.Date(x)
                         
                         BarData=BarData[Time>=paste0(x-1, " ", Market_Close_Time)&
                                           Time<paste0(x, " ", Market_Close_Time), ]
                         BarData=BarData[Time>=paste0(x, " ", Market_Open_Time)&
                                           Time<paste0(x, " ", Market_Close_Time), ]
                         
                         BarData[, Ind:=.I]
                         
                         Backtesting(BarData=BarData,
                                     Strategy_Name=Strategy_Name,
                                     Working_Dir=Working_Dir)
                       })
      })
    },
    
    "3"={
      # only pre-market trading time
      Time_Elapsed=system.time({
        Results=lapply(Trading_Dates,
                       #x=Trading_Dates[3]
                       function(x){
                         x=as.Date(x)
                         
                         BarData=BarData[Time>=paste0(x-1, " ", Market_Close_Time)&
                                           Time<paste0(x, " ", Market_Close_Time), ]
                         BarData=BarData[!(Time>=paste0(x, " ", Market_Open_Time)&
                                             Time<paste0(x, " ", Market_Close_Time)), ]
                         
                         BarData[, Ind:=.I]
                         
                         Backtesting(BarData=BarData,
                                     Strategy_Name=Strategy_Name,
                                     Working_Dir=Working_Dir)
                       })
      })
    }
  )
  
  return(
    list(Time_Elapsed=Time_Elapsed,
         Results=Results)
  )
  
}


#****************************
# Profit_Loss_Cut_Transmitted
#****************************
Profit_Loss_Cut_Transmitted=function(Orders_Transmitted, Next_BarData, Profit_Order, Stop_Order){
  N_Remaining_Orders=sum(Orders_Transmitted[["Action"]]=="Buy"&Orders_Transmitted[["Filled"]]==1)-
    sum(Orders_Transmitted[["Action"]]=="Sell"&Orders_Transmitted[["Filled"]]==1)
  
  Profit_Transmitted=c()
  Stop_Transmitted=c()
  if(N_Remaining_Orders>0){ # still on long
    
    Profit_Price=tail(Orders_Transmitted[Action=="Buy"&Filled==1], N_Remaining_Orders)[["Price"]][1]+Profit_Order
    Stop_Price=tail(Orders_Transmitted[Action=="Buy"&Filled==1], N_Remaining_Orders)[["Price"]][1]-Stop_Order
    
    Early_Profit_Ind=Profit_Price<Next_BarData[["High"]]
    Stop_Ind=Stop_Price>Next_BarData[["Low"]]
    
    if(is.na(Early_Profit_Ind)){
      Early_Profit_Ind=FALSE
    }
    if(is.na(Stop_Ind)){
      Stop_Ind=FALSE
    }
    
    if(Early_Profit_Ind){
      Profit_Transmitted=data.table(
        Symbol=Next_BarData[["Symbol"]],
        Submit_Time=Next_BarData[["Time"]],
        Filled_Time=Next_BarData[["Time"]],
        Action="Sell",
        Detail="Early_Profit",
        TotalQuantity=tail(Orders_Transmitted[Action=="Buy"&Filled==1][["TotalQuantity"]], N_Remaining_Orders),
        OrderType="MKT",
        Price=Profit_Price,
        Filled=1,
        Sigs_N=1
      )
    }
    
    if(Stop_Ind){
      Stop_Transmitted=data.table(
        Symbol=Next_BarData[["Symbol"]],
        Submit_Time=Next_BarData[["Time"]],
        Filled_Time=Next_BarData[["Time"]],
        Action="Sell",
        Detail="Stop",
        TotalQuantity=tail(Orders_Transmitted[Action=="Buy"&Filled==1][["TotalQuantity"]], N_Remaining_Orders),
        OrderType="MKT",
        Price=Stop_Price,
        Filled=1,
        Sigs_N=1
      )
    }
    
  }else if(N_Remaining_Orders<0){ # still on short
    Profit_Price=tail(Orders_Transmitted[Action=="Sell"&Filled==1], -N_Remaining_Orders)[["Price"]][1]-Profit_Order
    Stop_Price=tail(Orders_Transmitted[Action=="Sell"&Filled==1], -N_Remaining_Orders)[["Price"]][1]+Stop_Order
    
    Early_Profit_Ind=Profit_Price>Next_BarData[["Low"]]
    Stop_Ind=Stop_Price<Next_BarData[["High"]]
    
    if(is.na(Early_Profit_Ind)){
      Early_Profit_Ind=FALSE
    }
    if(is.na(Stop_Ind)){
      Stop_Ind=FALSE
    }
    
    
    if(Early_Profit_Ind){
      Profit_Transmitted=data.table(
        Symbol=Next_BarData[["Symbol"]],
        Submit_Time=Next_BarData[["Time"]],
        Filled_Time=Next_BarData[["Time"]],
        Action="Buy",
        Detail="Early_Profit",
        TotalQuantity=tail(Orders_Transmitted[Action=="Sell"&Filled==1][["TotalQuantity"]], -N_Remaining_Orders),
        OrderType="MKT",
        Price=Profit_Price,
        Filled=1,
        Sigs_N=1
      )
    }
    
    if(Stop_Ind){
      Stop_Transmitted=data.table(
        Symbol=Next_BarData[["Symbol"]],
        Submit_Time=Next_BarData[["Time"]],
        Filled_Time=Next_BarData[["Time"]],
        Action="Buy",
        Detail="Stop",
        TotalQuantity=tail(Orders_Transmitted[Action=="Sell"&Filled==1][["TotalQuantity"]], -N_Remaining_Orders),
        OrderType="MKT",
        Price=Stop_Price,
        Filled=1,
        Sigs_N=1
      )
    }
    
  }
  
  return(list(Profit_Transmitted=Profit_Transmitted,
              Stop_Transmitted=Stop_Transmitted))
  
}




#**************************
#
# [ --- Strategy --- ] ----
#
#**************************
# Init_Strategy
#*****************************************************************************
# initialize an environment with lists of parameters to function as a strategy
#*****************************************************************************
# Init_Strategy=function(Name){
#   Strategy_temp=list(Indicators=list(),
#                      Order_Rules=list(),
#                      Models=list()) # model parameters
#   class(Strategy_temp)="Strategy"
#   assign(Name,
#          Strategy_temp,
#          envir=.GlobalEnv)
# }
Init_Strategy=function(Name,
                       Max_Rows=50){
  env_temp=environment()
  class(env_temp)="Strategy"
  
  Indicators=list()
  Order_Rules=list()
  Models=list()
  
  assign(Name, env_temp, envir = .GlobalEnv)
  
  rm(list=c("Name", "env_temp"))
}

#**************
# Add_Indicator
#*****************************
# add an indicator to Strategy
Add_Indicator=function(Strategy,
                       Indicator=NULL,
                       IndicatorParams=NULL){
  
  # check TTR installation
  checkpackages("TTR")
  
  if(!exists(paste0(Strategy), envir=.GlobalEnv)){
    #Init.Strategy(Name=Strategy)
    stop(paste0("No strategy found named ", Strategy))
  }
  
  # TTR objects
  TTR_Objects=ls("package:TTR")
  
  # check the availability of Indicator in TTR
  if(Indicator=="Close"){
    New_IndicatorParams=TRUE
  }else{
    if(!Indicator%in%TTR_Objects){
      stop("Available indicators in TTR : ", paste(TTR_Objects, collapse=", "))
    }
    
    #*****************
    # check parameters
    #******************
    # names of passed arguments
    Passed_IndicatorParams_Names=names(IndicatorParams)
    
    # arguments/parameters in the function of Model in Models_Env
    TTR_Params=unlist(as.list(args(Indicator)))
    
    # names
    TTR_Params_Names=names(TTR_Params)
    
    # error if any of passed arguments is not defined in the function
    if(sum(!Passed_IndicatorParams_Names%in%TTR_Params_Names>0)){
      stop("Valid parameters : ", paste(TTR_Params_Names, collapse=", "))
    }
    
    
    #***********************************************
    # assign default values to unspecified arguments
    #***********************************************
    # arguments/parameters with defined default values
    TTR_Params_with_Default=TTR_Params[TTR_Params!=""]
    TTR_Params_with_Default_Names=names(TTR_Params_with_Default)
    
    New_IndicatorParams=c(IndicatorParams,
                          TTR_Params_with_Default[!TTR_Params_with_Default_Names%in%Passed_IndicatorParams_Names])
    
    # New_Passed_IndicatorParams_Names
    New_Passed_IndicatorParams_Names=names(New_IndicatorParams)
    
    # error if any of passed arguments is not defined in the function
    if(sum(!New_Passed_IndicatorParams_Names%in%TTR_Params_Names>0)){
      stop("Valid parameters : ", paste(TTR_Params_Names, collapse=", "))
    }
    
    # sort the arguments (not important)
    Ordered_Arguments=New_Passed_IndicatorParams_Names[order(match(New_Passed_IndicatorParams_Names, TTR_Params_Names))]
    New_IndicatorParams=New_IndicatorParams[Ordered_Arguments]
  }
  
  # add an indicator to the corresponding strategy
  Strategy_temp=get(Strategy, envir=.GlobalEnv)
  Strategy_temp$Indicators[[Indicator]]=New_IndicatorParams
  assign(paste0(Strategy), Strategy_temp, envir=.GlobalEnv)
}





#**********
# Add_Model
#************************
# add a model to Strategy
Add_Model=function(Strategy,
                   Model=NULL,
                   Model_Name=NULL,
                   ModelParams=NULL){
  if(!exists(paste0(Strategy), envir=.GlobalEnv)){
    #Init.Strategy(Name=Strategy)
    stop(paste0("No strategy found named ", Strategy))
  }
  
  # available models
  Available_Models=ls(Models_Env)
  
  # check the availability of Model in Models_Env
  if(!Model%in%Available_Models){
    stop("Available models : ", paste(Available_Models, collapse=", "))
  }
  
  # pull the info for Model
  Model_Info=Models_Env[[Model]]
  
  
  # check required indicators
  Strategy_temp=get(Strategy, envir=.GlobalEnv)
  Essential_Indicators=Model_Info$Essential_Indicators
  Excluded_Indicators=Essential_Indicators[!Essential_Indicators%in%names(Strategy_temp[["Indicators"]])]
  if(length(Excluded_Indicators)>0){
    stop("Add necessary indicators : ", paste(Excluded_Indicators, collapse=", "))
  }
  
  #*****************
  # check parameters
  #**************************
  # names of passed arguments
  Passed_Arguments_Names=names(ModelParams)
  
  # arguments/parameters in the function of Model in Models_Env
  Model_Params=unlist(as.list(args(Model_Info$Function)))
  
  # names
  Model_Params_Names=names(Model_Params)
  
  # error if any of passed arguments is not defined in the function
  if(sum(!Passed_Arguments_Names%in%Model_Params_Names>0)){
    stop("Valid parameters : ", paste(Model_Params_Names, collapse=", "))
  }
  
  
  #***********************************************
  # assign default values to unspecified arguments
  #***********************************************
  # arguments/parameters with defined default values
  Model_Params_with_Default=Model_Params[Model_Params!=""]
  Model_Params_with_Default_Names=names(Model_Params_with_Default)
  
  New_ModelParams=c(ModelParams,
                    Model_Params_with_Default[!Model_Params_with_Default_Names%in%Passed_Arguments_Names])
  
  # New_Passed_Arguments_Names
  New_Passed_Arguments_Names=names(New_ModelParams)
  
  # sort the arguments (not important)
  Ordered_Arguments=New_Passed_Arguments_Names[order(match(New_Passed_Arguments_Names, Model_Params_Names))]
  New_ModelParams=New_ModelParams[Ordered_Arguments]
  
  #*************
  # assign class
  #*************
  if(is.null(Model_Name)){
    stop("name the model please (Model_Name)")
  }
  class(New_ModelParams)=Model
  
  # add a model to the corresponding strategy
  Strategy_temp=get(Strategy, envir=.GlobalEnv)
  Strategy_temp$Models[[Model_Name]]=New_ModelParams
  assign(paste0(Strategy), Strategy_temp, envir=.GlobalEnv)
}




#**************
# Add_OrderRule
#******************************
# add an order rule to Strategy
Add_OrderRule=function(Strategy,
                       OrderRule=NULL,
                       OrderRuleParams=NULL){
  #
  lapply(c("data.table", "dplyr", "plyr"), checkpackages)
  
  if(!exists(paste0(Strategy), envir=.GlobalEnv)){
    #Init.Strategy(Name=Strategy)
    stop(paste0("No strategy found named ", Strategy))
  }
  
  # available Orders
  Available_OrderRules=ls(OrderRules_Env)
  
  # check the availability of OrderRule in OrderRules_Env
  if(!OrderRule%in%Available_OrderRules){
    stop("Available Orders : ", paste(Available_OrderRules, collapse=", "))
  }
  
  # pull the info for OrderRule
  OrderRules_Info=OrderRules_Env[[OrderRule]]
  
  #********************
  # check element names
  #********************
  OrderRules_Info=as.relistable(OrderRules_Info)
  All.default.elements=unlist(OrderRules_Info)
  #
  if(is.null(OrderRuleParams)){
    OrderRuleParams=OrderRules_Info
  }
  
  OrderRuleParams=as.relistable(OrderRuleParams)
  All.passed.elements=unlist(OrderRuleParams)
  #
  All.default.elements.concat.names=names(All.default.elements)
  All.passed.elements.concat.names=names(All.passed.elements)
  #
  All.default.elements.names=unique(unlist(strsplit(All.default.elements.concat.names, "[.]")))
  All.passed.elements.names=unique(unlist(strsplit(All.passed.elements.concat.names, "[.]")))
  if(sum(!All.passed.elements.names%in%All.default.elements.names)>0){
    stop("Invalid parameters entered : ",
         paste(All.passed.elements.names[!All.passed.elements.names%in%All.default.elements.names], collapse=", "),
         
         "\n
         Valid parameters : ",
         paste(All.default.elements.names, collapse=", "))
  }
  
  Passed.string.split=strsplit(All.passed.elements.concat.names, "[.]")
  
  Default.string.split=strsplit(All.default.elements.concat.names, "[.]")
  
  # depth
  # rbind.fill(lapply(Passed.string.split,
  #                   function(y){as.data.frame(t(y))}))
  # rbind.fill(lapply(Default.string.split,
  #                   function(y){as.data.frame(t(y))}))
  
  #****************************
  # compare values at each node
  #****************************
  comparing.table=data.table(Default=names(All.default.elements),
                             Value=All.default.elements) %>%
    left_join(data.table(Default=names(All.passed.elements),
                         Value=All.passed.elements),
              by="Default")
  
  # locations of elements with different values
  diff.loc=which(comparing.table$Value.x!=comparing.table$Value.y)
  
  # update the values in the default list
  for(ui in diff.loc){
    All.default.elements[ui]=comparing.table[ui, Value.y]
  }
  
  # 
  New_ModelParams=relist(All.default.elements)
  #New_ModelParams=as.data.table(New_ModelParams) %>% as.list()
  
  # add a model to the corresponding strategy
  Strategy_temp=get(Strategy, envir=.GlobalEnv)
  
  # Min_Sig_N must be smaller or equal to the number of models
  if(OrderRule=="Long"){
    if((length(names(Strategy_temp$Models))<New_ModelParams[["BuyToOpen"]][["Min_Sig_N"]])|
       (length(names(Strategy_temp$Models))<New_ModelParams[["SellToClose"]][["Min_Sig_N"]])){
      stop("Min_Sig_N must be smaller or equal to the number of models")
    }
  }
  if(OrderRule=="Short"){
    if((length(names(Strategy_temp$Models))<New_ModelParams[["SellToOpen"]][["Min_Sig_N"]])|
       (length(names(Strategy_temp$Models))<New_ModelParams[["BuyToClose"]][["Min_Sig_N"]])){
      stop("Min_Sig_N must be smaller or equal to the number of models")
    }
  }
  
  Strategy_temp$Order_Rules[[OrderRule]]=New_ModelParams
  assign(paste0(Strategy), Strategy_temp, envir=.GlobalEnv)
}





#******************************
#
# [ --- Live trading --- ] ----
#
#******************************
# ReqRealTimeBars
#**************************
# request realtime bar data
#*******************************************
# output : BarData in the global environment
# return New_Data=1 if the new data is derived; and 0 if not
ReqRealTimeBars=function(BarSize=5,
                         Log=F){
  # New_Data : 1 if the new bar data with the size of interest is added in BarData; and 0 if not
  New_Data=0
  
  # if BarSize is not a multiple of 5
  if(BarSize%%5!=0){
    message("BarSize must be a multiple of 5.")
    # round off BarSize to an integer
    BarSize<<-round(BarSize, -1)
    message(paste0("So, it is rounded off ", get("BarSize", envir=.GlobalEnv), "."))
  }
  
  #****************************************************************
  # RealTimeBarData is stored temporarily in the global environment
  #*************************************************************************************************
  # -> once RealTimeBarData is available, reqRealTimeBars is executed every 5 seconds automatically, 
  # which I think is a built-in functionality in twsCALLBACK_cust
  # -> nope... sometimes data is extracted faster than every 5 seconds
  # -> so, the code is modified to echo out the new data only when it is added
  #***************************************************************************
  # print("pre-IBrokers::reqRealTimeBars")
  IBrokers::reqRealTimeBars(tws, contract, barSize="5", useRTH=F,
                            eventWrapper=eWrapper_cust(),
                            CALLBACK=twsCALLBACK_cust)
  # .twsIncomingMSG
  # print("Post-IBrokers::reqRealTimeBars")
  # if it fails to create RealTimeBarData
  if(!exists("RealTimeBarData")){
    # print("RealTimeBarData doesn't exist")
    Sys.sleep(0.5) # suspend execution for a while to prevent the system from breaking
    return(New_Data) # terminate the algorithm by retunning New_Data
  }
  
  # initial Recent_RealTimeBarData
  if(!exists("Recent_RealTimeBarData")){
    # print("Recent_RealTimeBarData doesn't exist")
    if(!exists("Archiv")){
      print("Data has not been archived yet")
      print(paste0("waiting for the starting time of the next bar"))
    }
    Recent_RealTimeBarData<<-RealTimeBarData
    Sys.sleep(0.5) # suspend execution for a while to prevent the system from breaking
  }
  
  # if BarSize=5, no additional process is required
  if(BarSize==5){
    # if RealTimeBarData is not the new data
    if(!is.null(BarData) & sum(Recent_RealTimeBarData!=RealTimeBarData)==0){
      # remove RealTimeBarData at the end of everytime iteration
      rm(RealTimeBarData, envir=.GlobalEnv)
      
      return(New_Data) # if the new data is not derived, terminate the algorithm by retunning New_Data
    }
    
    # update BarData
    BarData<<-rbind(BarData, RealTimeBarData)
    
    New_Data=1
  }
  
  # if BarSize>5 and it is a multiple of 5 (BarSize%%5==0 is already taken into account in advance)
  if(BarSize>5){
    # if RealTimeBarData is not the new data
    if(exists("Archiv") & sum(Recent_RealTimeBarData[, -"Symbol"]==RealTimeBarData[, -"Symbol"])==ncol(Recent_RealTimeBarData[, -"Symbol"])){
      # remove RealTimeBarData at the end of everytime iteration
      rm(RealTimeBarData, envir=.GlobalEnv)
      # print("Recent_RealTimeBarData == RealTimeBarData")
      Sys.sleep(0.5) # suspend execution for a while to prevent the system from breaking
      return(New_Data) # if the new data is not derived, terminate the algorithm by returning New_Data=0
    }
    
    # open info
    if(as.numeric(RealTimeBarData$Time)%%BarSize==0){
      # define the Archive indicator
      Archiv<<-1
      # print("open info ; Archiv==1")
      
      Symbol<<-RealTimeBarData$Symbol
      Time<<-RealTimeBarData$Time
      Open<<-RealTimeBarData$Open
      High<<-RealTimeBarData$High
      Low<<-RealTimeBarData$Low
      Volume<<-RealTimeBarData$Volume
      Wap<<-RealTimeBarData$Wap # Wap is useless for BarSize>5
      Count<<-RealTimeBarData$Count
    }
    
    # if Archiv hasn't been defined yet, no need to proceed further
    if(!exists("Archiv", envir=.GlobalEnv)){
      return(New_Data)
    }
    
    # interim info (update High, Low, Volum, and Count)
    if(as.numeric(RealTimeBarData$Time)%%BarSize>0){
      # print("interim info")
      
      High<<-max(High, RealTimeBarData$High)
      Low<<-min(Low, RealTimeBarData$Low)
      Volume<<-Volume+RealTimeBarData$Volume
      Count<<-Count+RealTimeBarData$Count
    }
    
    # close info
    if(as.numeric(RealTimeBarData$Time)%%BarSize==(BarSize-5)){
      # remove the Archive indicator
      rm(Archiv, envir=.GlobalEnv)
      # print("close info ; rm(Archiv)")
      
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
                               Wap=Wap,
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
  
  # if the new data is added in BarData
  if(New_Data==1){
    # echo the updated data
    print(tail(BarData, 1))
    
    # write log everytime new data is added
    if(Log==T){
      if(file.exists(paste0(working.dir, "Log/Live_Trading_Log.csv"))){
        Log=data.table(Time=Sys.time())
        Log=rbind(Log,
                  fread(paste0(working.dir, "Log/Live_Trading_Log.csv")))
        fwrite(Log, paste0(working.dir, "Log/Live_Trading_Log.csv"))
      }else{
        Log=data.table(Time=Sys.time())
        fwrite(Log, paste0(working.dir, "Log/Live_Trading_Log.csv"))
      }
    }
  }
  
  # save Recent_RealTimeBarData
  Recent_RealTimeBarData<<-RealTimeBarData
  
  # remove RealTimeBarData at the end of everytime iteration
  rm(RealTimeBarData, envir=.GlobalEnv)
  
  return(New_Data) # terminate the algorithm by retunning New_Data
  
}





#*********
# snapShot
#*********#
# an example function provided from the interactive brokers' API archives
snapShot = function(twsCon,
                    eWrapper,
                    timestamp,
                    file,
                    playback = 1,
                    ...)
{
  if (missing(eWrapper))
    eWrapper <- eWrapper()
  names(eWrapper$.Data$data) <- eWrapper$.Data$symbols
  con <- twsCon[[1]]
  while (TRUE) {
    socketSelect(list(con), FALSE, NULL)
    curMsg <- .Internal(readBin(con, "character", 1L,
                                NA_integer_, TRUE, FALSE))
    if (!is.null(timestamp)) {
      processMsg(curMsg,
                 con,
                 eWrapper,
                 format(Sys.time(),
                        timestamp),
                 file,
                 ...)
    }
    else {
      processMsg(curMsg, con, eWrapper, timestamp,
                 file, ...)
    }
    if (!any(sapply(eWrapper$.Data$data, is.na)))
      return(do.call(rbind, lapply(eWrapper$.Data$data,
                                   as.data.frame)))
  }
}





#*****************
# twsCALLBACK_cust
#*************************
# customized twsCALLBACK()
# revised lines are highlighted with # # # #
#*****************
# twsCALLBACK_cust
#*************************
# customized twsCALLBACK()
# revised lines are highlighted with # # # #
twsCALLBACK_cust=function (twsCon, eWrapper, timestamp, file, playback = 1, ...) 
{
  if (missing(eWrapper)) 
    eWrapper <- eWrapper()
  con <- twsCon[[1]]
  if (inherits(twsCon, "twsPlayback")) {
    sys.time <- NULL
    while (TRUE) {
      if (!is.null(timestamp)) {
        last.time <- sys.time
        sys.time <- as.POSIXct(strptime(paste(readBin(con, 
                                                      character(), 2), collapse = " "), timestamp))
        if (!is.null(last.time)) {
          #Sys.sleep((sys.time - last.time) * playback)
          Sys.sleep((0.1) * playback)
        }
        curMsg <- readBin(con, character(), 1)
        if (length(curMsg) < 1) 
          next
        processMsg(curMsg, con, eWrapper, format(sys.time, 
                                                 timestamp), file, ...)
      }
      else {
        curMsg <- readBin(con, character(), 1)
        if (length(curMsg) < 1) 
          next
        processMsg(curMsg, con, eWrapper, timestamp, 
                   file, ...)
        if (curMsg == .twsIncomingMSG$REAL_TIME_BARS) 
          #Sys.sleep(5 * playback)
          Sys.sleep(0.1 * playback)
      }
    }
  }
  else {
    rep.ind=1 # # # # for snapshot
    tryCatch(while (isConnected(twsCon) & rep.ind==1) { # # # #
      if (!socketSelect(list(con), FALSE, 0.25)) {
        next
      }
      curMsg <- readBin(con, "character", 1L)
      # curMsg=NULL # # # #
      # print(paste0("con : ", con, " / curMsg : ", curMsg, " / timestamp : ", timestamp)) # # # #
      if(is.null(curMsg) # # # # sometime curMsg is not derived
      ){
        curMsg=50
      }else if(curMsg!=50){
        Sys.sleep(0.5) # suspend execution for a while to prevent the system from breaking
      }
      
      if (!is.null(timestamp)) {
        processMsg(curMsg, con, eWrapper, format(Sys.time(), 
                                                 timestamp), file, twsCon, ...)
      }
      else {
        processMsg(curMsg, con, eWrapper, timestamp, 
                   file, twsCon, ...)
      }
      rep.ind=0 # # # #
    }, error = function(e) {
      close(twsCon)
      print(e)
      stop("IB connection error. Connection closed", 
           call. = FALSE)
    })
  }
}




#**************
# eWrapper_cust
#**********************
# customized eWrapper()
# revised lines (realtimeBars) are highlighted with #
# eWrapper_cust=function (debug = FALSE, errfile = stderr())
# {
#   .Data <- new.env()
#   get.Data <- function(x) get(x, .Data)
#   assign.Data <- function(x, value) assign(x, value, .Data)
#   remove.Data <- function(x) remove(x, .Data)
#   if (is.null(debug)) {
#     errorMessage <- function(curMsg, msg, timestamp, file,
#                              twsconn, ...) {
#       cat(msg, "\n", file = errfile)
#     }
#     tickPrice <- tickSize <- tickOptionComputation <- tickGeneric <- tickString <- tickEFP <- orderStatus <- openOrder <- openOrderEnd <- updateAccountValue <- updateAccountTime <- updatePortfolio <- accountDownloadEnd <- nextValidId <- contractDetails <- bondContractDetails <- contractDetailsEnd <- execDetails <- execDetailsEnd <- updateMktDepth <- updateMktDepthL2 <- updateNewsBulletin <- managedAccounts <- receiveFA <- historicalData <- scannerParameters <- scannerData <- scannerDataEnd <- realtimeBars <- currentTime <- fundamentalData <- deltaNeutralValidation <- tickSnapshotEnd <- function(curMsg,
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           msg, timestamp, file, ...) {
#       c(curMsg, msg)
#     }
#   }
#   else if (!debug) {
#     tickPrice <- function(curMsg, msg, timestamp, file,
#                           ...) {
#       symbols <- get.Data("symbols")
#       # e_tick_price(NULL, msg, timestamp, file, symbols,
#       #              ...)
#     }
#     tickSize <- function(curMsg, msg, timestamp, file, ...) {
#       symbols <- get.Data("symbols")
#       # e_tick_size(NULL, msg, timestamp, file, symbols,
#       #             ...)
#     }
#     tickOptionComputation <- function(curMsg, msg, timestamp,
#                                       file, ...) {
#       symbols <- get.Data("symbols")
#       # e_tick_option(NULL, msg, timestamp, file, symbols,
#       #               ...)
#     }
#     tickGeneric <- function(curMsg, msg, timestamp, file,
#                             ...) {
#       symbols <- get.Data("symbols")
#       # e_tick_generic(NULL, msg, timestamp, file, symbols,
#       #                ...)
#     }
#     tickString <- function(curMsg, msg, timestamp, file,
#                            ...) {
#       symbols <- get.Data("symbols")
#       # e_tick_string(NULL, msg, timestamp, file, symbols,
#       #               ...)
#     }
#     tickEFP <- function(curMsg, msg, timestamp, file, ...) {
#       symbols <- get.Data("symbols")
#       # e_tick_EFP(NULL, msg, timestamp, file, symbols,
#       #            ...)
#     }
#     orderStatus <- function(curMsg, msg, timestamp, file,
#                             ...) {
#       # e_order_status(curMsg, msg)
#       c(curMsg, msg)
#     }
#     errorMessage <- function(curMsg, msg, timestamp, file,
#                              twsconn, ...) {
#       if (msg[3] == "1100")
#         twsconn$connected <- FALSE
#       if (msg[3] %in% c("1101", "1102"))
#         twsconn$connected <- TRUE
#       # cat("TWS Message:", msg, "\n")
#     }
#     openOrder <- function(curMsg, msg, timestamp, file,
#                           ...) {
#       c(curMsg, msg)
#     }
#     openOrderEnd <- function(curMsg, msg, timestamp, file,
#                              ...) {
#       c(curMsg, msg)
#     }
#     updateAccountValue <- function(curMsg, msg, timestamp,
#                                    file, ...) {
#       c(curMsg, msg)
#     }
#     updatePortfolio <- function(curMsg, msg, timestamp,
#                                 file, ...) {
#       # e_portfolio_value(curMsg, msg)
#       c(curMsg, msg)
#     }
#     updateAccountTime <- function(curMsg, msg, timestamp,
#                                   file, ...) {
#       c(curMsg, msg)
#     }
#     accountDownloadEnd <- function(curMsg, msg, timestamp,
#                                    file, ...) {
#       c(curMsg, msg)
#     }
#     nextValidId <- function(curMsg, msg, timestamp, file,
#                             ...) {
#       c(curMsg, msg)
#     }
#     contractDetails <- function(curMsg, msg, timestamp,
#                                 file, ...) {
#       c(curMsg, msg)
#     }
#     bondContractDetails <- function(curMsg, msg, timestamp,
#                                     file, ...) {
#       c(curMsg, msg)
#     }
#     contractDetailsEnd <- function(curMsg, msg, timestamp,
#                                    file, ...) {
#       c(curMsg, msg)
#     }
#     execDetails <- function(curMsg, msg, timestamp, file,
#                             ...) {
#       # e_execDetails(curMsg, msg, file, ...)
#     }
#     execDetailsEnd <- function(curMsg, msg, timestamp, file,
#                                ...) {
#       c(curMsg, msg)
#     }
#     updateMktDepth <- function(curMsg, msg, timestamp, file,
#                                ...) {
#       symbols <- get.Data("symbols")
#       # e_update_mkt_depth(NULL, msg, timestamp, file, symbols,
#       #                    ...)
#     }
#     updateMktDepthL2 <- function(curMsg, msg, timestamp,
#                                  file, ...) {
#       symbols <- get.Data("symbols")
#       # e_update_mkt_depthL2(NULL, msg, timestamp, file,
#       #                      symbols, ...)
#     }
#     updateNewsBulletin <- function(curMsg, msg, timestamp,
#                                    file, ...) {
#       cat("newsMsgId: ", msg[2], "newsMsgType: ", msg[3],
#           "newsMessage: ", msg[4], "origExch:", msg[5],
#           "\n")
#       c(curMsg, msg)
#     }
#     managedAccounts <- function(curMsg, msg, timestamp,
#                                 file, ...) {
#       c(curMsg, msg)
#     }
#     receiveFA <- function(curMsg, msg, timestamp, file,
#                           ...) {
#       c(curMsg, msg)
#     }
#     historicalData <- function(curMsg, msg, timestamp, file,
#                                ...) {
#       c(curMsg, msg)
#     }
#     scannerParameters <- function(curMsg, msg, timestamp,
#                                   file, ...) {
#       # cat(msg <- rawToChar(msg[-which(msg == as.raw(0))]))
#       c(curMsg, msg)
#     }
#     scannerData <- function(curMsg, reqId, rank, contract,
#                             distance, benchmark, projection, legsStr) {
#       # e_scannerData(curMsg, reqId, rank, contract, distance,
#       #               benchmark, projection, legsStr)
#     }
#     scannerDataEnd <- function(curMsg, msg, timestamp, file,
#                                ...) {
#       c(curMsg, msg)
#     }
#     realtimeBars <- function(curMsg, msg, timestamp, file,
#                              ...) {
#       symbols <- get.Data("symbols")
#       
#       `e_real_time_bars_dup` <- function(curMsg, msg, symbols, file, ...) {
#         # msg[1] is VERSION
#         columns <- c("Symbol","Time","Open","High","Low","Close","Volume",
#                      "Wap","Count")
#         id <- as.numeric(msg[2])
#         file <- file[[id]]
#         msg[2] <- symbols[as.numeric(msg[2])]
#         msg[3] <- strftime(structure(as.numeric(msg[3]), class=c("POSIXt","POSIXct")))
#         
#         #************
#         # edited part
#         #***************************
#         Data=matrix(msg[-1], nrow=1)
#         colnames(Data)=columns
#         Data=as.data.table(Data)
#         
#         #Corrected timezone
#         Adj_Time=as.POSIXct(format(as.POSIXct(Data$Time),
#                                    tz="America/Los_Angeles"),
#                             tz="America/Los_Angeles") # fix the timezone to PDT
#         
#         Data[, Time:=NULL]
#         Data[, Time:=Adj_Time]
#         Data[, (columns[!columns%in%c("Symbol", "Time")]):=lapply(.SD, as.numeric), .SDcols=columns[!columns%in%c("Symbol", "Time")]]
#         
#         setcolorder(Data, columns)
#         
#         RealTimeBarData<<-as.data.table(Data) # generate RealTimeBarData in the global environment
#         #************************************
#         
#       }
#       #**************
#       # original code
#       #**************
#       # `e_real_time_bars` <- function(curMsg, msg, symbols, file, ...) {
#       #   # msg[1] is VERSION
#       #   columns <- c("Id","time","open","high","low","close","volume",
#       #                "wap","count")
#       #   id <- as.numeric(msg[2])
#       #   file <- file[[id]]
#       #   msg[2] <- symbols[as.numeric(msg[2])]
#       #   msg[3] <- strftime(structure(as.numeric(msg[3]), class=c("POSIXt","POSIXct")))
#       #   cat(paste(columns,"=",msg[-1],sep=""),'\n',file=file,append=TRUE)
#       # }
#       
#       
#       e_real_time_bars_dup(curMsg, msg, symbols, file, ...) # # # #
#     }
#     currentTime <- function(curMsg, msg, timestamp, file,
#                             ...) {
#       c(curMsg, msg)
#     }
#     fundamentalData <- function(curMsg, msg, timestamp,
#                                 file, ...) {
#       # e_fundamentalData(curMsg, msg)
#     }
#     deltaNeutralValidation <- function(curMsg, msg, timestamp,
#                                        file, ...) {
#       c(curMsg, msg)
#     }
#     tickSnapshotEnd <- function(curMsg, msg, timestamp,
#                                 file, ...) {
#       c(curMsg, msg)
#     }
#   }
#   else {
#     tickPrice <- tickSize <- tickOptionComputation <- tickGeneric <- tickString <- tickEFP <- orderStatus <- openOrder <- openOrderEnd <- updateAccountValue <- updateAccountTime <- updatePortfolio <- accountDownloadEnd <- nextValidId <- contractDetails <- bondContractDetails <- contractDetailsEnd <- execDetails <- execDetailsEnd <- updateMktDepth <- updateMktDepthL2 <- updateNewsBulletin <- managedAccounts <- receiveFA <- historicalData <- scannerParameters <- scannerData <- scannerDataEnd <- realtimeBars <- currentTime <- fundamentalData <- deltaNeutralValidation <- tickSnapshotEnd <- function(curMsg,
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           msg, timestamp, file, ...) {
#       cat(as.character(timestamp), curMsg, msg, "\n",
#           file = file[[1]], append = TRUE, ...)
#     }
#     errorMessage <- function(curMsg, msg, timestamp, file,
#                              twsconn, ...) {
#       cat(as.character(timestamp), curMsg, msg, "\n",
#           file = file[[1]], append = TRUE, ...)
#     }
#     
#   }
#   eW <- list(.Data = .Data, get.Data = get.Data, assign.Data = assign.Data,
#              remove.Data = remove.Data, tickPrice = tickPrice, tickSize = tickSize,
#              #tickOptionComputation = tickOptionComputation,
#              tickGeneric = tickGeneric,
#              tickString = tickString, tickEFP = tickEFP, orderStatus = orderStatus,
#              errorMessage = errorMessage, openOrder = openOrder,
#              openOrderEnd = openOrderEnd, updateAccountValue = updateAccountValue,
#              updatePortfolio = updatePortfolio, updateAccountTime = updateAccountTime,
#              accountDownloadEnd = accountDownloadEnd, nextValidId = nextValidId,
#              contractDetails = contractDetails, bondContractDetails = bondContractDetails,
#              contractDetailsEnd = contractDetailsEnd, execDetails = execDetails,
#              execDetailsEnd = execDetailsEnd, updateMktDepth = updateMktDepth,
#              updateMktDepthL2 = updateMktDepthL2, updateNewsBulletin = updateNewsBulletin,
#              managedAccounts = managedAccounts, receiveFA = receiveFA,
#              historicalData = historicalData, scannerParameters = scannerParameters,
#              scannerData = scannerData, scannerDataEnd = scannerDataEnd,
#              realtimeBars = realtimeBars, currentTime = currentTime,
#              fundamentalData = fundamentalData, deltaNeutralValidation = deltaNeutralValidation,
#              tickSnapshotEnd = tickSnapshotEnd)
#   class(eW) <- "eWrapper"
#   invisible(eW)
# }
eWrapper_cust=function (debug = FALSE, errfile = stderr())
{
  .Data <- new.env()
  get.Data <- function(x) get(x, .Data)
  assign.Data <- function(x, value) assign(x, value, .Data)
  remove.Data <- function(x) remove(x, .Data)
  if (is.null(debug)) {
    errorMessage <- function(curMsg, msg, timestamp, file,
                             twsconn, ...) {
      cat(msg, "\n", file = errfile)
    }
    tickPrice <- tickSize <- tickOptionComputation <- tickGeneric <- tickString <- tickEFP <- orderStatus <- openOrder <- openOrderEnd <- updateAccountValue <- updateAccountTime <- updatePortfolio <- accountDownloadEnd <- nextValidId <- contractDetails <- bondContractDetails <- contractDetailsEnd <- execDetails <- execDetailsEnd <- updateMktDepth <- updateMktDepthL2 <- updateNewsBulletin <- managedAccounts <- receiveFA <- historicalData <- scannerParameters <- scannerData <- scannerDataEnd <- realtimeBars <- currentTime <- fundamentalData <- deltaNeutralValidation <- tickSnapshotEnd <- function(curMsg,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
  }
  else if (!debug) {
    errorMessage <- function(curMsg, msg, timestamp, file,
                             twsconn, ...) {
      if (msg[3] == "1100")
        twsconn$connected <- FALSE
      if (msg[3] %in% c("1101", "1102"))
        twsconn$connected <- TRUE
      # cat("TWS Message:", msg, "\n")
    }
    realtimeBars <- function(curMsg, msg, timestamp, file,
                             ...) {
      symbols <- get.Data("symbols")
      
      `e_real_time_bars_dup` <- function(curMsg, msg, symbols, file, ...) {
        # msg[1] is VERSION
        columns <- c("Symbol","Time","Open","High","Low","Close","Volume",
                     "Wap","Count")
        id <- as.numeric(msg[2])
        file <- file[[id]]
        msg[2] <- symbols[as.numeric(msg[2])]
        msg[3] <- strftime(structure(as.numeric(msg[3]), class=c("POSIXt","POSIXct")))
        
        #************
        # edited part
        #***************************
        Data=matrix(msg[-1], nrow=1)
        colnames(Data)=columns
        Data=as.data.table(Data)
        
        #Corrected timezone
        Adj_Time=as.POSIXct(format(as.POSIXct(Data$Time),
                                   tz="America/Los_Angeles"),
                            tz="America/Los_Angeles") # fix the timezone to PDT
        
        Data[, Time:=NULL]
        Data[, Time:=Adj_Time]
        Data[, (columns[!columns%in%c("Symbol", "Time")]):=lapply(.SD, as.numeric), .SDcols=columns[!columns%in%c("Symbol", "Time")]]
        
        setcolorder(Data, columns)
        
        RealTimeBarData<<-as.data.table(Data) # generate RealTimeBarData in the global environment
        #************************************
        
      }
      e_real_time_bars_dup(curMsg, msg, symbols, file, ...) # # # #
    }
  }
  else {
    errorMessage <- function(curMsg, msg, timestamp, file,
                             twsconn, ...) {
      cat(as.character(timestamp), curMsg, msg, "\n",
          file = file[[1]], append = TRUE, ...)
    }
    
  }
  eW <- list(.Data = .Data, get.Data = get.Data, assign.Data = assign.Data,
             remove.Data = remove.Data,
             errorMessage = errorMessage,
             realtimeBars = realtimeBars)
  class(eW) <- "eWrapper"
  invisible(eW)
}




#*********************
# processMsg_openorder
#**********************
# customized processMsg()
# revised lines (realtimeBars) are highlighted with #
processMsg_openorder=function (curMsg, con, eWrapper, timestamp, file, twsconn, ...) 
{
  if (curMsg == .twsIncomingMSG$TICK_PRICE) {
    msg <- readBin(con, "character", 6)
    eWrapper$tickPrice(curMsg, msg, timestamp, file, ...)
  }
  else if (curMsg == .twsIncomingMSG$TICK_SIZE) {
    msg <- readBin(con, "character", 4)
    eWrapper$tickSize(curMsg, msg, timestamp, file, ...)
  }
  else if (curMsg == .twsIncomingMSG$ORDER_STATUS) {
    msg <- readBin(con, "character", 11)
    eWrapper$orderStatus(curMsg, msg, timestamp, file, ...)
  }
  else if (curMsg == .twsIncomingMSG$ERR_MSG) {
    msg <- readBin(con, "character", 4)
    eWrapper$errorMessage(curMsg, msg, timestamp, file, 
                          twsconn, ...)
  }
  else if (curMsg == .twsIncomingMSG$OPEN_ORDER) {
    msg <- readBin(con, "character", 84)
    eWrapper$openOrder(curMsg, msg, timestamp, file, ...)
  }
  else if (curMsg == .twsIncomingMSG$ACCT_VALUE) {
    msg <- readBin(con, "character", 5)
    eWrapper$updateAccountValue(curMsg, msg, timestamp, 
                                file, ...)
  }
  else if (curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) {
    msg <- readBin(con, "character", 18)
    eWrapper$updatePortfolio(curMsg, msg, timestamp, file, 
                             ...)
  }
  else if (curMsg == .twsIncomingMSG$ACCT_UPDATE_TIME) {
    msg <- readBin(con, "character", 2)
    eWrapper$updateAccountTime(curMsg, msg, timestamp, file, 
                               ...)
  }
  else if (curMsg == .twsIncomingMSG$NEXT_VALID_ID) {
    msg <- readBin(con, "character", 2)
    eWrapper$nextValidId(curMsg, msg, timestamp, file, ...)
  }
  else if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {
    msg <- readBin(con, "character", 28)
    eWrapper$contractDetails(curMsg, msg, timestamp, file, 
                             ...)
  }
  else if (curMsg == .twsIncomingMSG$EXECUTION_DATA) {
    msg <- readBin(con, "character", 24)
    eWrapper$execDetails(curMsg, msg, timestamp, file, ...)
  }
  else if (curMsg == .twsIncomingMSG$MARKET_DEPTH) {
    msg <- readBin(con, "character", 7)
    eWrapper$updateMktDepth(curMsg, msg, timestamp, file, 
                            ...)
  }
  else if (curMsg == .twsIncomingMSG$MARKET_DEPTH_L2) {
    msg <- readBin(con, "character", 8)
    eWrapper$updateMktDepthL2(curMsg, msg, timestamp, file, 
                              ...)
  }
  else if (curMsg == .twsIncomingMSG$NEWS_BULLETINS) {
    msg <- readBin(con, "character", 5)
    eWrapper$newsBulletins(curMsg, msg, timestamp, file, 
                           ...)
  }
  else if (curMsg == .twsIncomingMSG$MANAGED_ACCTS) {
    msg <- readBin(con, "character", 2)
    eWrapper$managedAccounts(curMsg, msg, timestamp, file, 
                             ...)
  }
  else if (curMsg == .twsIncomingMSG$RECEIVE_FA) {
    msg <- readBin(con, "character", 2)
    stop("xml data currently unsupported")
    eWrapper$receiveFA(curMsg, msg, timestamp, file, ...)
  }
  else if (curMsg == .twsIncomingMSG$HISTORICAL_DATA) {
    header <- readBin(con, character(), 5)
    nbin <- as.numeric(header[5]) * 9
    msg <- readBin(con, character(), nbin)
    eWrapper$historicalData(curMsg, msg, timestamp, file, 
                            ...)
  }
  else if (curMsg == .twsIncomingMSG$BOND_CONTRACT_DATA) {
    warning("BOND_CONTRACT_DATA unimplemented as of yet")
    eWrapper$bondContractDetails(curMsg, msg, timestamp, 
                                 file, ...)
  }
  else if (curMsg == .twsIncomingMSG$SCANNER_PARAMETERS) {
    version <- readBin(con, character(), 1L)
    msg <- readBin(con, raw(), 1000000L)
    eWrapper$scannerParameters(curMsg, msg, timestamp, file, 
                               ...)
  }
  else if (curMsg == .twsIncomingMSG$SCANNER_DATA) {
    cD <- twsContractDetails()
    version <- readBin(con, character(), 1L)
    tickerId <- readBin(con, character(), 1L)
    numberOfElements <- as.integer(readBin(con, character(), 
                                           1L))
    for (i in 1:numberOfElements) {
      msg <- readBin(con, character(), 16L)
      rank <- msg[1]
      cD$contract$conId <- msg[2]
      cD$contract$symbol <- msg[3]
      cD$contract$sectype <- msg[4]
      cD$contract$expiry <- msg[5]
      cD$contract$strike <- msg[6]
      cD$contract$right <- msg[7]
      cD$contract$exch <- msg[8]
      cD$contract$currency <- msg[9]
      cD$contract$local <- msg[10]
      cD$marketName <- msg[11]
      cD$tradingClass <- msg[12]
      distance <- msg[13]
      benchmark <- msg[14]
      projection <- msg[15]
      legsStr <- msg[16]
      eWrapper$scannerData(curMsg, tickerId, rank, cD, 
                           distance, benchmark, projection, legsStr)
    }
  }
  else if (curMsg == .twsIncomingMSG$TICK_OPTION_COMPUTATION) {
    msg <- readBin(con, "character", 11)
    eWrapper$tickOptionComputation(curMsg, msg, timestamp, 
                                   file, ...)
  }
  else if (curMsg == .twsIncomingMSG$TICK_GENERIC) {
    msg <- readBin(con, "character", 4)
    eWrapper$tickGeneric(curMsg, msg, timestamp, file, ...)
  }
  else if (curMsg == .twsIncomingMSG$TICK_STRING) {
    msg <- readBin(con, "character", 4)
    eWrapper$tickString(curMsg, msg, timestamp, file, ...)
  }
  else if (curMsg == .twsIncomingMSG$TICK_EFP) {
    msg <- readBin(con, "character", 10)
    eWrapper$tickEFP(curMsg, msg, timestamp, file, ...)
  }
  else if (curMsg == .twsIncomingMSG$CURRENT_TIME) {
    msg <- readBin(con, "character", 2)
    eWrapper$currentTime(curMsg, msg, timestamp, file, ...)
  }
  else if (curMsg == .twsIncomingMSG$REAL_TIME_BARS) {
    msg <- readBin(con, "character", 10)
    # eWrapper$realtimeBars(curMsg, msg, timestamp, file, #####
    #                       ...)
  }
  else if (curMsg == .twsIncomingMSG$FUNDAMENTAL_DATA) {
    msg <- readBin(con, "character", 3)
    eWrapper$fundamentalData(curMsg, msg, timestamp, file, 
                             ...)
  }
  else if (curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
    msg <- readBin(con, "character", 2)
    eWrapper$contractDetailsEnd(curMsg, msg, timestamp, 
                                file, ...)
  }
  else if (curMsg == .twsIncomingMSG$OPEN_ORDER_END) {
    msg <- readBin(con, "character", 1)
    eWrapper$openOrderEnd(curMsg, msg, timestamp, file, 
                          ...)
  }
  else if (curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END) {
    msg <- readBin(con, "character", 2)
    eWrapper$accountDownloadEnd(curMsg, msg, timestamp, 
                                file, ...)
  }
  else if (curMsg == .twsIncomingMSG$EXECUTION_DATA_END) {
    msg <- readBin(con, "character", 2)
    eWrapper$execDetailsEnd(curMsg, msg, timestamp, file, 
                            ...)
  }
  else if (curMsg == .twsIncomingMSG$DELTA_NEUTRAL_VALIDATION) {
    msg <- readBin(con, "character", 5)
    eWrapper$deltaNeutralValidation(curMsg, msg, timestamp, 
                                    file, ...)
  }
  else if (curMsg == .twsIncomingMSG$TICK_SNAPSHOT_END) {
    msg <- readBin(con, "character", 2)
    eWrapper$tickSnapshotEnd(curMsg, msg, timestamp, file, 
                             ...)
  }
  else {
    warning(paste("Unknown incoming message: ", curMsg, 
                  ". Please reset connection", sep = ""), call. = FALSE)
  }
}





#********************
# .reqOpenOrders_cust
#********************
# customized .reqOpenOrders()
# revised lines (realtimeBars) are highlighted with #
.reqOpenOrders_cust=function(twsconn) {
  if( !is.twsConnection(twsconn))
    stop('requires twsConnection object')
  
  con <- twsconn[[1]]
  
  VERSION <- "1"
  
  writeBin(c(.twsOutgoingMSG$REQ_OPEN_ORDERS,VERSION), con)
}





#*****************
# reqopenorders_cb
#*****************
reqopenorders_cb=function(twsconn) {
  .reqOpenOrders_cust(twsconn)
  
  open_orders=list()
  con <- twsconn[[1]]
  eW  <- eWrapper()
  
  while(TRUE) {
    socketSelect(list(con), FALSE, NULL)
    curMsg <- readBin(con, character(), 1L)
    
    if(curMsg == .twsIncomingMSG$OPEN_ORDER){
      open_orders[[length(open_orders)+1]]=processMsg_openorder(curMsg, con, eW,
                                                                timestamp=NULL,file="")
    } else {
      processMsg_openorder(curMsg, con, eW,timestamp=NULL,file="")
    }
    if(curMsg == .twsIncomingMSG$OPEN_ORDER_END)
      break
  }
  return(open_orders)
}

#*****************
# Initiate_BarData
#*****************
# generate BarData for barsize of 30 seconds or larger in an attempt to reduce the preparation time
Initiate_BarData=function(BarSize=60,
                          Ignore_Prep=FALSE, # if FALSE, immediately request historical bar data of the given bar size
                          Prep_Seconds=10){  # if Ignore_Prep==TRUE, request historical bar data of the given bar size 'Prep_Seconds' before the starting time of the next bar
  if(BarSize>=30){
    # Legal barSize settings are technically '1 secs', '5 secs', '15 secs', '30 mins', '1 min', '2 mins', 
    # '3 mins','5 mins', '15 mins', '30 mins', '1 hour', '1 day', '1 week', '1 month' ,'3 months', and '1 year'.
    # They must be specified exactly and there is no guarantee from the API that all will work for all
    # securities or durations
    if(BarSize==30){
      BarSize_txt="30 secs"
    }else if(BarSize==60){
      BarSize_txt="1 min"
    }else if(BarSize==60*5){
      BarSize_txt="5 mins"
    }else if(BarSize==60*15){
      BarSize_txt="15 mins"
    }else if(BarSize==60*30){
      BarSize_txt="30 mins"
    }else if(BarSize==60*60){
      BarSize_txt="1 hour"
    }
    
    if(Ignore_Prep==FALSE){
      # wait for the initial time of the current Barsize
      if((BarSize-round(as.numeric(Sys.time())%%BarSize))-Prep_Seconds>0){
        print(paste0("wait for the initial time to request historical data of the given Barsize"))
        while((BarSize-round(as.numeric(Sys.time())%%BarSize))-Prep_Seconds>0){
          print(paste0("remaining time : ", (BarSize-round(as.numeric(Sys.time())%%BarSize))-10, " second(s)"))
          Sys.sleep(1) # suspend execution for a while to prevent the system from breaking
        }
      }
    }
    
    # reqHistoricalData_Temp
    print("request historical data from TWS")
    useRTH_temp="0"
    Duration_D=1+ceiling(Max_Rows/((22.5*60*60)/BarSize)) # the market is open 22.5 hours on a regular day
    reqHistoricalData_Temp=reqHistoricalData(conn=tws,
                                             Contract=contract,
                                             barSize=BarSize_txt,
                                             duration=paste0(Duration_D, " D"),
                                             useRTH=useRTH_temp) %>% tail(Max_Rows)
    
    # reqHistoricalData_Temp_Colnames
    reqHistoricalData_Temp_Colnames=colnames(reqHistoricalData_Temp)
    
    # BarData
    BarData=data.table(
      Symbol=contract$symbol,
      Time=index(reqHistoricalData_Temp),
      Open=reqHistoricalData_Temp[, grep("Open", reqHistoricalData_Temp_Colnames)],
      High=reqHistoricalData_Temp[, grep("High", reqHistoricalData_Temp_Colnames)],
      Low=reqHistoricalData_Temp[, grep("Low", reqHistoricalData_Temp_Colnames)],
      Close=reqHistoricalData_Temp[, grep("Close", reqHistoricalData_Temp_Colnames)],
      Volume=reqHistoricalData_Temp[, grep("Volume", reqHistoricalData_Temp_Colnames)],
      Wap=reqHistoricalData_Temp[, grep("WAP", reqHistoricalData_Temp_Colnames)],
      Count=reqHistoricalData_Temp[, grep("Count", reqHistoricalData_Temp_Colnames)]
    )
    
    colnames(BarData)=c("Symbol", "Time", "Open", "High", "Low", "Close", "Volume", "Wap", "Count")
  }else{
    BarData=c()
  }
  
  return(BarData)
}



#*************
# twsExecution
#*************
# I don't remember why I included this function, but let's keep it just in case.
# Nonetheleses, this function doesn't seem to have an important role to play.
twsExecution <- 
  function(orderId,
           clientId,
           execId,
           time,
           acctNumber,
           exchange,
           side,
           shares,
           price,
           permId,
           liquidation,
           cumQty,
           avgPrice,
           orderRef,
           evRule,
           evMultiplier) {
    
    # special constructor if called with no args
    if(is.null(names(match.call()[-1])))
      return(do.call('twsExecution', rep(list(NULL),16)))
    
    structure(list(orderId=orderId,
                   clientId=clientId,
                   execId=execId,
                   time=time,
                   acctNumber=acctNumber,
                   exchange=exchange,
                   side=side,
                   shares=shares,
                   price=price,
                   permId=permId,
                   liquidation=liquidation,
                   cumQty=cumQty,
                   avgPrice=avgPrice,
                   orderRef=orderRef,
                   evRule=evRule,
                   evMultiplier=evMultiplier),
              class="twsExecution")
    
  }



#***************
# readExecutions
#***************
# New utility function. Call immediately after a reqExecutions() call.
# I don't remember why I included this function, but let's keep it just in case.
# Nonetheleses, this function doesn't seem to have an important role to play.
readExecutions <- function(twsconn) {
  # .reqOpenOrders(twsconn)
  con <- twsconn[[1]]
  eW <- eWrapper()
  while (TRUE) {
    socketSelect(list(con), FALSE, NULL)
    curMsg <- readBin(con, character(), 1L)
    processMsg(curMsg, con, eW)
    if (curMsg == .twsIncomingMSG$EXECUTION_DATA_END) break
  }
}




#**********************
#
# [ --- Rcpp --- ] ----
#
#**********************
# Order_Filled
#*************
# c++ code
Order_Filled=\(){}
lapply("Rcpp", checkpackages)
if(Device=="desktop"){
  # desktop
  sourceCpp("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/Rcpp/Order_Filled_C.cpp")
}else if(Device=="laptop"){
  # laptop
  sourceCpp("C:/Users/jchoi02/Desktop/R/Stock_Analysis/Rcpp/Order_Filled_C.cpp")
}

#***************
# Order_Filled_R
#***************
Order_Filled_R=function(Which_Signals, Max_Orders){
  Ind_=Which_Signals[["Ind"]]
  Action_=Which_Signals[["Action"]]
  Detail_=Which_Signals[["Detail"]]
  Quantity_=Which_Signals[["Quantity"]]
  Both_Direction_=Which_Signals[["Both_Direction"]]
  
  Net_Quantity_=rep(0, nrow(Which_Signals))
  # Net_Quantity_[1]=Quantity_[1]
  Remove_=rep(0, nrow(Which_Signals))
  Both_Direction_Ind=0
  
  # begin orders with an open position
  # thus, indicate removal in Remove_[i] for closing positions until the first open position
  ind=1
  while_i=0
  while(while_i==0){
    if(Detail_[ind]=="BTC" |
       Detail_[ind]=="STC"){
      Net_Quantity_[ind]=0
      Remove_[ind]=1
    }else{
      while_i=1
      Net_Quantity_[ind]=Quantity_[ind]
    }
    ind=ind+1
  }
  
  for(i in ind:nrow(Which_Signals)){
    #i=2
    # Quantity_[i]=-4
    # Net_Quantity_[i-1]=0
    
    # adjust Quantity[i]
    {
      if((Net_Quantity_[i-1]<0)&
         (Net_Quantity_[i-1]+Quantity_[i]>0)&
         (Net_Quantity_[i-1]+Quantity_[i]<=Max_Orders)){
        Quantity_[i]=-Net_Quantity_[i-1]
      }else if((Net_Quantity_[i-1])>0&
               (Net_Quantity_[i-1]+Quantity_[i]<0)&
               (Net_Quantity_[i-1]+Quantity_[i]>=-Max_Orders)){
        Quantity_[i]=-Net_Quantity_[i-1]
      }
      
      if(abs(Net_Quantity_[i-1]+Quantity_[i])>Max_Orders){
        if(Detail_[i]=="BTC"|
           Detail_[i]=="STC"){
          if(Quantity_[i]<0){
            Quantity_[i]=max(Quantity_[i], -(Max_Orders))
          }else if(Quantity_[i]>=0){
            Quantity_[i]=min(Quantity_[i], Max_Orders)
          }
        }else{
          if(Quantity_[i]<0){
            Quantity_[i]=max(Quantity_[i], -(Max_Orders+Net_Quantity_[i-1]))
          }else if(Quantity_[i]>=0){
            Quantity_[i]=min(Quantity_[i], Max_Orders-Net_Quantity_[i-1])
          }
        }
      }
    }
    
    # if abs(Net_Quantity_[i-1])>=Max_Orders
    if(abs(Net_Quantity_[i-1])>=Max_Orders){
      if(Detail_[i]=="BTO"|
         Detail_[i]=="STO"){
        Net_Quantity_[i]=Net_Quantity_[i-1]
        Remove_[i]=1
        
        next
      }
      
      switch(as.character(Both_Direction_[i]),
             
             "TRUE"={
               # indicate removal in Remove_[i] for orders that occur at the same time in both directions (long & short)
               # such duplicated orders are removed from the 2nd one (the very 1st one is processed by the next for statment)
               if(Both_Direction_Ind==Ind_[i]){
                 Net_Quantity_[i]=Net_Quantity_[i-1]
                 Remove_[i]=1
                 
                 next
               }
               
               # Always first try to clear the existing positions
               if((Net_Quantity_[i-1]>0&Detail_[i]=="STC")|
                  (Net_Quantity_[i-1]<0&Detail_[i]=="BTC")){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i] # update Both_Direction_Ind after the 1st uplicated order is recorded
                 
                 next
               }
             },
             
             "FALSE"={
               if((Net_Quantity_[i-1]>0&Detail_[i]=="STC")|
                  (Net_Quantity_[i-1]<0&Detail_[i]=="BTC")){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 next
               }
             })
      
      Net_Quantity_[i]=Net_Quantity_[i-1]
      
      Remove_[i]=1
      
      next
    }
    
    
    # if(Net_Quantity_[i-1]>0)
    if(Net_Quantity_[i-1]>0){
      
      switch(as.character(Both_Direction_[i]),
             
             "TRUE"={
               # indicate removal in Remove_[i] for orders that occur at the same time in both directions (long & short)
               # such duplicated orders are removed from the 2nd one (the very 1st one is processed by the next for statment)
               if(Both_Direction_Ind==Ind_[i]){
                 Net_Quantity_[i]=Net_Quantity_[i-1]
                 Remove_[i]=1
                 
                 next
               }
               
               # Always first try to clear the existing positions
               if(Detail_[i]=="STC"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i] # update Both_Direction_Ind after the 1st uplicated order is recorded
                 
                 next
               }
               
               if(Detail_[i]=="BTO"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i] # update Both_Direction_Ind after the 1st uplicated order is recorded
                 
                 next
               }
             },
             
             "FALSE"={
               if(Detail_[i]=="BTO"|
                  Detail_[i]=="STC"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 
                 next
               }
             })
      
      Net_Quantity_[i]=Net_Quantity_[i-1]
      
      Remove_[i]=1
      
      next
    }
    
    
    # Net_Quantity_[i-1]<0
    if(Net_Quantity_[i-1]<0){
      
      switch(as.character(Both_Direction_[i]),
             
             "TRUE"={
               # indicate removal in Remove_[i] for orders that occur at the same time in both directions (long & short)
               # such duplicated orders are removed from the 2nd one (the very 1st one is processed by the next for statment)
               if(Both_Direction_Ind==Ind_[i]){
                 Net_Quantity_[i]=Net_Quantity_[i-1]
                 Remove_[i]=1
                 
                 next
               }
               
               # Always first try to clear the existing positions
               if(Detail_[i]=="BTC"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i] # update Both_Direction_Ind after the 1st uplicated order is recorded
                 
                 next
               }
               
               if(Detail_[i]=="STO"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i] # update Both_Direction_Ind after the 1st uplicated order is recorded
                 
                 next
               }
             },
             
             "FALSE"={
               if(Detail_[i]=="BTC"|
                  Detail_[i]=="STO"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 
                 next
               }
             })
      
      Net_Quantity_[i]=Net_Quantity_[i-1]
      
      Remove_[i]=1
      
      next
    }
    
    
    # Net_Quantity_[i-1]==0
    if(Net_Quantity_[i-1]==0){
      
      switch(as.character(Both_Direction_[i]),
             
             "TRUE"={
               # indicate removal in Remove_[i] for orders that occur at the same time in both directions (long & short)
               # such duplicated orders are removed from the 2nd one (the very 1st one is processed by the next for statment)
               if(Both_Direction_Ind==Ind_[i]){
                 Net_Quantity_[i]=Net_Quantity_[i-1]
                 Remove_[i]=1
                 
                 next
               }
               
               # This part allows to force the long position entrance when there is no position filled yet while Sigs_N indicates to enter both positions at the same time
               if(Detail_[i]=="BTO"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i] # update Both_Direction_Ind after the 1st uplicated order is recorded
                 
                 next
                 
               }else if(Detail_[i]=="STO"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 Both_Direction_Ind=Ind_[i] # update Both_Direction_Ind after the 1st uplicated order is recorded
                 
                 next
               }
             },
             
             "FALSE"={
               if(Detail_[i]=="BTO"|
                  Detail_[i]=="STO"){
                 Net_Quantity_[i]=Net_Quantity_[i-1]+Quantity_[i]
                 
                 next
               }
             })
      
      Net_Quantity_[i]=Net_Quantity_[i-1]
      
      Remove_[i]=1
      
      next
    }
  }
  
  
  return(
    list(
      Quantity=as.integer(Quantity_),
      Net_Quantity=as.integer(Net_Quantity_),
      Remove=as.integer(Remove_)
    )
  )
}


#****************
# apply_row_sum_C
#****************
apply_row_sum_C=\(){}
# cppFunction('NumericVector apply_row_sum_C(NumericMatrix x){
#   int n=x.nrow();
#   NumericVector output(n);
#   
#   for(int i=0; i<n; i++){
#       output[i]=sum(x(i, _));
#   }
#   return(output);
# }')
if(Device=="desktop"){
  # desktop
  sourceCpp("C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/Rcpp/apply_row_sum_C.cpp")
}else if(Device=="laptop"){
  # laptop
  sourceCpp("C:/Users/jchoi02/Desktop/R/Stock_Analysis/Rcpp/apply_row_sum_C.cpp")
}

#**********************************
#
# [ --- Common functions --- ] ----
#
#**********************************
# Calculated_Indicators
#**********************
Indicator_Calculator=function(BarData,
                              Strategy_Indicators,
                              Indicators){
  #*********************
  # calculate indicators
  Calculated_Indicators=lapply(Strategy_Indicators,
                               function(x)
                                 if(x=="Close"){
                                   BarData[["Close"]]
                                 }else{
                                   if(x=="BBands" & nrow(BarData)>=Indicators[[x]][['n']]+1){ # BBands : n-1, RSI : n+1
                                     do.call(x, 
                                             c(list(BarData[["Close"]]), # for now only using "Close price", additional work would be required in the future if the indicator does not depend on "Close price"
                                               Indicators[[x]]))
                                   }else if(x!="BBands" & nrow(BarData)>Indicators[[x]][['n']]+1){
                                     do.call(x, 
                                             c(list(BarData[["Close"]]), # for now only using "Close price", additional work would be required in the future if the indicator does not depend on "Close price"
                                               Indicators[[x]]))
                                   }
                                 }
  )
  
  # Calculated_Indicators=Calculated_Indicators[-which(sapply(Calculated_Indicators, is.null))]
  names(Calculated_Indicators)=Strategy_Indicators
  
  return(Calculated_Indicators)
}


#****************
# Signal_Obtainer
#****************
Signal_Obtainer=function(Strategy_Models,
                         Models_Env,
                         Models,
                         Strategy_Models_Class,
                         Calculated_Indicators){
  #***********
  # fit models
  Signals=as.data.table(sapply(Strategy_Models,
                               function(x){
                                 Model_Info=Models_Env[[Strategy_Models_Class[x]]] # variables and functions defined for the model object
                                 Calculated_Indicators_Combined=do.call(cbind, Calculated_Indicators) # combined Calculated_Indicators
                                 Calculated_Indicators_Names=names(Calculated_Indicators)[unlist(lapply(Calculated_Indicators, function(x) !is.null(x)))] #
                                 if(sum(!Model_Info[["Essential_Indicators"]]%in%Calculated_Indicators_Names)==0){ # if none of essential indicators hasn't been calculated in Calculated_Indicators, proceed to run the model
                                   do.call(Model_Info[["Function"]],
                                           c(list(Calculated_Indicators_Combined),
                                             Models[[x]]))}
                               }))
  
  return(Signals)
}


#*******************
# Orders_Transmitter
#*******************
Orders_Transmitter=function(BarData,
                            BarData_Include_Last_Time,
                            Time_Unit,
                            Penalty,
                            Tick_Size,
                            Which_Signals,
                            Long_Signals_Sums,
                            Short_Signals_Sums){
  
  BTO_Orders=Which_Signals[Which_Signals[["Detail"]]=="BTO", ]
  STC_Orders=Which_Signals[Which_Signals[["Detail"]]=="STC", ]
  STO_Orders=Which_Signals[Which_Signals[["Detail"]]=="STO", ]
  BTC_Orders=Which_Signals[Which_Signals[["Detail"]]=="BTC", ]
  
  BarData[, Time:=as.POSIXct(Time)]
  if(sum(c("Long", "Short")%in%Position_Names)==2){
    # Orders_Transmitted
    Orders_Transmitted=rbind(
      # buy to open
      if(nrow(BTO_Orders)>0){
        data.table(
          Symbol=BarData[Ind%in%BTO_Orders[, Ind], Symbol],
          Submit_Time=BarData[Ind%in%BTO_Orders[, Ind], Time]+Time_Unit,
          Filled_Time=BarData[Ind%in%BTO_Orders[, Ind], Time]+Time_Unit,
          Action="Buy",
          Detail="BTO",
          TotalQuantity=BTO_Orders[, Quantity],
          OrderType=Order_Rules[["Long"]][["BuyToOpen"]][["OrderType"]],
          Price=BarData_Include_Last_Time[Ind%in%c(BTO_Orders[, Ind]+1)][["Open"]]+Penalty*Tick_Size,
          Filled=1,
          Signs_N=Long_Signals_Sums[BTO_Orders[, Ind]],
          Row_N=1:nrow(BTO_Orders)
        )
      },
      
      # sell to close
      if(nrow(STC_Orders)>0){
        data.table(
          Symbol=BarData[Ind%in%STC_Orders[, Ind], Symbol],
          Submit_Time=BarData[Ind%in%STC_Orders[, Ind], Time]+Time_Unit,
          Filled_Time=BarData[Ind%in%STC_Orders[, Ind], Time]+Time_Unit,
          Action="Sell",
          Detail="STC",
          TotalQuantity=-STC_Orders[, Quantity],
          OrderType=Order_Rules[["Long"]][["SellToClose"]][["OrderType"]],
          Price=BarData_Include_Last_Time[Ind%in%c(STC_Orders[, Ind]+1)][["Open"]]-Penalty*Tick_Size,
          Filled=1,
          Signs_N=Short_Signals_Sums[STC_Orders[, Ind]],
          Row_N=1:nrow(STC_Orders)
        )
      },
      
      # sell to open
      if(nrow(STO_Orders)>0){
        data.table(
          Symbol=BarData[Ind%in%STO_Orders[, Ind], Symbol],
          Submit_Time=BarData[Ind%in%STO_Orders[, Ind], Time]+Time_Unit,
          Filled_Time=BarData[Ind%in%STO_Orders[, Ind], Time]+Time_Unit,
          Action="Sell",
          Detail="STO",
          TotalQuantity=-STO_Orders[, Quantity],
          OrderType=Order_Rules[["Short"]][["SellToOpen"]][["OrderType"]],
          Price=BarData_Include_Last_Time[Ind%in%c(STO_Orders[, Ind]+1)][["Open"]]-Penalty*Tick_Size,
          Filled=1,
          Signs_N=Short_Signals_Sums[STO_Orders[, Ind]],
          Row_N=1:nrow(STO_Orders)
        )
      },
      
      # buy to close
      if(nrow(BTC_Orders)>0){
        data.table(
          Symbol=BarData[Ind%in%BTC_Orders[, Ind], Symbol],
          Submit_Time=BarData[Ind%in%BTC_Orders[, Ind], Time]+Time_Unit,
          Filled_Time=BarData[Ind%in%BTC_Orders[, Ind], Time]+Time_Unit,
          Action="Buy",
          Detail="BTC",
          TotalQuantity=BTC_Orders[, Quantity],
          OrderType=Order_Rules[["Short"]][["BuyToClose"]][["OrderType"]],
          Price=BarData_Include_Last_Time[Ind%in%c(BTC_Orders[, Ind]+1)][["Open"]]+Penalty*Tick_Size,
          Filled=1,
          Signs_N=Long_Signals_Sums[BTC_Orders[, Ind]],
          Row_N=1:nrow(BTC_Orders)
        )
      }
    )
  }else if(sum(c("Long", "Short")%in%Position_Names)==1 &
           "Long"%in%Position_Names){
    
    # Orders_Transmitted
    Orders_Transmitted=rbind(
      # buy to open
      if(nrow(BTO_Orders)>0){
        data.table(
          Symbol=BarData[Ind%in%BTO_Orders[, Ind], Symbol],
          Submit_Time=BarData[Ind%in%BTO_Orders[, Ind], Time]+Time_Unit,
          Filled_Time=BarData[Ind%in%BTO_Orders[, Ind], Time]+Time_Unit,
          Action="Buy",
          Detail="BTO",
          TotalQuantity=BTO_Orders[, Quantity],
          OrderType=Order_Rules[["Long"]][["BuyToOpen"]][["OrderType"]],
          Price=BarData_Include_Last_Time[Ind%in%c(BTO_Orders[, Ind]+1)][["Open"]]+Penalty*Tick_Size,
          Filled=1,
          Signs_N=Long_Signals_Sums[BTO_Orders[, Ind]],
          Row_N=1:nrow(BTO_Orders)
        )
      },
      
      # sell to close
      if(nrow(STC_Orders)>0){
        data.table(
          Symbol=BarData[Ind%in%STC_Orders[, Ind], Symbol],
          Submit_Time=BarData[Ind%in%STC_Orders[, Ind], Time]+Time_Unit,
          Filled_Time=BarData[Ind%in%STC_Orders[, Ind], Time]+Time_Unit,
          Action="Sell",
          Detail="STC",
          TotalQuantity=-STC_Orders[, Quantity],
          OrderType=Order_Rules[["Long"]][["SellToClose"]][["OrderType"]],
          Price=BarData_Include_Last_Time[Ind%in%c(STC_Orders[, Ind]+1)][["Open"]]-Penalty*Tick_Size,
          Filled=1,
          Signs_N=Short_Signals_Sums[STC_Orders[, Ind]],
          Row_N=1:nrow(STC_Orders)
        )
      }
    )
  }else if(sum(c("Long", "Short")%in%Position_Names)==1 &
           "Short"%in%Position_Names){
    
    # Orders_Transmitted
    Orders_Transmitted=rbind(
      # sell to open
      if(nrow(STO_Orders)>0){
        data.table(
          Symbol=BarData[Ind%in%STO_Orders[, Ind], Symbol],
          Submit_Time=BarData[Ind%in%STO_Orders[, Ind], Time]+Time_Unit,
          Filled_Time=BarData[Ind%in%STO_Orders[, Ind], Time]+Time_Unit,
          Action="Sell",
          Detail="STO",
          TotalQuantity=-STO_Orders[, Quantity],
          OrderType=Order_Rules[["Short"]][["SellToOpen"]][["OrderType"]],
          Price=BarData_Include_Last_Time[Ind%in%c(STO_Orders[, Ind]+1)][["Open"]]-Penalty*Tick_Size,
          Filled=1,
          Signs_N=Short_Signals_Sums[STO_Orders[, Ind]],
          Row_N=1:nrow(STO_Orders)
        )
      },
      
      # buy to close
      if(nrow(BTC_Orders)>0){
        data.table(
          Symbol=BarData[Ind%in%BTC_Orders[, Ind], Symbol],
          Submit_Time=BarData[Ind%in%BTC_Orders[, Ind], Time]+Time_Unit,
          Filled_Time=BarData[Ind%in%BTC_Orders[, Ind], Time]+Time_Unit,
          Action="Buy",
          Detail="BTC",
          TotalQuantity=BTC_Orders[, Quantity],
          OrderType=Order_Rules[["Short"]][["BuyToClose"]][["OrderType"]],
          Price=BarData_Include_Last_Time[Ind%in%c(BTC_Orders[, Ind]+1)][["Open"]]+Penalty*Tick_Size,
          Filled=1,
          Signs_N=Long_Signals_Sums[BTC_Orders[, Ind]],
          Row_N=1:nrow(BTC_Orders)
        )
      }
    )
  }
  Orders_Transmitted=Orders_Transmitted[order(Submit_Time, Row_N), ]
  
  return(Orders_Transmitted)
}


#*******************
# Balance_Calculator
#*******************
Balance_Calculator=function(Orders_Transmitted){
  Collapse_Orders_Transmitted=cbind(Orders_Transmitted[Action=="Buy", 
                                                       c("Filled_Time", "Price")],
                                    Orders_Transmitted[Action=="Sell", 
                                                       c("Filled_Time", "Price")])
  colnames(Collapse_Orders_Transmitted)=c("Buy_Time", "Buy_Price", "Sell_Time", "Sell_Price")
  
  if(nrow(Collapse_Orders_Transmitted)>0){
    Duplicated_Row=unique(c(which(duplicated(Collapse_Orders_Transmitted[, c("Buy_Time", "Buy_Price")])), 
                            which(duplicated(Collapse_Orders_Transmitted[, c("Sell_Time", "Sell_Price")]))))
    if(length(Duplicated_Row)>0){
      Collapse_Orders_Transmitted=Collapse_Orders_Transmitted[-Duplicated_Row, ]
    }
    
    Collapse_Orders_Transmitted[, Profit:=(Sell_Price-Buy_Price)/Tick_Size*Tick_Value-2*Commission]
    Collapse_Orders_Transmitted[, Cum_Profit:=cumsum(Profit)]
    # Collapse_Orders_Transmitted[, Time:=as.POSIXct(format(as.POSIXct(max(Buy_Time, Sell_Time)),
    #                                                       tz="America/Los_Angeles")), by=1:nrow(Collapse_Orders_Transmitted)]
    Collapse_Orders_Transmitted[, Time:=max(Buy_Time, Sell_Time), by=1:nrow(Collapse_Orders_Transmitted)]
    # Collapse_Orders_Transmitted[, Date:=as.Date(Time, tz="America/Los_Angeles")]
    Collapse_Orders_Transmitted[, Date:=as.Date(Time)]
    # Collapse_Orders_Transmitted[, Date:=Time]
    Collapse_Orders_Transmitted[, Daily_Cum_Profit:=Cum_Profit[Time==max(Time)], by="Date"]
    Collapse_Orders_Transmitted[, Daily_Profit:=sum(Profit), by="Date"]
    
    Ind_Profit=Collapse_Orders_Transmitted[, .SD, .SDcols=c("Time", "Date", "Profit", "Daily_Profit", "Cum_Profit", "Daily_Cum_Profit")]
    Net_Profit=tail(Collapse_Orders_Transmitted$Cum_Profit, 1)
  }else{
    Ind_Profit=-Inf
    Net_Profit=-Inf
  }
  
  return(list(Orders_Transmitted=Orders_Transmitted,
              Ind_Profit=Ind_Profit,
              Net_Profit=Net_Profit))
}


#*********************
#
# [ --- Etc --- ] ----
#
#*********************
# Value_Difference_Normalizer
#****************************
Value_Difference_Normalizer=function(BarData_Temp,
                                     Width=10,
                                     Time_Unit){
  Temp=do.call(rbind,
               lapply(0:Width,
                      function(x){
                        Temp=BarData_to_Use[Ind%in%(BarData_Temp$Ind+x)]
                        Temp[, From:=BarData_to_Use[Ind%in%(BarData_Temp$Ind), Time]]
                        Temp[, Order_Ind:=x]
                      }))
  
  # remove redundant orders
  Temp[, Ind_Temp:=.I]
  i=1
  Run=TRUE
  Unique_Time=Temp[Order_Ind==0, Time]
  while(Run==TRUE){
    Remove_Ind=Temp[From>Unique_Time[i] &
                      From<=(Unique_Time[i]+Width*Time_Unit),
                    Ind_Temp]
    
    if(length(Remove_Ind)>0){
      Temp=Temp[!Ind_Temp%in%Remove_Ind, ]
      Unique_Time=Temp[Order_Ind==0, Time]
    }else{
      i=i+1
    }
    
    if(i==length(Unique_Time)){
      Run=FALSE
    }
  }
  
  Temp_2=Temp[Order_Ind==0, ]
  Temp[, Normalized_Close:=unlist(lapply(0:Width,
                                         function(x){
                                           mapply(
                                             function(a, b){
                                               b-a
                                             },
                                             BarData_to_Use[Ind%in%(Temp_2$Ind+1), Open],
                                             BarData_to_Use[Ind%in%(Temp_2$Ind+x+1), Open]
                                           )
                                         }))]
  
  Trading_Dates=unique(as.Date(Temp$Time))
  
  return(do.call(rbind,
                 lapply(Trading_Dates,
                        #x=Trading_Dates[2]
                        function(x){
                          x=as.Date(x)
                          
                          # consider the pre-market trading time
                          Temp=Temp[Time>=paste0(x-1, " ", Market_Close_Time)&
                                      Time<(as.POSIXct(paste0(x, " ", Market_Close_Time))-Time_Unit), ]
                          Temp=Temp[!(Time>=paste0(x, " ", Market_Open_Time)&
                                        Time<(as.POSIXct(paste0(x, " ", Market_Close_Time))-Time_Unit)), ]
                        }
                 )))
}
