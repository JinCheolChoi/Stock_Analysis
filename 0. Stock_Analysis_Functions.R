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
  # if time is between 13:10:00 and 13:15:00 PDT
  if(ToDay%in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")&
     (CurrentTime>=(as.ITime("13:10:00"))&
      CurrentTime<=(as.ITime("13:15:00")))){
    
    # (1) for 23 mins from 13:10:00 to 13:33:00 PDT (market closed : 13:15:00 to 13:30:00 PDT)
    Duration=60*23
    
    # put the system to sleep
    message("market closed : 13:15:00 to 13:30:00 PDT")
    
  }
  
  # if time is between 13:50:00 and 14:00:00 PDT
  if(ToDay%in%c("Monday", "Tuesday", "Wednesday", "Thursday")&
     (CurrentTime>=(as.ITime("13:50:00"))&
      CurrentTime<=(as.ITime("14:00:00")))){
    
    # (2) for 73 mins from 13:50:00 to 15:03:00 PDT (market closed : 14:00:00 to 15:00:00 PDT)
    Duration=60*73
    
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
  if((ToDay=="Friday" & CurrentTime>=(as.ITime("13:50:00"))) | # the market closes at 14:00:00 PDT on Friday
     (ToDay=="Saturday") | # the market closes on Saturday
     (ToDay=="Sunday" & CurrentTime<(as.ITime("15:05:00")))){ # the market opens at 15:00:00 PDT on Sunday
    
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
    for(Secs_Left in 1:Duration){
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
                              Market="Futures"){
  
  # the market closes at 14:00:00 PDT on Friday; and
  # at 15:00:00 PDT on the other weekdays
  ToDay=weekdays(as.Date(format(Sys.time(), tz="America/Los_Angeles")))
  CurrentTime=as.ITime(format(Sys.time(), tz="America/Los_Angeles")) # time zone : PDT
  File_Exist=file.exists(paste0(Data_Dir, Contract$symbol, "/", Contract$symbol, "_", as.Date(format(Sys.time(), tz="America/Los_Angeles")), ".csv"))
  
  # if time is after 15:00:00 PDT on weekdays, proceed ot extract historical data
  if(!ToDay%in%c("Saturday", "Sunday") &
     CurrentTime>=(as.ITime("14:00:00"))){
    
    if(!File_Exist| # if 5 second bar has not been saved yet, or
       Force==T){ # Force==1 (execute the saving process by force (overwrite the data))
      
      # request historical data of 5 seconds bar
      HistData=as.data.table(reqHistoricalData(tws, Contract, barSize="5 secs", duration="2 D", useRTH="0")) # useRTH="0" : not limited to regular trading hours
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
      
      # remove redundant data
      # different time zone examples : "GMT", "America/Los_Angeles", "Europe/London"
      if(Market=="Futures"){
        Time_From=as.POSIXct(paste0(as.Date(format(Sys.time(), tz="America/Los_Angeles"))-1, " 15:00:00"), tz="America/Los_Angeles")
        Time_To=as.POSIXct(paste0(as.Date(format(Sys.time(), tz="America/Los_Angeles")), " 15:00:00"), tz="America/Los_Angeles")
      }else if(Market=="Stock"){
        Time_From=as.POSIXct(paste0(as.Date(format(Sys.time(), tz="America/Los_Angeles"))-1, " 17:00:00"), tz="America/Los_Angeles")
        Time_To=as.POSIXct(paste0(as.Date(format(Sys.time(), tz="America/Los_Angeles")), " 17:00:00"), tz="America/Los_Angeles")
      }
      
      HistData=HistData[Time>=Time_From &
                          Time<Time_To, ]
      
      HistData[, Time:=as.POSIXct(format(as.POSIXct(Time), 
                                         tz="America/Los_Angeles"), 
                                  tz="America/Los_Angeles")]
      
      # save historical data up to today's market closed at 15:00:00 pm PDT
      fwrite(HistData,
             paste0(Data_Dir, Contract$symbol, "/", Contract$symbol, "_", as.Date(format(Sys.time(), tz="America/Los_Angeles")), ".csv"))
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
  }else{
    message("No new data to save yet.") # echo Message in terminal
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
    #(1) for 23 mins from 13:10:00 to 13:33:00 PDT (market closed : 13:15:00 to 13:30:00 PDT)
    #(2) for 73 mins from 13:50:00 to 15:03:00 PDT (market closed : 14:00:00 to 15:00:00 PDT)
    # (ToDay=="Friday" & CurrentTime>=(as.ITime("13:50:00"))) | # the market closes at 14:00:00 PDT on Friday
    #   (ToDay=="Saturday") | # the market closes on Saturday
    #   (ToDay=="Sunday" & CurrentTime<(as.ITime("15:05:00")))
    `5SecsBarHistData`[, Daily_Time:=strftime(Time, format="%H:%M:%S", tz="America/Los_Angeles")]
    `5SecsBarHistData`<<-`5SecsBarHistData`[!(Daily_Time>="13:10:00" &
                                                Daily_Time<"13:35:00"), ]
    `5SecsBarHistData`<<-`5SecsBarHistData`[!(Daily_Time>="13:50:00" &
                                                Daily_Time<"15:05:00"), ]
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
  Trend=Order_Rules[["General"]][["Trend"]]
  Stop_Order=as.numeric(Order_Rules[["General"]][["Stop_Order"]])
  Profit_Order=as.numeric(Order_Rules[["General"]][["Profit_Order"]])
  Strategy_Indicators=names(Indicators)
  Strategy_Models=names(Models)
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
  Strategy_Rules=names(Order_Rules)[names(Order_Rules)!="General"]
  
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
                                LmtPrice=0,
                                Filled=0,
                                Sigs_N=0)
  Orders_Transmitted=Orders_Transmitted[-1,]
  Live_Data[, `:=`(Symbol=NULL, Time=NULL, Open=NULL,
                   High=NULL, Low=NULL, Close=NULL,
                   Volume=NULL, Net_Volume=NULL, Count=NULL)]
  Time_Unit=BarData$Time[2]-BarData$Time[1]
  Early_Order_Transmit_Proceeded="No"
  for(i in 1:(nrow(BarData)-1)){
    # i=67
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
      Calculated_Indicators=sapply(Strategy_Indicators,
                                   function(x)
                                     if(x=="Close"){
                                       Live_Data[["Close"]]
                                     }else{
                                       if(nrow(Live_Data)>Indicators[[x]][['n']]+1){ # BBands : n-1, RSI : n+1
                                         do.call(x, 
                                                 c(list(Live_Data[["Close"]]), # for now only using "Close price", additional work would be required in the future if the indicator does not depend on "Close price"
                                                   Indicators[[x]]))
                                       }
                                     }
      )
      # Calculated_Indicators=Calculated_Indicators[-which(sapply(Calculated_Indicators, is.null))]
      
      #***********
      # fit models
      #***********
      Signals=as.data.table(sapply(Strategy_Models,
                                   function(x){
                                     Model_Info=Models_Env[[x]] # variables and functions defined for the model object
                                     Calculated_Indicators_Combined=do.call(cbind, Calculated_Indicators) # combined Calculated_Indicators
                                     Calculated_Indicators_Names=names(Calculated_Indicators)[unlist(lapply(Calculated_Indicators, function(x) !is.null(x)))] #
                                     if(sum(!Model_Info[["Essential_Indicators"]]%in%Calculated_Indicators_Names)==0){ # if none of essential indicators hasn't been calculated in Calculated_Indicators, proceed to run the model
                                       do.call(Model_Info[["Function"]],
                                               c(list(Calculated_Indicators_Combined),
                                                 Models[[x]]))}
                                   }))
      
      # Signals are assigned opposite if Trend=TRUE
      if(nrow(Signals)==2){
        if(Trend==TRUE){
          if(sum(Signals$Trend)>0){
            Signals[, which(sapply(Signals, function(x) sum(x==T)==1)):=lapply(.SD, function(x) x==F), .SDcols=which(sapply(Signals, function(x) sum(x==T)==1))]
          }else{
            Signals[1, ]=FALSE
            Signals[2, ]=FALSE
          }
        }
      }
      
      #***************
      # transmit order
      #***************
      if(nrow(Signals)>0){
        # - N of models <= Sigs_N <= N of models
        Sigs_N=apply(Signals, 1, sum)
        
        # number of orders held (+:more long, -:more short)
        N_Orders_held=sum(Orders_Transmitted[["Action"]]=="Buy")-
          sum(Orders_Transmitted[["Action"]]=="Sell")
        
        # Order_to_Transmit
        Order_to_Transmit=lapply(Strategy_Rules,
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
        # print(paste0(i))
        # add Order_to_Transmit to Orders_Transmitted
        Orders_Transmitted=rbind(Orders_Transmitted,
                                 do.call(rbind, Order_to_Transmit),
                                 fill=T)
        # remove Orders_Transmitted
        rm(Order_to_Transmit)
        #print(paste0("Transmit order / i : ", i, " / action : ", tail(Orders_Transmitted[["Detail"]], 1)))
      }
    }
    
    #***********
    # fill order
    #***********
    # buy
    if(sum(Orders_Transmitted[["Action"]]=="Buy"&
           Orders_Transmitted[["Filled"]]==0)>0){ # if there is any transmitted buy order that has not been filled yet
      
      Unfilled_Buy_Position_Times=Orders_Transmitted[Action=="Buy"&Filled==0, Submit_Time]
      Unfilled_Buy_Position_Prices=Orders_Transmitted[Submit_Time%in%Unfilled_Buy_Position_Times&Filled==0, LmtPrice]
      Which_Buy_Position_to_Fill=which(BarData[["Low"]][i+1]<Unfilled_Buy_Position_Prices)[1] # fill the oldest order that have met the price criterion
      
      Orders_Transmitted[Submit_Time==Unfilled_Buy_Position_Times[Which_Buy_Position_to_Fill],
                         `:=`(Filled_Time=BarData[i+1, Time],
                              Filled=1)]
      
      # if not filled, just cancel the transmit 60 minutes later
      if(sum(Orders_Transmitted[["Filled"]]==0)){
        if((as.numeric(BarData[i+1, Time])-as.numeric(Orders_Transmitted[Filled==0, Submit_Time]))>60*60){
          Orders_Transmitted=Orders_Transmitted[Filled!=0, ]
          #print(paste0("Transmit order / i : ", i, " / action : Cancelled"))
        }
      }
      
    }
    
    # sell
    if(sum(Orders_Transmitted[["Action"]]=="Sell"&
           Orders_Transmitted[["Filled"]]==0)>0){ # if there is any transmitted sell order that has not been filled yet
      
      Unfilled_Sell_Position_Times=Orders_Transmitted[Action=="Sell"&Filled==0, Submit_Time]
      Unfilled_Sell_Position_Prices=Orders_Transmitted[Submit_Time%in%Unfilled_Sell_Position_Times&Filled==0, LmtPrice]
      
      Which_Sell_Position_to_Fill=which(BarData[["High"]][i+1]>Unfilled_Sell_Position_Prices)[1] # fill the oldest order that have met the price criterion
      
      Orders_Transmitted[Submit_Time==Unfilled_Sell_Position_Times[Which_Sell_Position_to_Fill],
                         `:=`(Filled_Time=BarData[i+1, Time],
                              Filled=1)]
      
      # if not filled, just cancel the transmit
      if(sum(Orders_Transmitted[["Filled"]]==0)){
        if((as.numeric(BarData[i+1, Time])-as.numeric(Orders_Transmitted[Filled==0, Submit_Time]))>60*60){
          Orders_Transmitted=Orders_Transmitted[Filled!=0, ]
          #print(paste0("Transmit order / i : ", i, " / action : Cancelled"))
        }
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
  Collapse_Orders_Transmitted=cbind(Orders_Transmitted[Action=="Buy", 
                                                       c("Filled_Time", "LmtPrice")],
                                    Orders_Transmitted[Action=="Sell", 
                                                       c("Filled_Time", "LmtPrice")])
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
                     Strategy){
  Order_Rules=Strategy$Order_Rules
  Indicators=Strategy$Indicators
  Models=Strategy$Models
  
  Simple_BBands_Info=get("Simple_BBands", envir=Models_Env)
  assign("Simple_BBands", Simple_BBands_Info$Function, envir=.GlobalEnv)
  
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
  # general parameters
  Max_Rows=Strategy$Max_Rows
  
  # order parameters
  General=Order_Rules$General
  OrderType=Order_Rules$General$OrderType
  Position_Direction=Order_Rules$General$Position_Direction
  
  # indicators
  Passed_Indicators=names(Indicators)
  Passed_Models=names(Models)
  
  # models
  Long_Consec_Times=Models$Simple_BBands$Long_Consec_Times
  Short_Consec_Times=Models$Simple_BBands$Short_Consec_Times
  Long_PctB=Models$Simple_BBands$Long_PctB
  Short_PctB=Models$Simple_BBands$Short_PctB
  
  
  #******************
  # preliminary steps
  #******************
  if(Position_Direction=="both"){
    Max_Long_Orders=Max_Short_Orders=Max_Orders-1
  }else if(Position_Direction=="long"){
    Max_Long_Orders=Max_Orders-1
    Max_Short_Orders=-1
  }else if(Position_Direction=="short"){
    Max_Long_Orders=-1
    Max_Short_Orders=Max_Orders-1
  }
  
  
  #*********************
  # simulation algorithm
  #*********************
  for(i in 1:(nrow(BarData)-1)){
    # i=1
    if(!exists("Live_Data")){
      # define Live_Data
      Live_Data=BarData[i, ]
    }else{
      Live_Data=rbind(Live_Data, BarData[i, ], fill=T) %>% tail(Max_Rows)
    }
    
    if(!exists("Orders_Transmitted")){
      # define Orders_Transmitted
      Orders_Transmitted=data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                                    Submit_Time=tail(Live_Data, 1)[, Time],
                                    Filled_Time=tail(Live_Data, 1)[, Time],
                                    Action="", 
                                    TotalQuantity=0,
                                    OrderType=OrderType,
                                    LmtPrice=0,
                                    Filled=0)
      Orders_Transmitted=Orders_Transmitted[-1,]
    }
    
    
    #*********************
    # calculate indicators
    #*********************
    # bollinger bands
    if("BBands"%in%Passed_Indicators){
      if(nrow(Live_Data)>Indicators$BBands$n-1){
        BBands_Data=Live_Data[, BBands(Close, n=Indicators$BBands$n, sd=Indicators$BBands$sd)]
      }
    }
    
    # rsi
    if("RSI"%in%Passed_Indicators){
      if(nrow(Live_Data)>Indicators$RSI$n+1){
        Live_Data[, RSI:=RSI(Close, n=Indicators$RSI$n)]
      }
    }
    
    # macd
    if("MACD"%in%Passed_Indicators){
      if(nrow(Live_Data)>34){
        MACD_Data=Live_Data[, MACD(Close)]
      }
    }
    
    
    #***********
    # fit models
    #***********
    # Simple_BBands
    if("Simple_BBands"%in%Passed_Models){
      # signal to enter a long (short) position determined by Simple_BBands
      Long_Sig_by_Simple_BBands=0
      Short_Sig_by_Simple_BBands=0
      
      if("BBands"%in%Passed_Indicators&
         exists("BBands_Data")){
        
        # number of filled orders
        N_Filled_Buys=nrow(Orders_Transmitted[Action=="Buy"&Filled==1, ]) # buy
        N_Filled_Sells=nrow(Orders_Transmitted[Action=="Sell"&Filled==1, ]) # sell
        
        if(N_Filled_Buys-N_Filled_Sells==0){ # if there is no currently filled order
          Sigs_by_Simple_BBands=Simple_BBands(BBands_Data,Long_Consec_Times, Short_Consec_Times, Long_PctB, Short_PctB)
        }else if(N_Filled_Buys-N_Filled_Sells>0){ # if the currently filled order is buy order, generate signal to sell as soon as pctB>=Short_PctB
          Sigs_by_Simple_BBands=Simple_BBands(BBands_Data, Long_Consec_Times, 1, Long_PctB, Short_PctB)
        }else if(N_Filled_Buys-N_Filled_Sells<0){ # if the currently filled order is short order, generate signal to sell as soon as pctB<=Long_PctB
          Sigs_by_Simple_BBands=Simple_BBands(BBands_Data, 1, Short_Consec_Times, Long_PctB, Short_PctB)
        }
        # determined signals
        Long_Sig_by_Simple_BBands=Sigs_by_Simple_BBands[1]
        Short_Sig_by_Simple_BBands=Sigs_by_Simple_BBands[2]
        
      }else{
        if(!"BBands"%in%Passed_Indicators){
          stop("BBands required")
        }
      }
    }
    
    # Simple_RSI
    if("Simple_RSI"%in%Passed_Models){
      # signal to enter a long (short) position determined by Simple_RSI
      Long_Sig_by_Simple_RSI=0
      Short_Sig_by_Simple_RSI=0
      
      if("RSI"%in%Passed_Indicators&
         length(Live_Data$RSI)>0){
        
      }else{
        if(!"RSI"%in%Passed_Indicators){
          stop("RSI required")
        }
      }
    }
    
    
    #***************
    # transmit order
    #***************
    # buy
    if(Long_Sig_by_Simple_BBands){
      # determine the position
      if(sum(Orders_Transmitted[Action=="Buy", TotalQuantity])-
         sum(Orders_Transmitted[Action=="Sell", TotalQuantity])<=
         (Max_Long_Orders)){ # the number of currently remaining filled or transmitted long positions is limited to Max_Orders(= Max_Long_Orders + 1)
        print(paste0("buy : ", i))
        Orders_Transmitted=rbind(Orders_Transmitted,
                                 data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                                            Submit_Time=tail(Live_Data, 1)[, Time],
                                            Filled_Time=tail(Live_Data, 1)[, Time],
                                            Action="Buy",
                                            TotalQuantity=1,
                                            OrderType=OrderType,
                                            LmtPrice=tail(Live_Data, 1)[, Close],
                                            Filled=0))
      }
    }
    
    # sell
    if(Short_Sig_by_Simple_BBands){
      if(sum(Orders_Transmitted[Action=="Sell", TotalQuantity])-
         sum(Orders_Transmitted[Action=="Buy", TotalQuantity])<=
         (Max_Short_Orders)){ # the number of currently remaining filled or transmitted short positions is limited to Max_Orders(= Max_Short_Orders + 1)
        print(paste0("sell : ", i))
        Orders_Transmitted=rbind(Orders_Transmitted,
                                 data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                                            Submit_Time=tail(Live_Data, 1)[, Time],
                                            Filled_Time=tail(Live_Data, 1)[, Time],
                                            Action="Sell",
                                            TotalQuantity=1,
                                            OrderType=OrderType,
                                            LmtPrice=tail(Live_Data, 1)[, Close],
                                            Filled=0))
      }
    }
    
    
    #***********
    # fill order
    #***********
    # buy
    if(nrow(Orders_Transmitted[Action=="Buy"&Filled==0, ])>0){ # if there is any transmitted buy order that has not been filled yet
      
      Unfilled_Buy_Position_Times=Orders_Transmitted[Action=="Buy"&Filled==0, Submit_Time]
      Unfilled_Buy_Position_Prices=Orders_Transmitted[Submit_Time%in%Unfilled_Buy_Position_Times, LmtPrice]
      Which_Buy_Position_to_Fill=which(BarData[i+1, Low]<Unfilled_Buy_Position_Prices)[1] # fill the oldest order that have met the price criterion
      
      Orders_Transmitted[Submit_Time==Unfilled_Buy_Position_Times[Which_Buy_Position_to_Fill],
                         `:=`(Filled_Time=BarData[i+1, Time],
                              Filled=1)]
    }
    # sell
    if(nrow(Orders_Transmitted[Action=="Sell"&Filled==0, ])>0){ # if there is any transmitted sell order that has not been filled yet
      
      Unfilled_Sell_Position_Times=Orders_Transmitted[Action=="Sell"&Filled==0, Submit_Time]
      Unfilled_Sell_Position_Prices=Orders_Transmitted[Submit_Time%in%Unfilled_Sell_Position_Times, LmtPrice]
      Which_Sell_Position_to_Fill=which(BarData[i+1, High]>Unfilled_Sell_Position_Prices)[1] # fill the oldest order that have met the price criterion
      
      Orders_Transmitted[Submit_Time==Unfilled_Sell_Position_Times[Which_Sell_Position_to_Fill],
                         `:=`(Filled_Time=BarData[i+1, Time],
                              Filled=1)]
    }
    
  }
  
  
  #**********************
  # calculate the balance
  #**********************
  Collapse_Orders_Transmitted=cbind(Orders_Transmitted[Action=="Buy", 
                                                       c("Filled_Time", "LmtPrice")],
                                    Orders_Transmitted[Action=="Sell", 
                                                       c("Filled_Time", "LmtPrice")])
  colnames(Collapse_Orders_Transmitted)=c("Buy_Time", "Buy_Price", "Sell_Time", "Sell_Price")
  Duplicated_Row=unique(c(which(duplicated(Collapse_Orders_Transmitted[, c("Buy_Time", "Buy_Price")])), 
                          which(duplicated(Collapse_Orders_Transmitted[, c("Sell_Time", "Sell_Price")]))))
  if(length(Duplicated_Row)>0){
    Collapse_Orders_Transmitted=Collapse_Orders_Transmitted[-Duplicated_Row, ]
  }
  
  Ind_Profit=2*Collapse_Orders_Transmitted[, Sell_Price-Buy_Price]-2*0.52
  Net_Profit=sum(Ind_Profit)
  
  
  return(list(BarData=BarData,
              Orders_Transmitted=Orders_Transmitted,
              Ind_Profit=Ind_Profit,
              Net_Profit=Net_Profit))
  
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




#****************************
# Profit_Loss_Cut_Transmitted
#****************************
Profit_Loss_Cut_Transmitted=function(Orders_Transmitted, Next_BarData, Profit_Order, Stop_Order){
  N_Remaining_Orders=sum(Orders_Transmitted[["Action"]]=="Buy"&Orders_Transmitted[["Filled"]]==1)-
    sum(Orders_Transmitted[["Action"]]=="Sell"&Orders_Transmitted[["Filled"]]==1)
  
  Profit_Transmitted=c()
  Stop_Transmitted=c()
  if(N_Remaining_Orders>0){ # still on long
    
    Profit_Price=tail(Orders_Transmitted[Action=="Buy"&Filled==1], N_Remaining_Orders)[["LmtPrice"]][1]+Profit_Order
    Stop_Price=tail(Orders_Transmitted[Action=="Buy"&Filled==1], N_Remaining_Orders)[["LmtPrice"]][1]-Stop_Order
    
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
        LmtPrice=Profit_Price,
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
        LmtPrice=Stop_Price,
        Filled=1,
        Sigs_N=1
      )
    }
    
  }else if(N_Remaining_Orders<0){ # still on short
    Profit_Price=tail(Orders_Transmitted[Action=="Sell"&Filled==1], -N_Remaining_Orders)[["LmtPrice"]][1]-Profit_Order
    Stop_Price=tail(Orders_Transmitted[Action=="Sell"&Filled==1], -N_Remaining_Orders)[["LmtPrice"]][1]+Stop_Order
    
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
        LmtPrice=Profit_Price,
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
        LmtPrice=Stop_Price,
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
  
  # add a model to the corresponding strategy
  Strategy_temp=get(Strategy, envir=.GlobalEnv)
  Strategy_temp$Models[[Model]]=New_ModelParams
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
  lapply(c("plyr"), checkpackages)
  
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
  IBrokers::reqRealTimeBars(tws, contract, barSize="5", useRTH=F,
                            eventWrapper=eWrapper_cust(),
                            CALLBACK=twsCALLBACK_cust)
  
  # if it fails to create RealTimeBarData
  if(!exists("RealTimeBarData")){
    print("RealTimeBarData doesn't exist")
    Sys.sleep(0.5) # suspend execution for a while to prevent the system from breaking
    return(New_Data) # terminate the algorithm by retunning New_Data
  }
  
  # initial Recent_RealTimeBarData
  if(!exists("Recent_RealTimeBarData")){
    print("Recent_RealTimeBarData doesn't exist")
    Recent_RealTimeBarData=RealTimeBarData
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
      print("Recent_RealTimeBarData == RealTimeBarData")
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
    tickPrice <- function(curMsg, msg, timestamp, file,
                          ...) {
      symbols <- get.Data("symbols")
      # e_tick_price(NULL, msg, timestamp, file, symbols,
      #              ...)
    }
    tickSize <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- get.Data("symbols")
      # e_tick_size(NULL, msg, timestamp, file, symbols,
      #             ...)
    }
    tickOptionComputation <- function(curMsg, msg, timestamp,
                                      file, ...) {
      symbols <- get.Data("symbols")
      # e_tick_option(NULL, msg, timestamp, file, symbols,
      #               ...)
    }
    tickGeneric <- function(curMsg, msg, timestamp, file,
                            ...) {
      symbols <- get.Data("symbols")
      # e_tick_generic(NULL, msg, timestamp, file, symbols,
      #                ...)
    }
    tickString <- function(curMsg, msg, timestamp, file,
                           ...) {
      symbols <- get.Data("symbols")
      # e_tick_string(NULL, msg, timestamp, file, symbols,
      #               ...)
    }
    tickEFP <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- get.Data("symbols")
      # e_tick_EFP(NULL, msg, timestamp, file, symbols,
      #            ...)
    }
    orderStatus <- function(curMsg, msg, timestamp, file,
                            ...) {
      # e_order_status(curMsg, msg)
      c(curMsg, msg)
    }
    errorMessage <- function(curMsg, msg, timestamp, file,
                             twsconn, ...) {
      if (msg[3] == "1100")
        twsconn$connected <- FALSE
      if (msg[3] %in% c("1101", "1102"))
        twsconn$connected <- TRUE
      # cat("TWS Message:", msg, "\n")
    }
    openOrder <- function(curMsg, msg, timestamp, file,
                          ...) {
      c(curMsg, msg)
    }
    openOrderEnd <- function(curMsg, msg, timestamp, file,
                             ...) {
      c(curMsg, msg)
    }
    updateAccountValue <- function(curMsg, msg, timestamp,
                                   file, ...) {
      c(curMsg, msg)
    }
    updatePortfolio <- function(curMsg, msg, timestamp,
                                file, ...) {
      # e_portfolio_value(curMsg, msg)
      c(curMsg, msg)
    }
    updateAccountTime <- function(curMsg, msg, timestamp,
                                  file, ...) {
      c(curMsg, msg)
    }
    accountDownloadEnd <- function(curMsg, msg, timestamp,
                                   file, ...) {
      c(curMsg, msg)
    }
    nextValidId <- function(curMsg, msg, timestamp, file,
                            ...) {
      c(curMsg, msg)
    }
    contractDetails <- function(curMsg, msg, timestamp,
                                file, ...) {
      c(curMsg, msg)
    }
    bondContractDetails <- function(curMsg, msg, timestamp,
                                    file, ...) {
      c(curMsg, msg)
    }
    contractDetailsEnd <- function(curMsg, msg, timestamp,
                                   file, ...) {
      c(curMsg, msg)
    }
    execDetails <- function(curMsg, msg, timestamp, file,
                            ...) {
      # e_execDetails(curMsg, msg, file, ...)
    }
    execDetailsEnd <- function(curMsg, msg, timestamp, file,
                               ...) {
      c(curMsg, msg)
    }
    updateMktDepth <- function(curMsg, msg, timestamp, file,
                               ...) {
      symbols <- get.Data("symbols")
      # e_update_mkt_depth(NULL, msg, timestamp, file, symbols,
      #                    ...)
    }
    updateMktDepthL2 <- function(curMsg, msg, timestamp,
                                 file, ...) {
      symbols <- get.Data("symbols")
      # e_update_mkt_depthL2(NULL, msg, timestamp, file,
      #                      symbols, ...)
    }
    updateNewsBulletin <- function(curMsg, msg, timestamp,
                                   file, ...) {
      cat("newsMsgId: ", msg[2], "newsMsgType: ", msg[3],
          "newsMessage: ", msg[4], "origExch:", msg[5],
          "\n")
      c(curMsg, msg)
    }
    managedAccounts <- function(curMsg, msg, timestamp,
                                file, ...) {
      c(curMsg, msg)
    }
    receiveFA <- function(curMsg, msg, timestamp, file,
                          ...) {
      c(curMsg, msg)
    }
    historicalData <- function(curMsg, msg, timestamp, file,
                               ...) {
      c(curMsg, msg)
    }
    scannerParameters <- function(curMsg, msg, timestamp,
                                  file, ...) {
      # cat(msg <- rawToChar(msg[-which(msg == as.raw(0))]))
      c(curMsg, msg)
    }
    scannerData <- function(curMsg, reqId, rank, contract,
                            distance, benchmark, projection, legsStr) {
      # e_scannerData(curMsg, reqId, rank, contract, distance,
      #               benchmark, projection, legsStr)
    }
    scannerDataEnd <- function(curMsg, msg, timestamp, file,
                               ...) {
      c(curMsg, msg)
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
      #**************
      # original code
      #**************
      # `e_real_time_bars` <- function(curMsg, msg, symbols, file, ...) {
      #   # msg[1] is VERSION
      #   columns <- c("Id","time","open","high","low","close","volume",
      #                "wap","count")
      #   id <- as.numeric(msg[2])
      #   file <- file[[id]]
      #   msg[2] <- symbols[as.numeric(msg[2])]
      #   msg[3] <- strftime(structure(as.numeric(msg[3]), class=c("POSIXt","POSIXct")))
      #   cat(paste(columns,"=",msg[-1],sep=""),'\n',file=file,append=TRUE)
      # }
      
      
      e_real_time_bars_dup(curMsg, msg, symbols, file, ...) # # # #
    }
    currentTime <- function(curMsg, msg, timestamp, file,
                            ...) {
      c(curMsg, msg)
    }
    fundamentalData <- function(curMsg, msg, timestamp,
                                file, ...) {
      # e_fundamentalData(curMsg, msg)
    }
    deltaNeutralValidation <- function(curMsg, msg, timestamp,
                                       file, ...) {
      c(curMsg, msg)
    }
    tickSnapshotEnd <- function(curMsg, msg, timestamp,
                                file, ...) {
      c(curMsg, msg)
    }
  }
  else {
    tickPrice <- tickSize <- tickOptionComputation <- tickGeneric <- tickString <- tickEFP <- orderStatus <- openOrder <- openOrderEnd <- updateAccountValue <- updateAccountTime <- updatePortfolio <- accountDownloadEnd <- nextValidId <- contractDetails <- bondContractDetails <- contractDetailsEnd <- execDetails <- execDetailsEnd <- updateMktDepth <- updateMktDepthL2 <- updateNewsBulletin <- managedAccounts <- receiveFA <- historicalData <- scannerParameters <- scannerData <- scannerDataEnd <- realtimeBars <- currentTime <- fundamentalData <- deltaNeutralValidation <- tickSnapshotEnd <- function(curMsg,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          msg, timestamp, file, ...) {
      cat(as.character(timestamp), curMsg, msg, "\n",
          file = file[[1]], append = TRUE, ...)
    }
    errorMessage <- function(curMsg, msg, timestamp, file,
                             twsconn, ...) {
      cat(as.character(timestamp), curMsg, msg, "\n",
          file = file[[1]], append = TRUE, ...)
    }
    
  }
  eW <- list(.Data = .Data, get.Data = get.Data, assign.Data = assign.Data,
             remove.Data = remove.Data, tickPrice = tickPrice, tickSize = tickSize,
             #tickOptionComputation = tickOptionComputation,
             tickGeneric = tickGeneric,
             tickString = tickString, tickEFP = tickEFP, orderStatus = orderStatus,
             errorMessage = errorMessage, openOrder = openOrder,
             openOrderEnd = openOrderEnd, updateAccountValue = updateAccountValue,
             updatePortfolio = updatePortfolio, updateAccountTime = updateAccountTime,
             accountDownloadEnd = accountDownloadEnd, nextValidId = nextValidId,
             contractDetails = contractDetails, bondContractDetails = bondContractDetails,
             contractDetailsEnd = contractDetailsEnd, execDetails = execDetails,
             execDetailsEnd = execDetailsEnd, updateMktDepth = updateMktDepth,
             updateMktDepthL2 = updateMktDepthL2, updateNewsBulletin = updateNewsBulletin,
             managedAccounts = managedAccounts, receiveFA = receiveFA,
             historicalData = historicalData, scannerParameters = scannerParameters,
             scannerData = scannerData, scannerDataEnd = scannerDataEnd,
             realtimeBars = realtimeBars, currentTime = currentTime,
             fundamentalData = fundamentalData, deltaNeutralValidation = deltaNeutralValidation,
             tickSnapshotEnd = tickSnapshotEnd)
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




