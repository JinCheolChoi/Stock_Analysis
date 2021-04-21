#*******************
#
# checkpackages ----
#
#*******************
# package check
#**************
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


#**************
#
# snapShot ----
#
#**************
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





#**********************
#
# twsCALLBACK_cust ----
#
#**********************
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
          Sys.sleep((sys.time - last.time) * playback)
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
          Sys.sleep(5 * playback)
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
        rep.ind=0 # # # #
      }
      
    }, error = function(e) {
      close(twsCon)
      stop("IB connection error. Connection closed", 
           call. = FALSE)
    })
  }
}





#*******************
#
# eWrapper_cust ----
#
#*******************
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
      e_tick_price(NULL, msg, timestamp, file, symbols, 
                   ...)
    }
    tickSize <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- get.Data("symbols")
      e_tick_size(NULL, msg, timestamp, file, symbols, 
                  ...)
    }
    tickOptionComputation <- function(curMsg, msg, timestamp, 
                                      file, ...) {
      symbols <- get.Data("symbols")
      e_tick_option(NULL, msg, timestamp, file, symbols, 
                    ...)
    }
    tickGeneric <- function(curMsg, msg, timestamp, file, 
                            ...) {
      symbols <- get.Data("symbols")
      e_tick_generic(NULL, msg, timestamp, file, symbols, 
                     ...)
    }
    tickString <- function(curMsg, msg, timestamp, file, 
                           ...) {
      symbols <- get.Data("symbols")
      e_tick_string(NULL, msg, timestamp, file, symbols, 
                    ...)
    }
    tickEFP <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- get.Data("symbols")
      e_tick_EFP(NULL, msg, timestamp, file, symbols, 
                 ...)
    }
    orderStatus <- function(curMsg, msg, timestamp, file, 
                            ...) {
      e_order_status(curMsg, msg)
      c(curMsg, msg)
    }
    errorMessage <- function(curMsg, msg, timestamp, file, 
                             twsconn, ...) {
      if (msg[3] == "1100") 
        twsconn$connected <- FALSE
      if (msg[3] %in% c("1101", "1102")) 
        twsconn$connected <- TRUE
      cat("TWS Message:", msg, "\n")
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
      e_portfolio_value(curMsg, msg)
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
      e_execDetails(curMsg, msg, file, ...)
    }
    execDetailsEnd <- function(curMsg, msg, timestamp, file, 
                               ...) {
      c(curMsg, msg)
    }
    updateMktDepth <- function(curMsg, msg, timestamp, file, 
                               ...) {
      symbols <- get.Data("symbols")
      e_update_mkt_depth(NULL, msg, timestamp, file, symbols, 
                         ...)
    }
    updateMktDepthL2 <- function(curMsg, msg, timestamp, 
                                 file, ...) {
      symbols <- get.Data("symbols")
      e_update_mkt_depthL2(NULL, msg, timestamp, file, 
                           symbols, ...)
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
      cat(msg <- rawToChar(msg[-which(msg == as.raw(0))]))
      c(curMsg, msg)
    }
    scannerData <- function(curMsg, reqId, rank, contract, 
                            distance, benchmark, projection, legsStr) {
      e_scannerData(curMsg, reqId, rank, contract, distance, 
                    benchmark, projection, legsStr)
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
                                   tz="PST8PDT"), 
                            tz="PST8PDT") # fix the timezone to PDT
        
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
      e_fundamentalData(curMsg, msg)
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
             tickOptionComputation = tickOptionComputation, tickGeneric = tickGeneric, 
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





#*******************
#
# System_Break ----
#
#*******************
# a break during the temporary market close time
#******************************************************************************
# Output : return Rerun_Live_Trading=1 (1) after break during market closure or 
#                                      (2) after a temporary break (for test) during the market open time
System_Break=function(Rerun_Trading=0, Log=F){
  
  #**************************
  # No break (market is open)
  # re-run indicator
  Rerun=1
  Duration=60
  
  # the market closes temporarily on weekdays
  ToDay=weekdays(as.Date(format(Sys.time(), tz="PST8PDT")))
  CurrentTime=as.ITime(format(Sys.time(), tz="PST8PDT")) # time zone : PDT
  
  #**********************
  # Daily temporary break
  if(ToDay%in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")&
     (CurrentTime>=(as.ITime("13:10:00"))& # if time is between 13:10:00 and 13:15:00 PDT
      CurrentTime<=(as.ITime("13:15:00")))){
    
    # (1) for 25 mins from 13:10:00 to 13:35:00 PDT (market closed : 13:15:00 to 13:30:00 PDT)
    Duration=60*25
    
    # put the system to sleep
    message("market closed : 13:15:00 to 13:30:00 PDT")
    
  }else if(ToDay%in%c("Monday", "Tuesday", "Wednesday", "Thursday")&
           (CurrentTime>=(as.ITime("13:50:00"))& # if time is between 13:50:00 and 14:00:00 PDT
            CurrentTime<=(as.ITime("14:00:00")))){
    
    # (2) for 75 mins from 13:50:00 to 15:05:00 PDT (market closed : 14:00:00 to 15:00:00 PDT)
    Duration=60*75
    
    # put the system to sleep
    message("market closed : 14:00:00 to 15:00:00 PDT")
    
  }else if(ToDay%in%c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday")&
           (CurrentTime>=(as.ITime("23:40:00"))& # if time is between 23:40:00 and 23:45:00 PDT
            CurrentTime<=(as.ITime("23:45:00")))){
    
    # (3) for 20 mins from 23:40:00 to 24:00:00 PDT (TWS automatic log-off)
    Duration=60*20
    
    # put the system to sleep
    message("TWS automatic log-off and restart")
    
  }
  
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
    if(file.exists(paste0(working.dir, "/Log/Stop_Live_Trading_Log.csv"))){
      Log=data.table(Time=Sys.time())
      Log=rbind(Log,
                fread(paste0(working.dir, "/Log/Stop_Live_Trading_Log.csv")))
      fwrite(Log, paste0(working.dir, "/Log/Stop_Live_Trading_Log.csv"))
    }else{
      Log=data.table(Time=Sys.time())
      fwrite(Log, paste0(working.dir, "/Log/Stop_Live_Trading_Log.csv"))
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





#**************************
#
# Daily_Hist_Data_Save ----
#
#**************************
# execute a daily save of 5 second bar data at 15:00:00 pm PDT
Daily_Hist_Data_Save=function(Force=T, Log=F){
  # the market closes at 14:00:00 PDT on Friday; and
  # at 15:00:00 PDT on the other weekdays
  ToDay=weekdays(as.Date(format(Sys.time(), tz="PST8PDT")))
  CurrentTime=as.ITime(format(Sys.time(), tz="PST8PDT")) # time zone : PDT
  File_Exist=file.exists(paste0(working.dir, "Data/", contract$symbol, "_", as.Date(format(Sys.time(), tz="PST8PDT")), ".csv"))
  
  # if time is after 15:00:00 PDT on weekdays, proceed ot extract historical data
  if(!ToDay%in%c("Saturday", "Sunday") &
     CurrentTime>=(as.ITime("15:00:00"))){
    
    if(!File_Exist| # if 5 second bar has not been saved yet, or
       Force==T){ # Force==1 (execute the saving process by force (overwrite the data))
      
      # request historical data of 5 seconds bar
      HistData=as.data.table(reqHistoricalData(tws, contract, barSize="5 secs", duration="2 D", useRTH="0")) # useRTH="0" : not limited to regular trading hours
      colnames(HistData)=c("Time", "Open", "High", "Low", "Close", "Volume", "Wap", "hasGaps", "Count")
      
      HistData[, hasGaps:=NULL] # hasGaps is redundant
      
      HistData=data.table(Symbol=contract$symbol,
                          HistData)
      
      # HistData=as.data.table(reqHistoricalData(tws, contract, barSize="5 secs", duration="1 M", useRTH="0")) # useRTH="0" : not limited to regular trading hours
      # colnames(HistData)=c("Time", "Open", "High", "Low", "Close", "Volume", "Wap", "hasGaps", "Count")
      #
      # HistData[, hasGaps:=NULL] # hasGaps is redundant
      #
      # HistData=data.table(Symbol=contract$symbol,
      #                     HistData)
      #
      # # "for statement" to get and save bar data day-by-day
      # for(Date in seq(as.Date("2021-03-15"), as.Date(format(Sys.time(), tz="PST8PDT")), by="day")){
      #   if(weekdays.Date(as.Date(Date))=="Saturday"|
      #      weekdays.Date(as.Date(Date))=="Sunday"){
      #     next
      #   }
      #
      #   Time_Cutoff=as.POSIXct(paste0(as.Date(Date), " 15:00:00"), tz="PST8PDT")
      #
      #
      #   HistData[Time>=(Time_Cutoff-60*60*24)&
      #              Time<Time_Cutoff, ]
      #
      #   fwrite(HistData[Time>=(Time_Cutoff-60*60*24)&
      #                     Time<Time_Cutoff, ],
      #          paste0(working.dir, "Data/", contract$symbol, "_", as.Date(Date), ".csv"))
      # }
      
      # remove redundant data
      # different time zone examples : "GMT", "PST8PDT", "Europe/London"
      Time_From=as.POSIXct(paste0(as.Date(format(Sys.time(), tz="PST8PDT"))-1, " 15:00:00"), tz="PST8PDT")
      Time_To=as.POSIXct(paste0(as.Date(format(Sys.time(), tz="PST8PDT")), " 15:00:00"), tz="PST8PDT")
      HistData=HistData[Time>=Time_From &
                          Time<Time_To, ]
      
      HistData[, Time:=as.POSIXct(format(as.POSIXct(Time), 
                                         tz="PST8PDT"), 
                                  tz="PST8PDT")]
      
      # save historical data up to today's market closed at 15:00:00 pm PDT
      fwrite(HistData,
             paste0(working.dir, "Data/", contract$symbol, "_", as.Date(format(Sys.time(), tz="PST8PDT")), ".csv"))
    }
    
    # write log everytime historical data is extracted and saved
    if(Log==T){
      if(file.exists(paste0(working.dir, "/Log/Daily_Hist_Data_Save.csv"))){
        Log=data.table(Time=Sys.time())
        Log=rbind(Log,
                  fread(paste0(working.dir, "/Log/Daily_Hist_Data_Save.csv")))
        fwrite(Log, paste0(working.dir, "/Log/Daily_Hist_Data_Save.csv"))
      }else{
        Log=data.table(Time=Sys.time())
        fwrite(Log, paste0(working.dir, "/Log/Daily_Hist_Data_Save.csv"))
      }
    }
  }else{
    message("No new data to save yet.")
    Sys.sleep(60*5)
  }
  
}





#*********************
#
# ReqRealTimeBars ----
#
#**************************
# request realtime bar data
#*******************************************
# output : BarData in the global environment
# return New_Data=1 if the new data is derived; and 0 if not
ReqRealTimeBars=function(BarSize=5, Log=F){
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
      print(tail(BarData, 1))
      
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


#*********************
#
# Import_HistData ----
#
#****************************************************
# import historical data saved in a repository folder
#******************************************************
# output : `5SecsBarHistData` in the global environment
Import_HistData=function(Location, Symbol, First_date, Last_date, Convert_Tz=F){
  # remove `5SecsBarHistData` in the global environment
  if(exists("5SecsBarHistData")){rm(`5SecsBarHistData`, envir=.GlobalEnv)}
  
  # import
  for(Date in seq(as.Date(First_date), Last_date, by="day")){
    File_name=paste0(Symbol, "_", as.Date(Date), ".csv")
    if(!file.exists(paste0(Location, File_name))){
      next
    }
    
    if(!exists("5SecsBarHistData", envir=.GlobalEnv)){
      `5SecsBarHistData`<<-fread(paste0(Location, File_name))
    }else{
      `5SecsBarHistData`<<-rbind(`5SecsBarHistData`,
                                 fread(paste0(Location, File_name)))
    }
  }
  
  # convert time zone
  # this process of converting time to the PDT time zone can be skipped as needed for less processing time
  if(Convert_Tz==T){
    `5SecsBarHistData`[, Time:=as.POSIXct(format(as.POSIXct(Time),
                                                 tz="PST8PDT"),
                                          tz="PST8PDT")]
  }
}





#******************
#
# Candle_Chart ----
#
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



#***************************
#
# Collapse_5SecsBarData ----
#
#***************************
# collapse 5 seconds bar data to a larger-sized bar data
#*******************************************************
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
Collapse_5SecsBarData=function(`5SecsBarData`, BarSize, Convert_Tz=F){
  
  if(BarSize==5){ # if BarSize=5, no additional process is required
    Collapsed_BarData=`5SecsBarData` %>% as.data.frame() %>% as.data.table()
    Collapsed_BarData[, Wap:=NULL]
  }else if(BarSize>5 & BarSize%%5==0){ # if BarSize is a multiple of 5
    `5SecsBarData`[, Group:=rep(1:(ceiling(nrow(`5SecsBarData`)/(BarSize/5))),
                                each=(BarSize/5))[1:nrow(`5SecsBarData`)]]
    # # generate Remainder to verify the process
    # `5SecsBarData`[, Remainder:=as.numeric(as.POSIXct(format(as.POSIXct(`5SecsBarData`$Time), 
    #                                                tz="PST8PDT"), 
    #                                         tz="PST8PDT"))%%30]
    Collapsed_BarData=`5SecsBarData`[, .(Symbol=unique(Symbol),
                                         Time=min(Time),
                                         Open=Open[Volume>0][Time==min(Time)], # open price at the earliest bar with non-zero volume
                                         High=max(High),
                                         Low=min(Low),
                                         Close=Close[Time==max(Time)],
                                         Volume=sum(Volume),
                                         Count=sum(Count)),
                                     by="Group"]
    
    Collapsed_BarData[, Group:=NULL]
    `5SecsBarData`[, Group:=NULL]
  }else{
    message("BarSize must be a multiple of 5")
  }
  
  # convert time zone
  # this process of converting time to the PDT time zone can be skipped as needed for less processing time
  if(Convert_Tz==T){
    Collapsed_BarData[, Time:=as.POSIXct(format(as.POSIXct(Time),
                                                tz="PST8PDT"),
                                         tz="PST8PDT")]
  }
  
  return(as.data.table(Collapsed_BarData))
}





#****************
#
# BBands_Sim ----
#
#**************************************************
# Fast simulation based on bollinger bands strategy
#**************************************************
BBands_Sim=function(Consec_Times, Long_PctB, Short_PctB, Commision=0.52){
  # Long_Pos_Ind : indice of rows to which a long position is filled
  # fill long positions if PctB is below Long_PctB for consecutive times (Consec_Times) in the recent bar data
  if(Consec_Times==1){
    Long_Pos_Ind=which(shift(Collapsed_BarData$PctB, 0)<Long_PctB)
  }else{
    Long_Pos_Ind=which(Reduce("+", lapply(shift(Collapsed_BarData$PctB, 0:(Consec_Times-1)),
                                          function(x) x<Long_PctB))==Consec_Times)
  }
  Short_Pos_Ind=which(Collapsed_BarData$PctB>Short_PctB) # a short position must be filled after a long position
  
  #
  Tradings=data.table(
    Collapsed_BarData[c(Long_Pos_Ind+1), 
                      .SD,
                      .SDcols=c("Time", "Open")],
    Collapsed_BarData[sapply(Long_Pos_Ind,
                             function(x) Short_Pos_Ind[which(x<Short_Pos_Ind)[1]])+1,
                      .SD,
                      .SDcols=c("Time", "Open")]
  )
  colnames(Tradings)=c("Long_Time", "Long_Price", "Short_Time", "Short_Price")
  
  # remove rows with duplicated short positions
  Tradings=Tradings[!duplicated(Tradings[,
                                         .SD,
                                         .SDcols=c("Short_Time", "Short_Price")])&
                      !is.na(Short_Price), ]
  
  # output
  Out=c()
  
  Out$Tradings=Tradings
  Out$Profit=sum(Tradings$Short_Price-Tradings$Long_Price, na.rm=T)*4*0.5
  Out$Commision=2*Commision*nrow(Tradings)
  Out$Net_Profit=Out$Profit-Out$Commision
  
  return(Out)
}





#********************
#
# Run_Simulation ----
#
#********************
# run a simulation
#*****************
Run_Simulation=function(Data_Params, Model_Param_Sets){
  # data parameters
  working.dir=Data_Params$working.dir
  BarSize=Data_Params$BarSize
  
  # assign local parameters
  Order_Direction=Model_Param_Sets$Order_Direction
  Max_Positions=Model_Param_Sets$Max_Positions
  Indicators=Model_Param_Sets$Indicators
  OrderType=Model_Param_Sets$OrderType
  Live_Data_Max_Rows=Model_Param_Sets$Live_Data_Max_Rows
  Models=Model_Param_Sets$Models
  Model_Params=Model_Param_Sets$Model_Params
  
  # model parameters
  Consec_Times=Model_Param_Sets$Model_Params$Simple_BBands["Consec_Times"]
  Long_PctB=Model_Param_Sets$Model_Params$Simple_BBands["Long_PctB"]
  Short_PctB=Model_Param_Sets$Model_Params$Simple_BBands["Short_PctB"]
  
  # import packages
  lapply(
    c(
      "IBrokers",
      "TTR",
      "data.table",
      "dplyr",
      "DescTools" # candle chart
    ), 
    checkpackages)
  
  # import data
  # output : `5SecsBarHistData`
  Import_HistData(Location=paste0(working.dir, "Data/"),
                  Symbol="MNQ",
                  First_date="2021-01-20",
                  Last_date=as.Date(format(Sys.time(), tz="PST8PDT")))
  
  # collapse data to the chosen-sized bar data
  Collapsed_BarData=Collapse_5SecsBarData(`5SecsBarHistData`,
                                          BarSize=BarSize)
  
  #
  if(Order_Direction=="both"){
    Long_Max_Positions=Short_Max_Positions=Max_Positions-1
  }else if(Order_Direction=="long"){
    Long_Max_Positions=Max_Positions-1
    Short_Max_Positions=-1
  }else if(Order_Direction=="short"){
    Long_Max_Positions=-1
    Short_Max_Positions=Max_Positions-1
  }
  
  #
  for(i in 1:nrow(Collapsed_BarData)){
    # i=90
    if(!exists("Live_Data")){
      Live_Data=Collapsed_BarData[i, ]
      Order_Transmit=data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                                Submit_Time=tail(Live_Data, 1)[, Time],
                                Filled_Time=tail(Live_Data, 1)[, Time],
                                Action="", 
                                TotalQuantity=0,
                                OrderType="MKT",
                                LmtPrice=0,
                                Fill=0)
    }else{
      Live_Data=rbind(Live_Data, Collapsed_BarData[i, ], fill=T) %>% tail(Live_Data_Max_Rows)
    }
    
    #*********************
    # calculate indicators
    #*********************
    # bollinger bands
    if("BBands"%in%Indicators){
      if(nrow(Live_Data)>19){
        BBands_Data=Live_Data[, BBands(Close)]
      }
    }
    
    # rsi
    if("RSI"%in%Indicators){
      if(nrow(Live_Data)>15){
        Live_Data[, RSI:=RSI(Close)]
      }
    }
    
    # macd
    if("MACD"%in%Indicators){
      if(nrow(Live_Data)>34){
        MACD_Data=Live_Data[, MACD(Close)]
      }
    }
    
    #***********
    # run models
    #***********
    # Simple_BBands
    if("Simple_BBands"%in%Models){
      Long_by_Simple_BBands=0
      Short_by_Simple_BBands=0
      
      if("BBands"%in%Indicators&
         exists("BBands_Data")){
        
        # determine position by pctB
        if(Order_Direction=="both"){
          Long_by_Simple_BBands=sum(tail(BBands_Data, Consec_Times)[,"pctB"]<=Long_PctB, na.rm=T)==Consec_Times
          Short_by_Simple_BBands=sum(tail(BBands_Data, Consec_Times)[,"pctB"]>=Short_PctB, na.rm=T)==Consec_Times
        }else if(Order_Direction=="long"){
          Long_by_Simple_BBands=sum(tail(BBands_Data, Consec_Times)[,"pctB"]<=Long_PctB, na.rm=T)==Consec_Times
          Short_by_Simple_BBands=sum(tail(BBands_Data, 1)[,"pctB"]>=Short_PctB, na.rm=T)==1
        }else if(Order_Direction=="short"){
          Long_by_Simple_BBands=sum(tail(BBands_Data, 1)[,"pctB"]<=Long_PctB, na.rm=T)==1
          Short_by_Simple_BBands=sum(tail(BBands_Data, Consec_Times)[,"pctB"]>=Short_PctB, na.rm=T)==Consec_Times
        }
        
      }else{
        if(!"BBands"%in%Indicators){
          stop("BBands required")
        }
      }
    }
    
    # Simple_RSI
    if("Simple_RSI"%in%Models){
      if(!"RSI"%in%Indicators){
        
      }else{
        
      }
    }
    
    
    
    #**************
    # fill position
    #**************
    # buy
    if(nrow(Order_Transmit[Action=="Buy"&Fill==0, ])>0){
      
      Unfilled_Buy_Position_Times=Order_Transmit[Action=="Buy"&Fill==0, Submit_Time]
      Unfilled_Buy_Position_Prices=Order_Transmit[Submit_Time%in%Unfilled_Buy_Position_Times, LmtPrice]
      Which_Buy_Position_to_Fill=which(tail(Live_Data, 1)[, Low]<Unfilled_Buy_Position_Prices)[1] # fill the earlier one among positions that have met the price criterion
      
      Order_Transmit[Submit_Time==Unfilled_Buy_Position_Times[Which_Buy_Position_to_Fill],
                     `:=`(Filled_Time=tail(Live_Data, 1)[, Time],
                          Fill=1)]
    }
    # sell
    if(nrow(Order_Transmit[Action=="Sell"&Fill==0, ])>0){
      
      Unfilled_Sell_Position_Times=Order_Transmit[Action=="Sell"&Fill==0, Submit_Time]
      Unfilled_Sell_Position_Prices=Order_Transmit[Submit_Time%in%Unfilled_Sell_Position_Times, LmtPrice]
      Which_Sell_Position_to_Fill=which(tail(Live_Data, 1)[, High]>Unfilled_Sell_Position_Prices)[1] # fill the earlier one among positions that have met the price criterion
      
      Order_Transmit[Submit_Time==Unfilled_Sell_Position_Times[Which_Sell_Position_to_Fill],
                     `:=`(Filled_Time=tail(Live_Data, 1)[, Time],
                          Fill=1)]
    }
    
    
    #******************
    # transmit position
    #******************
    # buy
    if(Long_by_Simple_BBands){
      # determine the position
      if(sum(Order_Transmit[Action=="Buy", TotalQuantity])-
         sum(Order_Transmit[Action=="Sell", TotalQuantity])<=
         (Long_Max_Positions)){ # the number of currently filled or transmitted long positions is limited to (Max_Positions + short positions)
        print(paste0("buy : ", i))
        Order_Transmit=rbind(Order_Transmit,
                             data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                                        Submit_Time=tail(Live_Data, 1)[, Time],
                                        Filled_Time=tail(Live_Data, 1)[, Time],
                                        Action="Buy",
                                        TotalQuantity=1,
                                        OrderType=OrderType,
                                        LmtPrice=tail(Live_Data, 1)[, Close],
                                        Fill=0))
      }
    }
    
    # sell
    if(Short_by_Simple_BBands){
      if(sum(Order_Transmit[Action=="Sell", TotalQuantity])-
         sum(Order_Transmit[Action=="Buy", TotalQuantity])<=
         (Short_Max_Positions)){ # the number of currently filled or transmitted short positions is limited to (Max_Positions + long positions)
        print(paste0("sell : ", i))
        Order_Transmit=rbind(Order_Transmit,
                             data.table(Symbol=tail(Live_Data, 1)[, Symbol],
                                        Submit_Time=tail(Live_Data, 1)[, Time],
                                        Filled_Time=tail(Live_Data, 1)[, Time],
                                        Action="Sell",
                                        TotalQuantity=1,
                                        OrderType=OrderType,
                                        LmtPrice=tail(Live_Data, 1)[, Close],
                                        Fill=0))
      }
    }
    
    
  }
  
  # order history
  Order_Transmit=Order_Transmit[-1, ] %>% as.data.table()
  
  
  #**********************
  # calculate the balance
  #**********************
  Collapse_Order_Transmit=cbind(Order_Transmit[Action=="Buy", 
                                               c("Filled_Time", "LmtPrice")],
                                Order_Transmit[Action=="Sell", 
                                               c("Filled_Time", "LmtPrice")])
  colnames(Collapse_Order_Transmit)=c("Buy_Time", "Buy_Price", "Sell_Time", "Sell_Price")
  Duplicated_Row=unique(c(which(duplicated(Collapse_Order_Transmit[, c("Buy_Time", "Buy_Price")])), 
                          which(duplicated(Collapse_Order_Transmit[, c("Sell_Time", "Sell_Price")]))))
  if(length(Duplicated_Row)>0){
    Collapse_Order_Transmit=Collapse_Order_Transmit[-Duplicated_Row, ]
  }
  Net_Profit=2*sum(Collapse_Order_Transmit[, Sell_Price-Buy_Price])-2*0.52*nrow(Collapse_Order_Transmit)
  
  
  return(list(Collapsed_BarData=Collapsed_BarData,
              Order_Transmit=Order_Transmit,
              Net_Profit=Net_Profit))
  
}





