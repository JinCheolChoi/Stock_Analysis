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
    rep.ind=1 # # # #
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
        
        #*****************************
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
System_Break=function(){
  if(as.ITime(format(Sys.time(), tz="PST8PDT"))>=(as.ITime("13:15:00")-60*5)& # if time is between 13:10:00 and 13:15:00 PDT
     as.ITime(format(Sys.time(), tz="PST8PDT"))<=(as.ITime("13:15:00"))){
    # (1) for 25 mins from 13:10:00 to 13:35:00 PDT (market close : 13:15:00 to 13:30:00 PDT)
    Sys.sleep(60*25)
    
    # if connection is lost, reconnect
    while(!isConnected(tws)){
      tws=twsConnect(port=7497)
    }
  }else if(as.ITime(format(Sys.time(), tz="PST8PDT"))>=(as.ITime("14:00:00")-60*5)& # if time is between 13:55:00 and 14:00:00 PDT
           as.ITime(format(Sys.time(), tz="PST8PDT"))<=(as.ITime("14:00:00"))){
    # (2) for 70 mins from 13:55:00 to 15:05:00 PDT (market close : 14:00:00 to 15:00:00 PDT)
    Sys.sleep(60*70)
    
    # if connection is lost, reconnect
    while(!isConnected(tws)){
      tws=twsConnect(port=7497)
    }
    
    # execute a daily save of 5 second bar data afterwards
    Daily_Hist_Data_Save()
    
  }
}



#**************************
#
# Daily_Hist_Data_Save ----
#
#**************************
# execute a daily save of 5 second bar data at 15:00:00 pm PDT
Daily_Hist_Data_Save=function(){
  # request historical data of 5 seconds bar
  HistData=as.data.table(reqHistoricalData(tws, contract, barSize="5 secs", duration="2 D", useRTH="0")) # useRTH="0" : not limited to regular trading hours
  colnames(HistData)=c("Time", "Open", "High", "Low", "Close", "Volume", "Wap", "hasGaps", "Count")
  
  HistData[, hasGaps:=NULL] # hasGaps is redundant
  
  HistData=data.table(Symbol=contract$symbol,
                      HistData)
  
  # remove redundant data
  # different time zone examples : "GMT", "PST8PDT", "Europe/London"
  Time_Cutoff=as.POSIXct(paste0(as.Date(format(Sys.time(), tz="PST8PDT")), " 15:00:00"), tz="PST8PDT")
  HistData=HistData[Time<Time_Cutoff, ]
  HistData[, Time:=as.POSIXct(format(as.POSIXct(Time), 
                                     tz="PST8PDT"), 
                              tz="PST8PDT")]
  
  # save historical data up to today's market closed at 15:00:00 pm PDT
  if(!file.exists(paste0(working.dir, "Data/", contract$symbol, "_", Sys.Date(), ".csv"))){
    fwrite(HistData,
           paste0(working.dir, "Data/", contract$symbol, "_", Sys.Date(), ".csv"))
  }else if(file.exists(paste0(working.dir, "Data/", contract$symbol, "_", Sys.Date(), ".csv"))){ 
    # if a file already exists for this symbol, combine it with the newly extract historical data
    fwrite(
      unique(
        rbind(fread(paste0(working.dir, "Data/", contract$symbol, "_", Sys.Date(), ".csv")),
              HistData)
      ),
      paste0(working.dir, "Data/", contract$symbol, "_", Sys.Date(), ".csv"))
  }
}







