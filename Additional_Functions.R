`twsExecution` <- 
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


# New utility function. Call immediately after a reqExecutions() call.
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