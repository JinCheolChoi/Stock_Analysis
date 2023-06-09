#**************************************
#
# create an environment for models ----
#
#**************************************
Models_Env=new.env()





#***********
#
# Trend ----
#
#***********
Models_Env$Trend=list(
  Essential_Indicators=c(), # list of required indicators
  
  Function=function(Data,
                    Interval=5,
                    Extent=5,
                    Live_Trading=TRUE,
                    Reverse=FALSE){
    
    # Data=c(list(Calculated_Indicators_Combined),
    #        Models[[x]])[[1]]
    # Interval=c(list(Calculated_Indicators_Combined),
    #            Models[[x]])$Interval
    # Extent=c(list(Calculated_Indicators_Combined),
    #          Models[[x]])$Extent
    # Live_Trading=c(list(Calculated_Indicators_Combined),
    #                      Models[[x]])$Live_Trading
    
    if(!is.null(Data)){
      if(Interval<2){
        stop("Interval must be larger than 1.")
      }
      
      if(Interval<nrow(Data)){
        
        if(Live_Trading==TRUE){
          # estimate the linear trend from linear regression
          Linear_Model=lm(as.numeric(tail(Data[, "Close"], Interval))~seq(1:Interval))
          Trend=Linear_Model$coefficients[2]
          
          Long_Sig=FALSE
          Short_Sig=FALSE
          
          if(abs(Trend)>=Extent){
            # if there is an upward trend, short signal becomes positive with an expectation that the trend will soon turn bearish
            if(Trend<0){
              Long_Sig=FALSE
              Short_Sig=TRUE
            }else if(Trend==0){
              Long_Sig=FALSE
              Short_Sig=FALSE
            }else if(Trend>0){
              Long_Sig=TRUE
              Short_Sig=FALSE
            }
            
          }
          
          # return signals
          if(Reverse==TRUE){
            return(c(Short_Sig, Long_Sig))
          }else{
            return(c(Long_Sig, Short_Sig))
          }
        }else{
          
          # estimate the linear trend from linear regression
          Trends=sapply(c(1:(length(Data[, "Close"])-Interval+1)),
                        function(x) lm(as.numeric(Data[x:(x+Interval-1), "Close"])~seq(1:Interval))$coefficients[2])
          
          # if there is an upward trend, short signal becomes positive with an expectation that the trend will soon turn bearish
          Long_Sig=rep(NA, nrow(Data))
          Short_Sig=rep(NA, nrow(Data))
          
          Long_Sig[(which((abs(Trends)>=Extent)&
                            (Trends<0))+Interval-1)]=FALSE
          Short_Sig[(which((abs(Trends)>=Extent)&
                             (Trends<0))+Interval-1)]=TRUE
          
          Long_Sig[(which((abs(Trends)>=Extent)&
                            (Trends==0))+Interval-1)]=FALSE
          Short_Sig[(which((abs(Trends)>=Extent)&
                             (Trends==0))+Interval-1)]=FALSE
          
          Long_Sig[(which((abs(Trends)>=Extent)&
                            (Trends>0))+Interval-1)]=TRUE
          Short_Sig[(which((abs(Trends)>=Extent)&
                             (Trends>0))+Interval-1)]=FALSE
          
          # return signals
          Signals=c()
          if(Reverse==TRUE){
            Signals$Long_Sig=Short_Sig
            Signals$Short_Sig=Long_Sig
          }else{
            Signals$Long_Sig=Long_Sig
            Signals$Short_Sig=Short_Sig
          }
          return(Signals)
        }
        
      }
      
    }
    
  }
)





#*******************
#
# Simple_BBands ----
#
#*******************
Models_Env$Simple_BBands=list(
  Essential_Indicators=c("BBands"), # list of required indicators
  
  Function=function(Data,
                    Long_Consec_Times=1,
                    Short_Consec_Times=1,
                    Long_PctB=0,
                    Short_PctB=1,
                    Live_Trading=TRUE,
                    Reverse=FALSE){
    
    # Data=c(list(Calculated_Indicators_Combined),
    #        Models[[x]])[[1]]
    # Long_Consec_Times=c(list(Calculated_Indicators_Combined),
    #                     Models[[x]])$Long_Consec_Times
    # Short_Consec_Times=c(list(Calculated_Indicators_Combined),
    #                      Models[[x]])$Short_Consec_Times
    # Long_PctB=c(list(Calculated_Indicators_Combined),
    #             Models[[x]])$Long_PctB
    # Short_PctB=c(list(Calculated_Indicators_Combined),
    #              Models[[x]])$Short_PctB
    # Live_Trading=c(list(Calculated_Indicators_Combined),
    #                      Models[[x]])$Live_Trading
    
    if(Live_Trading==TRUE){
      # positive long signal if pctB<=Long_PctB in the past `Long_Consec_Times` consecutive times
      Long_Sig=sum(tail(Data, Long_Consec_Times)[, "pctB"]<=Long_PctB,
                   na.rm=T)==Long_Consec_Times
      
      # positive short signal if pctB>=Long_PctB in the past `Short_Consec_Times` consecutive times
      Short_Sig=sum(tail(Data, Short_Consec_Times)[, "pctB"]>=Short_PctB,
                    na.rm=T)==Short_Consec_Times
      
      # return signals
      if(Reverse==TRUE){
        return(c(Short_Sig, Long_Sig))
      }else{
        return(c(Long_Sig, Short_Sig))
      }
    }else{
      # positive long signal if pctB<=Long_PctB in the past `Long_Consec_Times` consecutive times
      # Long_Sig=cbind(Data[, "pctB"],
      #                Data[, "pctB"]<=Long_PctB,
      #                c(rep(NA, length=Long_Consec_Times-1),
      #                  RcppRoll::roll_sum(Data[, "pctB"]<=Long_PctB, Long_Consec_Times)))
      Long_Sig=c(rep(NA, length=Long_Consec_Times-1),
                 RcppRoll::roll_sum(Data[, "pctB"]<=Long_PctB, Long_Consec_Times, na.rm=T))==Long_Consec_Times
      # positive short signal if pctB>=Long_PctB in the past `Short_Consec_Times` consecutive times
      # Short_Sig=cbind(Data[, "pctB"],
      #                Data[, "pctB"]<=Short_PctB,
      #                c(rep(NA, length=Short_Consec_Times-1),
      #                  RcppRoll::roll_sum(Data[, "pctB"]<=Short_PctB, Short_Consec_Times)))
      Short_Sig=c(rep(NA, length=Short_Consec_Times-1),
                  RcppRoll::roll_sum(Data[, "pctB"]>=Short_PctB, Short_Consec_Times, na.rm=T))==Short_Consec_Times
      
      # return signals
      Signals=c()
      if(Reverse==TRUE){
        Signals$Long_Sig=Short_Sig
        Signals$Short_Sig=Long_Sig
      }else{
        Signals$Long_Sig=Long_Sig
        Signals$Short_Sig=Short_Sig
      }
      return(Signals)
    }
  }
)





#****************
#
# Simple_RSI ----
#
#****************
Models_Env$Simple_RSI=list(
  Essential_Indicators=c("RSI"), # list of required indicators
  
  Function=function(Data,
                    Long_Consec_Times=1,
                    Short_Consec_Times=1,
                    Long_RSI=30,
                    Short_RSI=70,
                    Live_Trading=TRUE,
                    Reverse=FALSE){
    
    # Data=c(list(Calculated_Indicators_Combined),
    #        Models[[x]])[[1]]
    # Long_Consec_Times=c(list(Calculated_Indicators_Combined),
    #                     Models[[x]])$Long_Consec_Times
    # Short_Consec_Times=c(list(Calculated_Indicators_Combined),
    #                      Models[[x]])$Short_Consec_Times
    # Long_RSI=c(list(Calculated_Indicators_Combined),
    #             Models[[x]])$Long_RSI
    # Short_RSI=c(list(Calculated_Indicators_Combined),
    #              Models[[x]])$Short_RSI
    # Live_Trading=c(list(Calculated_Indicators_Combined),
    #                      Models[[x]])$Live_Trading
    
    if(Live_Trading==TRUE){
      # positive long signal if RSI<=Long_RSI in the past `Long_Consec_Times` consecutive times
      Long_Sig=sum(tail(Data, Long_Consec_Times)[, "RSI"]<=Long_RSI,
                   na.rm=T)==Long_Consec_Times
      
      # positive short signal if RSI<=Short_RSI in the past `Long_Consec_Times` consecutive times
      Short_Sig=sum(tail(Data, Short_Consec_Times)[, "RSI"]>=Short_RSI,
                    na.rm=T)==Short_Consec_Times
      
      # return signals
      if(Reverse==TRUE){
        return(c(Short_Sig, Long_Sig))
      }else{
        return(c(Long_Sig, Short_Sig))
      }
    }else{
      # positive long signal if RSI<=Long_RSI in the past `Long_Consec_Times` consecutive times
      # Long_Sig=cbind(Data[, "RSI"],
      #                Data[, "RSI"]<=Long_RSI,
      #                c(rep(NA, length=Long_Consec_Times-1),
      #                  RcppRoll::roll_sum(Data[, "RSI"]<=Long_RSI, Long_Consec_Times)))
      Long_Sig=c(rep(NA, length=Long_Consec_Times-1),
                 RcppRoll::roll_sum(Data[, "RSI"]<=Long_RSI, Long_Consec_Times, na.rm=T))==Long_Consec_Times
      # positive short signal if RSI<=Short_RSI in the past `Long_Consec_Times` consecutive times
      # Short_Sig=cbind(Data[, "RSI"],
      #                Data[, "RSI"]<=Short_RSI,
      #                c(rep(NA, length=Short_Consec_Times-1),
      #                  RcppRoll::roll_sum(Data[, "RSI"]<=Short_RSI, Short_Consec_Times)))
      Short_Sig=c(rep(NA, length=Short_Consec_Times-1),
                  RcppRoll::roll_sum(Data[, "RSI"]>=Short_RSI, Short_Consec_Times, na.rm=T))==Short_Consec_Times
      
      # return signals
      Signals=c()
      if(Reverse==TRUE){
        Signals$Long_Sig=Short_Sig
        Signals$Short_Sig=Long_Sig
      }else{
        Signals$Long_Sig=Long_Sig
        Signals$Short_Sig=Short_Sig
      }
      return(Signals)
    }
  }
)





#***********************
#
# RSI_Averages_Band ----
#
#***********************
Models_Env$RSI_Averages_Band=list(
  Essential_Indicators=c("RSI"), # list of required indicators
  
  Function=function(Data,
                    MA_Length=4, # the number of lagged RSIs to look back to calculate the moving average RSI value
                    RSI_RSI_MA_Diff_Min=0, # the minimum difference between RSI and the moving average RSI value to enter the position
                    RSI_RSI_MA_Diff_Max=Inf, # the maximum difference between RSI and the moving average RSI value to enter the position
                    Early_Execution_Gap=Inf, # the minimum difference between RSI and the moving average RSI value to close the position
                                             # or, this can allow to enter the opposite position when there is currently no filled order
                    Live_Trading=TRUE,
                    Reverse=FALSE){
    
    # Data=c(list(Calculated_Indicators_Combined),
    #        Models[[x]])[[1]]
    # MA_Length=c(list(Calculated_Indicators_Combined),
    #             Models[[x]])$MA_Length
    # RSI_RSI_MA_Diff_Min=c(list(Calculated_Indicators_Combined),
    #                       Models[[x]])$RSI_RSI_MA_Diff_Min
    # RSI_RSI_MA_Diff_Max=c(list(Calculated_Indicators_Combined),
    #                       Models[[x]])$RSI_RSI_MA_Diff_Max
    # Early_Execution_Gap=c(list(Calculated_Indicators_Combined),
    #                       Models[[x]])$Early_Execution_Gap
    # Live_Trading=c(list(Calculated_Indicators_Combined),
    #                Models[[x]])$Live_Trading
    
    
    # #*****
    # # plot
    # #*****
    # start_ind=20
    # end_ind=400
    # Calculated_Indicators$RSI[start_ind:end_ind]
    # par(mfrow=c(2, 1))
    # plot(Calculated_Indicators$Close[start_ind:end_ind],
    #       type="l")
    # plot(Calculated_Indicators$RSI[start_ind:end_ind],
    #      type="l")
    # 
    # # 1
    # lines(RcppRoll::roll_meanr(Calculated_Indicators[["RSI"]],
    #                            16,
    #                            align="right")[start_ind:end_ind],
    #       type="l",
    #       col="red")
    # 
    # # 2
    # lines(RcppRoll::roll_meanr(Calculated_Indicators[["RSI"]],
    #                            48,
    #                            align="right")[start_ind:end_ind],
    #       type="l",
    #       col="blue")
    # Long_Signals[start_ind:end_ind,]
    # Short_Signals[start_ind:end_ind, ]
    
    if(Live_Trading==TRUE){
      RSI_MA=mean(tail(Data[, "RSI"], MA_Length))
      
      # positive long signal if the current RSI > moving average of RSIs in the past MA_Length points
      Long_Sig=(Data[, "RSI"]>(RSI_MA+abs(RSI_RSI_MA_Diff_Min))) & (Data[, "RSI"]<(RSI_MA+abs(RSI_RSI_MA_Diff_Max)))
      # early execution to close the short position by buying it back
      Long_Sig=(Long_Sig | (RSI_MA>(Data[, "RSI"]+abs(Early_Execution_Gap))))
      
      # positive short signal if the current RSI < moving average of RSIs in the past MA_Length points
      Short_Sig=(RSI_MA>Data[, "RSI"]+abs(RSI_RSI_MA_Diff_Min)) & (RSI_MA<Data[, "RSI"]+abs(RSI_RSI_MA_Diff_Max))
      # early execution to close the long position by selling it
      Short_Sig=(Short_Sig | (Data[, "RSI"]>(RSI_MA+abs(Early_Execution_Gap))))
      
      # return signals
      if(Reverse==TRUE){
        return(c(Short_Sig, Long_Sig))
      }else{
        return(c(Long_Sig, Short_Sig))
      }
    }else{
      RSI_MA=RcppRoll::roll_meanr(Data[, "RSI"],
                                  MA_Length,
                                  align="right")
      
      # positive long signal if the current RSI > moving average of RSIs in the past MA_Length points
      Long_Sig=(Data[, "RSI"]>(RSI_MA+abs(RSI_RSI_MA_Diff_Min))) & (Data[, "RSI"]<(RSI_MA+abs(RSI_RSI_MA_Diff_Max)))
      # early execution to close the short position by buying it back
      Long_Sig=(Long_Sig | (RSI_MA>(Data[, "RSI"]+abs(Early_Execution_Gap))))
      
      # positive short signal if the current RSI < moving average of RSIs in the past MA_Length points
      Short_Sig=(RSI_MA>Data[, "RSI"]+abs(RSI_RSI_MA_Diff_Min)) & (RSI_MA<Data[, "RSI"]+abs(RSI_RSI_MA_Diff_Max))
      # early execution to close the long position by selling it
      Short_Sig=(Short_Sig | (Data[, "RSI"]>(RSI_MA+abs(Early_Execution_Gap))))
      
      # return signals
      Signals=c()
      if(Reverse==TRUE){
        Signals$Long_Sig=Short_Sig
        Signals$Short_Sig=Long_Sig
      }else{
        Signals$Long_Sig=Long_Sig
        Signals$Short_Sig=Short_Sig
      }
      return(Signals)
    }
  }
)




#*****************
#
# Simple_Test ----
#
#*****************
Models_Env$Simple_Test=list(
  Essential_Indicators=c("BBands"), # list of required indicators
  
  Function=function(Data,
                    x=1,
                    y=1){
    if(nrow(Data)>50){
      output=c(T, F)
    }else{
      output=c(F, F)
    }
    
    # return signals
    return(output)
  }
)







