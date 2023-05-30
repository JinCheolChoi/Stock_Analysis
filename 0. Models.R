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
                    Simulation_Trading=FALSE){
    
    # Data=c(list(Calculated_Indicators_Combined),
    #        Models[[x]])[[1]]
    # Interval=c(list(Calculated_Indicators_Combined),
    #            Models[[x]])$Interval
    # Extent=c(list(Calculated_Indicators_Combined),
    #          Models[[x]])$Extent
    # Simulation_Trading=c(list(Calculated_Indicators_Combined),
    #                      Models[[x]])$Simulation_Trading
    
    if(!is.null(Data)){
      if(Interval<2){
        stop("Interval must be larger than 1.")
      }
      
      if(Interval<nrow(Data)){
        
        if(Simulation_Trading==FALSE){
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
          return(c(Long_Sig, Short_Sig))
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
          Signals$Long_Sig=Long_Sig
          Signals$Short_Sig=Short_Sig
          return(Signals)
        }
        
      }
      
    }
    
  }
)



#*********************
#
# Simple_BBands_1 ----
#
#*********************
Models_Env$Simple_BBands_1=list(
  Essential_Indicators=c("BBands"), # list of required indicators
  
  Function=function(Data,
                    Long_Consec_Times=1,
                    Short_Consec_Times=1,
                    Long_PctB=0,
                    Short_PctB=1,
                    Simulation_Trading=FALSE){
    
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
    # Simulation_Trading=c(list(Calculated_Indicators_Combined),
    #                      Models[[x]])$Simulation_Trading
    
    if(Simulation_Trading==FALSE){
      # positive long signal if pctB<=Long_PctB in the past `Long_Consec_Times` consecutive times
      Long_Sig=sum(tail(Data, Long_Consec_Times)[, "pctB"]<=Long_PctB,
                   na.rm=T)==Long_Consec_Times
      
      # positive short signal if pctB>=Long_PctB in the past `Short_Consec_Times` consecutive times
      Short_Sig=sum(tail(Data, Short_Consec_Times)[, "pctB"]>=Short_PctB,
                    na.rm=T)==Short_Consec_Times
      
      # return signals
      return(c(Long_Sig, Short_Sig))
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
      Signals$Long_Sig=Long_Sig
      Signals$Short_Sig=Short_Sig
      return(Signals)
    }
  }
)


#*********************
#
# Simple_BBands_2 ----
#
#**********************
Models_Env$Simple_BBands_2=list(
  Essential_Indicators=c("BBands"), # list of required indicators
  
  Function=function(Data,
                    Long_Consec_Times=1,
                    Short_Consec_Times=1,
                    Long_PctB=0,
                    Short_PctB=1,
                    Simulation_Trading=FALSE){
    if(Simulation_Trading==FALSE){
      # positive long signal if pctB<=Long_PctB in the past `Long_Consec_Times` consecutive times
      Long_Sig=sum(tail(Data, Long_Consec_Times)[, "pctB"]<=Long_PctB,
                   na.rm=T)==Long_Consec_Times
      
      # positive short signal if pctB>=Long_PctB in the past `Short_Consec_Times` consecutive times
      Short_Sig=sum(tail(Data, Short_Consec_Times)[, "pctB"]>=Short_PctB,
                    na.rm=T)==Short_Consec_Times
      
      # return signals
      return(c(Long_Sig, Short_Sig))
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
      Signals$Long_Sig=Long_Sig
      Signals$Short_Sig=Short_Sig
      return(Signals)
    }
  }
)





#******************
#
# Simple_RSI_1 ----
#
#******************
Models_Env$Simple_RSI_1=list(
  Essential_Indicators=c("RSI"), # list of required indicators
  
  Function=function(Data,
                    Long_Consec_Times=1,
                    Short_Consec_Times=1,
                    Long_RSI=30,
                    Short_RSI=70,
                    Simulation_Trading=FALSE){
    
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
    # Simulation_Trading=c(list(Calculated_Indicators_Combined),
    #                      Models[[x]])$Simulation_Trading
    
    if(Simulation_Trading==FALSE){
      # positive long signal if RSI<=Long_RSI in the past `Long_Consec_Times` consecutive times
      Long_Sig=sum(tail(Data, Long_Consec_Times)[, "RSI"]<=Long_RSI,
                   na.rm=T)==Long_Consec_Times
      
      # positive short signal if RSI<=Short_RSI in the past `Long_Consec_Times` consecutive times
      Short_Sig=sum(tail(Data, Short_Consec_Times)[, "RSI"]>=Short_RSI,
                    na.rm=T)==Short_Consec_Times
      
      # return signals
      return(c(Long_Sig, Short_Sig))
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
      Signals$Long_Sig=Long_Sig
      Signals$Short_Sig=Short_Sig
      return(Signals)
    }
  }
) 





#******************
#
# Simple_RSI_2 ----
#
#******************
Models_Env$Simple_RSI_2=list(
  Essential_Indicators=c("RSI"), # list of required indicators
  
  Function=function(Data,
                    Long_Consec_Times=1,
                    Short_Consec_Times=1,
                    Long_RSI=30,
                    Short_RSI=70,
                    Simulation_Trading=FALSE){
    if(Simulation_Trading==FALSE){
      # positive long signal if RSI<=Long_RSI in the past `Long_Consec_Times` consecutive times
      Long_Sig=sum(tail(Data, Long_Consec_Times)[, "RSI"]<=Long_RSI,
                   na.rm=T)==Long_Consec_Times
      
      # positive short signal if RSI<=Short_RSI in the past `Long_Consec_Times` consecutive times
      Short_Sig=sum(tail(Data, Short_Consec_Times)[, "RSI"]>=Short_RSI,
                    na.rm=T)==Short_Consec_Times
      
      # return signals
      return(c(Long_Sig, Short_Sig))
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
      Signals$Long_Sig=Long_Sig
      Signals$Short_Sig=Short_Sig
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







