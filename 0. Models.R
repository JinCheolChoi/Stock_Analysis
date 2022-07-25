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
                    Extent=5){
    if(!is.null(Data)){
      if(Interval<2){
        stop("Interval must be larger than 1.")
      }
      if(Interval<nrow(Data)){
        # estimate the linear trend from linear regression
        Linear_Model=lm(as.numeric(tail(Data[, "Close"], Interval))~seq(1:Interval))
        Trend=Linear_Model$coefficients[2]
        
        Long_Sig=FALSE
        Short_Sig=FALSE
        
        if(abs(Trend)>=Extent){
          # if there is an upward trend, short signal becomes positive with an expectation that the trend will soon turn bearish
          if(Trend>0){
            Long_Sig=FALSE
            Short_Sig=TRUE
          }else if(Trend==0){
            Long_Sig=FALSE
            Short_Sig=FALSE
          }else if(Trend<=0){
            Long_Sig=TRUE
            Short_Sig=FALSE
          }
          
        }
        
        # return signals
        return(c(Long_Sig, Short_Sig))
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
                    Short_PctB=1){
    # positive long signal if pctB<=Long_PctB in the past `Long_Consec_Times` consecutive times
    Long_Sig=sum(tail(Data, Long_Consec_Times)[, "pctB"]<=Long_PctB,
                 na.rm=T)==Long_Consec_Times
    
    # positive short signal if pctB>=Long_PctB in the past `Short_Consec_Times` consecutive times
    Short_Sig=sum(tail(Data, Short_Consec_Times)[, "pctB"]>=Short_PctB,
                  na.rm=T)==Short_Consec_Times
    
    # return signals
    return(c(Long_Sig, Short_Sig))
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
                    Short_PctB=1){
    # positive long signal if pctB<=Long_PctB in the past `Long_Consec_Times` consecutive times
    Long_Sig=sum(tail(Data, Long_Consec_Times)[, "pctB"]<=Long_PctB,
                 na.rm=T)==Long_Consec_Times
    
    # positive short signal if pctB>=Long_PctB in the past `Short_Consec_Times` consecutive times
    Short_Sig=sum(tail(Data, Short_Consec_Times)[, "pctB"]>=Short_PctB,
                  na.rm=T)==Short_Consec_Times
    
    # return signals
    return(c(Long_Sig, Short_Sig))
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
                    Short_RSI=70){
    # positive long signal if RSI<=Long_RSI in the past `Long_Consec_Times` consecutive times
    Long_Sig=sum(tail(Data, Long_Consec_Times)[, "RSI"]<=Long_RSI,
                 na.rm=T)==Long_Consec_Times
    
    # positive short signal if RSI<=Short_RSI in the past `Long_Consec_Times` consecutive times
    Short_Sig=sum(tail(Data, Short_Consec_Times)[, "RSI"]>=Short_RSI,
                  na.rm=T)==Short_Consec_Times
    
    # return signals
    return(c(Long_Sig, Short_Sig))
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
                    Short_RSI=70){
    # positive long signal if RSI<=Long_RSI in the past `Long_Consec_Times` consecutive times
    Long_Sig=sum(tail(Data, Long_Consec_Times)[, "RSI"]<=Long_RSI,
                 na.rm=T)==Long_Consec_Times
    
    # positive short signal if RSI<=Short_RSI in the past `Long_Consec_Times` consecutive times
    Short_Sig=sum(tail(Data, Short_Consec_Times)[, "RSI"]>=Short_RSI,
                  na.rm=T)==Short_Consec_Times
    
    # return signals
    return(c(Long_Sig, Short_Sig))
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







