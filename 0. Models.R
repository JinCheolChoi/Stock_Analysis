#**************************************
#
# create an environment for models ----
#
#**************************************
Models_Env=new.env()





#*******************
#
# Simple_BBands ----
#
#*******************
Models_Env$Simple_BBands=list(
  Essential_Indicators=c("BBands"), # list of required indicators
  
  Function=function(BBands_Data,
                    Long_Consec_Times=1,
                    Short_Consec_Times=1,
                    Long_PctB=0,
                    Short_PctB=1){
    # positive long signal if pctB<=Long_PctB in the past `Long_Consec_Times` consecutive times
    Long_Sig_by_Simple_BBands=sum(tail(BBands_Data, Long_Consec_Times)[, "pctB"]<=Long_PctB,
                                  na.rm=T)==Long_Consec_Times
    
    # positive short signal if pctB<=Long_PctB in the past `Long_Consec_Times` consecutive times
    Short_Sig_by_Simple_BBands=sum(tail(BBands_Data, Short_Consec_Times)[, "pctB"]>=Short_PctB,
                                   na.rm=T)==Short_Consec_Times
    
    # return signals
    return(c(Long_Sig_by_Simple_BBands, Short_Sig_by_Simple_BBands))
  }
)





#*******************
#
# Simple_RSI ----
#
#*******************
Models_Env$Simple_RSI=list(
  Essential_Indicators=c("BBands"), # list of required indicators
  
  Function=function(BBands_Data,
                    Long_Consec_Times=1,
                    Short_Consec_Times=1,
                    Long_PctB=0,
                    Short_PctB=1){
    # positive long signal if pctB<=Long_PctB in the past `Long_Consec_Times` consecutive times
    Long_Sig_by_Simple_BBands=sum(tail(BBands_Data, Long_Consec_Times)[, "pctB"]<=Long_PctB,
                                  na.rm=T)==Long_Consec_Times
    
    # positive short signal if pctB<=Long_PctB in the past `Long_Consec_Times` consecutive times
    Short_Sig_by_Simple_BBands=sum(tail(BBands_Data, Short_Consec_Times)[, "pctB"]>=Short_PctB,
                                   na.rm=T)==Short_Consec_Times
    
    # return signals
    return(c(Long_Sig_by_Simple_BBands, Short_Sig_by_Simple_BBands))
  }
)





#*****************
#
# Simple_Test ----
#
#*****************
Models_Env$Simple_Test=list(
  Essential_Indicators=c("BBands", "RSI"), # list of required indicators
  
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







