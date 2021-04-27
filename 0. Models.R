#*******************
#
# Simple_BBands ----
#
#*******************
Simple_BBands=function(BBands_Data, Long_Consec_Times, Short_Consec_Times, Long_PctB, Short_PctB){
  Long_Sig_by_Simple_BBands=sum(tail(BBands_Data, Long_Consec_Times)[,"pctB"]<=Long_PctB, na.rm=T)==Long_Consec_Times
  Short_Sig_by_Simple_BBands=sum(tail(BBands_Data, Short_Consec_Times)[,"pctB"]>=Short_PctB, na.rm=T)==Short_Consec_Times
  
  return(c(Long_Sig_by_Simple_BBands, Short_Sig_by_Simple_BBands))
}













