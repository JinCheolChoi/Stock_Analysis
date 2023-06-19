#********************
#
# empty the workspace
#
#********************
rm(list=ls())


#***********
#
# parameters
#
#******************
# working directory
#******************
Device="laptop" # or "desktop"

if(Device=="desktop"){
  # desktop
  working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/"
  data.dir="E:/Stock_Data/" # upper folder that has a folder storing stock data
  rdata.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/"
  
  source(paste0("C:/Users/JinCheol Choi/Desktop/R/Functions/Functions.R")) # desktop
}else if(Device=="laptop"){
  # laptop
  working.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis/"
  data.dir="C:/Users/jchoi02/Desktop/Data/" # upper folder that has a folder storing stock data
  rdata.dir="C:/Users/jchoi02/Desktop/R/Stock_Analysis_Daily_Data/Rdata/"
  
  source(paste0("C:/Users/jchoi02/Desktop/R/Functions/Functions.R"))
}


#****************
# data parameters
#****************
Symbols=c("MNQ")

#*****************
#
# preliminary step
#
#*******************
# load functions
source(paste0(working.dir, "0. Stock_Analysis_Functions.R"))

# import libraries
for(pack in c("IBrokers",
              "TTR",
              "data.table",
              "dplyr",
              "DescTools", # candle chart
              
              "RcppRoll",
              "Rcpp",
              "RcppArmadillo",
              "bench",
              "tseries")){ 
  lapply(pack, checkpackages)
}

# Barsize
Barsizes=c(5,
           60,
           60*5,
           60*15,
           60*30,
           60*60)
for(Barsize in Barsizes){
  switch(as.character(Barsize),
         "5"={folder_name="5secs"},
         "60"={folder_name="1min"},
         "300"={folder_name="5mins"},
         "900"={folder_name="15mins"},
         "1800"={folder_name="30mins"},
         "3600"={folder_name="60mins"}
  )
  
  for(Symbol in Symbols){
    # import data
    Get_Data(Symbols=Symbol,
             Data_Dir=data.dir,
             BarSize=Barsize,
             Convert_Tz=F,
             Filter=F)
    
    if(Device=="desktop"){
      # desktop
      fwrite(MNQ,
             paste0("E:/Stock_Data/", folder_name, "/", Symbols, "/", Symbols, ".csv"))
      fwrite(MNQ,
             paste0("C:/Users/JinCheol Choi/Desktop/Stock_Data/", folder_name, "/", Symbols, "/", Symbols, ".csv"))
    }else if(Device=="laptop"){
      # laptop
      fwrite(MNQ,
             paste0("C:/Users/jchoi02/Desktop/Data/", folder_name, "/", Symbols, "/", Symbols, ".csv"))
    }
    
  }
}
