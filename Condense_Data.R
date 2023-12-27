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
Device="desktop" # or "laptop"

if(Device=="desktop"){
  # desktop
  working.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis/"
  data.dir="E:/Stock_Data/" # upper folder that has a folder storing stock data
  rdata.dir="C:/Users/JinCheol Choi/Desktop/R/Stock_Analysis_Daily_Data/Rdata/"
  
  source(paste0("C:/Users/JinCheol Choi/Desktop/R/Functions/Functions.R"))
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
Symbols=c("IWM",
          "M2K",
          "MNQ")

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

# BarSize
BarSizes=c(5,
           60,
           60*5,
           60*15,
           60*30,
           60*60)

# import data
Get_Data(Symbols=Symbols,
         Data_Dir=data.dir,
         BarSizes=BarSizes,
         Convert_Tz=F,
         Filter=F)

# save the files
for(Symbol in Symbols){
  for(BarSize in BarSizes){
    switch(as.character(BarSize),
           "5"={folder_name="5secs"},
           "60"={folder_name="1min"},
           "300"={folder_name="5mins"},
           "900"={folder_name="15mins"},
           "1800"={folder_name="30mins"},
           "3600"={folder_name="60mins"}
    )
    
    # if there is a value named as paste0(Symbol, "_", BarSize) in the global environment , export it
    if(paste0(Symbol, "_", BarSize)%in%ls(envir=.GlobalEnv)){
      # desktop
      if(Device=="desktop"){
        # create a folder if not exist
        if(!dir.exists(paste0("E:/Stock_Data/", folder_name, "/", Symbol, "/"))){
          dir.create(paste0("E:/Stock_Data/", folder_name, "/", Symbol, "/")) 
        }
        if(!dir.exists(paste0("C:/Users/JinCheol Choi/Desktop/Stock_Data/", folder_name, "/", Symbol, "/"))){
          dir.create(paste0("C:/Users/JinCheol Choi/Desktop/Stock_Data/", folder_name, "/", Symbol, "/")) 
        }
        
        # save the file
        fwrite(get(paste0(Symbol, "_", BarSize)),
               paste0("E:/Stock_Data/", folder_name, "/", Symbol, "/", Symbol, ".csv"))
        fwrite(get(paste0(Symbol, "_", BarSize)),
               paste0("C:/Users/JinCheol Choi/Desktop/Stock_Data/", folder_name, "/", Symbol, "/", Symbol, ".csv"))
      }else if(Device=="laptop"){
        # create a folder if not exist
        if(!dir.exists(paste0("C:/Users/jchoi02/Desktop/Data/", folder_name, "/", Symbol, "/"))){
          dir.create(paste0("C:/Users/jchoi02/Desktop/Data/", folder_name, "/", Symbol, "/")) 
        }
        
        # save the file
        fwrite(get(paste0(Symbol, "_", BarSize)),
               paste0("C:/Users/jchoi02/Desktop/Data/", folder_name, "/", Symbol, "/", Symbol, ".csv"))
      }
      
      print(paste0("saved : ", Symbol, " - ", folder_name))
    }
  }
}

