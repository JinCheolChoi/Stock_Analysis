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
Device="laptop" # "laptop" or "desktop"

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
              "tseries",
              
              "parallel")){ 
  lapply(pack, checkpackages)
}

library(mgcv)
library(ggplot2)
library(gratia)

# bar size
BarSize="1min"

# import data
BarData=fread(paste0(data.dir, BarSize, "/", Symbols, "/", Symbols, ".csv"))
BarData[, Time:=as.POSIXct(format(as.POSIXct(Time), tz="America/Los_Angeles"),
                           tz="America/Los_Angeles")]

BarData[, Ind:=.I]

dat=copy(BarData[1:100, .SD, .SDcols = c("Ind", "Close")])


#
dat=data.table(dat)
dat[, Close:=scale(Close)]
eps = 10000 # numeric; the value of the finite difference used to approximate the first derivative.
smooth_degree=0.8 # between 0 and 1

# bs="tp" or "cr" or "ps"
b1 = mgcv::gam(Close ~ s(Ind, bs='tp', k=(nrow(dat)*(1-smooth_degree)-1), fx=TRUE), data = dat)

# fitted plot
ggplot(dat, aes(x = Ind, y = Close)) +
  geom_point() +
  geom_line() +
  geom_line(colour = "red", size = 0.8, aes(y = fitted(b1))) +
  theme_bw()

{
  # finite difference approximation of derivatives
  # first derivative
  newDF = with(dat, data.frame(Ind = seq(min(Ind), max(Ind), length=eps)))
  difference_app_eps = 1e-3
  X0 = predict(b1, newDF)
  
  # new_dat
  new_dat=data.table(
    newDF,
    Close=X0
  )
  
  newDFeps_p = newDF + difference_app_eps
  X1 = predict(b1, newDFeps_p)
  fd_d1 = (X1 - X0) / difference_app_eps # fd_d1=fderiv(b1, n = 1000)
  
  # second derivative
  newDFeps_m = newDF - difference_app_eps
  X_1 = predict(b1, newDFeps_m)
  fd_d2 = (X1 + X_1 - 2*X0) / difference_app_eps^2
}

plot(b1$fit, type='l')

plot(fd_d1, type='l')
abline(h=0)

plot(fd_d2, type='l')
abline(h=0)

Optimizations=data.table(
  Ind=new_dat$Ind,
  fd_d1=fd_d1,
  fd_d2=fd_d2
)

Optimizations=Optimizations[fd_d1<0.01 & fd_d1> -0.01, ]
Optimizations[, Group:=c(0, cumsum(!diff(Optimizations[, Ind])<=1))]
Optimizations[, `:=`(Opt_Ind=mean(Ind)), by="Group"]

Prev_Lows=new_dat[Optimizations[fd_d2>0, ], , on=c("Ind")]
Prev_Lows[, Lowest_Close:=min(Close), by="Group"]
Prev_Lows=unique(Prev_Lows[, .SD, .SDcols=c("Opt_Ind", "Lowest_Close")])

Prev_Highs=new_dat[Optimizations[fd_d2<0, ], , on=c("Ind")]
Prev_Highs[, Highest_Close:=max(Close), by="Group"]
Prev_Highs=unique(Prev_Highs[, .SD, .SDcols=c("Opt_Ind", "Highest_Close")])

ggplot(dat, aes(x = Ind, y = Close)) +
  geom_point() +
  geom_line() +
  geom_line(colour = "red", size = 0.8, aes(y = fitted(b1))) +
  geom_vline(xintercept = Optimizations$Ind) +
  theme_bw()

# local minimum
ggplot(dat, aes(x = Ind, y = Close)) +
  geom_point() +
  geom_line() +
  geom_line(colour = "red", size = 0.8, aes(y = fitted(b1))) +
  geom_vline(xintercept = Prev_Lows$Opt_Ind) +
  # geom_hline(yintercept = Prev_Lows$Lowest_Close) +
  theme_bw()

# local maximum
ggplot(dat, aes(x = Ind, y = Close)) +
  geom_point() +
  geom_line() +
  geom_line(colour = "red", size = 0.8, aes(y = fitted(b1))) +
  geom_vline(xintercept = Prev_Highs$Opt_Ind) +
  # geom_hline(yintercept = Prev_Highs$Highest_Close) +
  theme_bw()

# # kernel regression
# ggplot(dat, aes(x = Ind, y = Close)) +
#   geom_point() +
#   geom_line() +
#   geom_line(colour = "blue", size = 0.8, aes(y = ksmooth(x=dat$Ind,
#                                                          y=dat$Close,
#                                                          bandwidth=5,
#                                                          n.points=nrow(dat))$y)) +
#   theme_bw()
# 
# kernel_smooth=ksmooth(x=dat$Ind,
#                       y=dat$Close,
#                       bandwidth=5,
#                       n.points=1000)
# plot(diff(scale(kernel_smooth$y)), type='o')
# plot(scale(kernel_smooth$y)[-1], type='o')
# abline(h=0)
# abline(v=which(diff(scale(kernel_smooth$y))<0.0001 & diff(scale(kernel_smooth$y))>-0.0001))


# new indicator idea : utilize the number of bullish/bearish candles
# also utilize data of the difference between the open and closing prices over the past candles