#**********************
# Daily temporary break
if(as.ITime(format(Sys.time(), tz="PST8PDT"))>=(as.ITime("13:15:00")-60*5)& # if time is between 13:10:00 and 13:15:00 PDT
   as.ITime(format(Sys.time(), tz="PST8PDT"))<=(as.ITime("13:15:00"))){
  
  # (1) for 25 mins from 13:10:00 to 13:35:00 PDT (market close : 13:15:00 to 13:30:00 PDT)
  Sys.sleep(60*25)
  
  # if connection is lost, reconnect
  while(!isConnected(tws)){tws=twsConnect(port=7497)}
  
}else if(as.ITime(format(Sys.time(), tz="PST8PDT"))>=(as.ITime("14:00:00")-60*10)& # if time is between 13:50:00 and 14:00:00 PDT
         as.ITime(format(Sys.time(), tz="PST8PDT"))<=(as.ITime("14:00:00"))){
  
  # (2) for 75 mins from 13:50:00 to 15:05:00 PDT (market close : 14:00:00 to 15:00:00 PDT)
  Sys.sleep(60*75)
  
  # if connection is lost, reconnect
  while(!isConnected(tws)){tws=twsConnect(port=7497)}
  
  # execute a daily save of 5 second bar data afterwards
  Daily_Hist_Data_Save()
  
  
}else if(as.ITime(format(Sys.time(), tz="PST8PDT"))>=(as.ITime("23:45:00")-60*5)& # if time is between 23:40:00 and 23:45:00 PDT
         as.ITime(format(Sys.time(), tz="PST8PDT"))<=(as.ITime("23:45:00"))){
  
  # (3) for 20 mins from 23:40:00 to 24:00:00 PDT (automatic log-off)
  Sys.sleep(60*20)
  
  # if connection is lost, reconnect
  while(!isConnected(tws)){tws=twsConnect(port=7497)}
  
  
}

#***********
# Long break
if((weekdays(as.Date(format(Sys.time(), tz="PST8PDT")))=="Friday" & as.ITime(format(Sys.time(), tz="PST8PDT"))>=(as.ITime("14:00:00"))) | # the market closes at 14:00:00 PDT on Friday
   (weekdays(as.Date(format(Sys.time(), tz="PST8PDT")))=="Saturday") | # the market closes on Saturday
   (weekdays(as.Date(format(Sys.time(), tz="PST8PDT")))=="Sunday" & as.ITime(format(Sys.time(), tz="PST8PDT"))<(as.ITime("15:00:00")))){ # the market opens at 15:00:00 PDT on Sunday
  
  print("The market is closed.")
  
  break
}

