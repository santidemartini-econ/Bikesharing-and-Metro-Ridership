

##################################################################
##                           GET DATA                           ##
##################################################################

rm(list=ls())

library(httr)
library(jsonlite)
library(anytime)

setwd('E:/My Drive/Transport Mode Choice')

#### QUERY GENERAL INFO


repeat{
  
  
  res = GET("https://gbfs.mex.lyftbikes.com/gbfs/en/station_status.json")
  
  datasss = fromJSON(rawToChar(res$content))
  
  data = datasss$data[[1]]
  
  data$last_reported = anytime(data$last_reported)
  
  fulltime = Sys.time()
  
  fecha=paste0(substr(fulltime,1,10),'_',substr(fulltime,12,13),'-',substr(fulltime,15,16))
  
  saveRDS(data,paste0('CDMX/Data CDMX/Availability Bikes/generalinfo_',fecha,'.rds'))
  
  Sys.sleep(3600)
  
}

A = readRDS(paste0('CDMX/Data CDMX/Availability Bikes/generalinfo_',fecha,'.rds'))
