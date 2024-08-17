

##################################################################
##                           GET DATA                           ##
##################################################################

rm(list=ls())

library(httr)
library(jsonlite)

setwd('E:/My Drive/Transport Mode Choice')

# QUERY GENERAL INFO

res = GET('https://apitransporte.buenosaires.gob.ar/ecobici/gbfs/stationInformation?client_id=f65b1c550dc34f64b39033af6ad84681&client_secret=7508F895b8674ECF9CF8c60Ee3Dc0335')

datasss = fromJSON(rawToChar(res$content))

data = datasss$data[[1]]

data$rental_methods = NULL
saveRDS(data,'Argentina/Data Argentina/Availability Bikes/generalinfo.rds')

repeat{

res2 = GET('https://apitransporte.buenosaires.gob.ar/ecobici/gbfs/stationStatus?client_id=f65b1c550dc34f64b39033af6ad84681&client_secret=7508F895b8674ECF9CF8c60Ee3Dc0335')
datasss2 = fromJSON(rawToChar(res2$content))

data2 = datasss2$data[[1]]

fulltime = Sys.time()

fecha=paste0(substr(fulltime,1,10),'_',substr(fulltime,12,13),'-',substr(fulltime,15,16))

saveRDS(data2,paste0('Argentina/Data Argentina/Availability Bikes/generalinfo_',fecha,'.rds'))

Sys.sleep(3600)

}



