#################################################################
##                    HARMONIZE    SUBWAYS                     ##
#################################################################


rm(list=ls())

library(sf)
library(dplyr)
library(mapview)
library(data.table)
library(lmtest)
library(sandwich)
library(stargazer)
library(lubridate)
library(broom)
library(ggplot2)
library(forcats)
library(kableExtra)
library(plm)

setwd('G:/My Drive/Transport Mode Choice/CDMX')

source("Code/remove_accents.R")


#########################

#### Subways
subway.t = read.csv('Data/Ridership/Metro/afluenciastc_simple_02_2024.csv')
names(subway.t)[names(subway.t) == 'afluencia'] = 'pases'
subway.t$ESTACION =   remove.accents(toupper(subway.t$estacion))

subway.t$LINEA = substr(subway.t$linea,7,nchar(subway.t$linea))
subway.t$LINEA = ifelse(subway.t$LINEA %in% 1:9 , paste0(0,subway.t$LINEA),subway.t$LINEA)

subway.t$estacion_id = paste0(subway.t$ESTACION,' - ',subway.t$LINEA)

subway.t = subway.t %>% filter(pases>0 & !is.na(fecha))

# Just do a small descriptive stat to add to the paper
subway.t %>% group_by(anio) %>% summarise(sumpas = sum(pases,na.rm=T)/365)

# Read 

subway.o = st_read('Data\\Geo\\Metro\\STC_Metro_estaciones_utm14n.shp')
subway.o$ESTACION =   remove.accents(toupper(subway.o$NOMBRE))
subway.o$estacion_id = paste0(subway.o$ESTACION,' - ',subway.o$LINEA)


# Harmonize names
# unique(subway.t$estacion_id[!subway.t$estacion_id %in% subway.o$estacion_id])
# unique(subway.o$estacion_id[!subway.o$estacion_id %in% subway.t$estacion_id])

subway.t$estacion_id = ifelse(subway.t$estacion_id=="NIÑOS HEROES - 03" , "NIÑOS HEROES/PODER JUDICIAL CDMX - 03" ,
                              ifelse(subway.t$estacion_id=="UAM-AZCAPOTZALCO - 06", "UAM AZCAPOTZALCO - 06" ,
                                     ifelse(subway.t$estacion_id=="MIXIUHCA - 09"   , "MIXHIUCA - 09"  , subway.t$estacion_id)))

# Keep consistent data with bikestations
dayin =seq(as.Date('2010-02-01'),as.Date('2019-12-31'),by='1 day')
subway.t = subway.t %>% filter(fecha %in% dayin)


#### Treatment databases

distancias = c('0_200','0_400','400_3000','800_3000','800_2600','800_2800','1400_2800','1000_2000','1200_3200')

mindate = min(subway.t$fecha,na.rm=T)

for(d in distancias){
  m = readRDS(paste0('Data/Distances/Intervals x Open/m_',d,'.rds'))
  m = m[m$day >= mindate , ]
  names(m)[1:195]  = subway.o$estacion_id
  # Long
  m_l <- melt(setDT(m), id.vars = c("day"), variable.name = "estacion_id")
  names(m_l) = c('fecha','estacion_id',paste0('n_',d))
  m_l$fecha = as.character(m_l$fecha)
  if(d == distancias[1]){
    subway_f = subway.t %>% left_join(m_l)
  }else{
    subway_f = subway_f  %>% left_join(m_l)
  }
  
  subway_f$bin = ifelse(as.numeric(subway_f[,ncol(subway_f)])>0 ,1 , 0)
  names(subway_f)[which(names(subway_f) == 'bin')] = paste0('bin_',d)
}




subway_f$inter_400_3000 = subway_f$bin_0_400*subway_f$n_400_3000
subway_f$inter_800_3000 = subway_f$bin_0_400*subway_f$n_800_3000
subway_f$inter_800_2600 = subway_f$bin_0_400*subway_f$n_800_2600
subway_f$inter_800_2800 = subway_f$bin_0_400*subway_f$n_800_2800
subway_f$inter_1400_2800 = subway_f$bin_0_400*subway_f$n_1400_2800
subway_f$inter_1000_2000 = subway_f$bin_0_400*subway_f$n_1000_2000
subway_f$inter_1200_3200 = subway_f$bin_0_400*subway_f$n_1200_3200
subway_f$inter_1200_3200_2 = subway_f$bin_0_200*subway_f$n_1200_3200

# I add passes in logs
subway_f$log_pases  = ifelse(subway_f$pases>0, log(subway_f$pases),NA)
subway_f$fecha = as.Date(subway_f$fecha)
subway_f$month = format(subway_f$fecha, '%Y-%m')
subway_f$linea = substr(subway_f$estacion_id , nchar(subway_f$estacion_id)-1,nchar(subway_f$estacion_id))


#### ADD CONTROL VARIABLES

weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

# I only consider weekends, i do not consider holidays
subway_f$nolab = ifelse(weekdays(subway_f$fecha) %in% weekdays1 ,0,1)

## Clima
# clima = readRDS('Data Argentina/Clima/clima.rds')
# names(clima)[names(clima) == 'date'] = 'fecha'
# clima$fecha = as.Date(clima$fecha)

## Merge
# subway_f = subway_f %>% left_join(wknd)
# subway_f = subway_f %>% left_join(clima)

# a = subway_f %>% filter(month== '2018-05')
#### SAVE

saveRDS(subway_f,'Data/Final/final_daily.rds')





