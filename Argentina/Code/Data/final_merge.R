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

setwd('G:/My Drive/Transport Mode Choice/Argentina')

source("Code/remove_accents.R")


#########################

#### Subways
subway.t = readRDS('Data Argentina/Subway/grp_sbwy_daily.rds')
subway.t$estacion_id = ifelse(subway.t$estacion_id == 'AGÏ¿½ERO - D','AGUERO - D',subway.t$estacion_id)
subway.t = subway.t %>% filter(pases>0 & !is.na(fecha))

subway.o = st_read('Data Argentina/Geo/Subte/estaciones-de-subte.shp')
subway.o$ESTACION =   remove.accents(toupper(subway.o$ESTACION))
subway.o$estacion_id = paste0(subway.o$ESTACION,' - ',subway.o$LINEA)

####


#### Treatment databases

distancias = c('0_200','0_400','400_3000','800_3000','800_2600','800_2800','800_3400','1400_2800','1000_2000','1200_3200')

mindate = min(subway.t$fecha,na.rm=T)

for(d in distancias){
  m = readRDS(paste0('Data Argentina/Distances/Intervals x Open/m_',d,'.rds'))
  m[m$day >= mindate , ]
  names(m)[1:90]  = subway.o$estacion_id
  # Long
  m_l <- melt(setDT(m), id.vars = c("day"), variable.name = "estacion_id")
  names(m_l) = c('fecha','estacion_id',paste0('n_',d))
  if(d == distancias[1]){
    subway_f = subway.t %>% left_join(m_l)
  }else{
    subway_f = subway_f  %>% left_join(m_l)
  }
  
  subway_f$bin = ifelse(as.numeric(pull(subway_f[,ncol(subway_f)]))>0 ,1 , 0)
  names(subway_f)[which(names(subway_f) == 'bin')] = paste0('bin_',d)
}

# Filter variables with NaNs (i.e., where we do not have trips data)

subway_f = subway_f[!is.na(subway_f$bin_0_400),]

# I add variables of interactions between the binary variable that shows wheter there is a bike stations within 400m and a variable denoting the number of variables that are in different time distances
subway_f$inter_400_3000 = subway_f$bin_0_400*subway_f$n_400_3000
subway_f$inter_800_3000 = subway_f$bin_0_400*subway_f$n_800_3000
subway_f$inter_800_2600 = subway_f$bin_0_400*subway_f$n_800_2600
subway_f$inter_800_2800 = subway_f$bin_0_400*subway_f$n_800_2800
subway_f$inter_800_3400 = subway_f$bin_0_400*subway_f$n_800_3400
subway_f$inter_1400_2800 = subway_f$bin_0_400*subway_f$n_1400_2800
subway_f$inter_1000_2000 = subway_f$bin_0_400*subway_f$n_1000_2000
subway_f$inter_1200_3200 = subway_f$bin_0_400*subway_f$n_1200_3200
subway_f$inter_1200_3200_2 = subway_f$bin_0_200*subway_f$n_1200_3200

# I add passes in logs
subway_f$log_pases  = ifelse(subway_f$pases>0, log(subway_f$pases),NA)
subway_f$log_pases_peak  = ifelse(subway_f$pases_peak>0, log(subway_f$pases_peak),NA)
subway_f$log_pases_nopeak  = ifelse(subway_f$pases_nopeak>0, log(subway_f$pases_nopeak),NA)
subway_f$month = format(subway_f$fecha, '%Y-%m')
subway_f$linea = substr(subway_f$estacion_id , nchar(subway_f$estacion_id)-1,nchar(subway_f$estacion_id))

#### ADD CONTROL VARIABLES

## Weekend
wknd = readRDS('Data Argentina/Weekday/wkdays.rds')
wknd = wknd[,c('day','weekend','nolab','wkday')]
names(wknd)[names(wknd) == 'day'] = 'fecha'
wknd$fecha = as.Date(wknd$fecha)

## Clima
clima = readRDS('Data Argentina/Clima/clima.rds')
names(clima)[names(clima) == 'date'] = 'fecha'
clima$fecha = as.Date(clima$fecha)

## Demographics

demos = read.csv('Data Argentina/Demographics/demos_final.csv')
demos$X = NULL

demos$year = as.character(demos$year)

## Merge
subway_f = subway_f %>% left_join(wknd)
subway_f = subway_f %>% left_join(clima)

subway_f$year = substr(subway_f$fecha,1,4)

subway_f = subway_f %>% left_join(demos)
subway_f$year = NULL

#### First, effect by treatment groups (define group by year where they were treated)

origstat =  c(subway_f$estacion_id[subway_f$fecha == min(subway_f$fecha) &subway_f$bin_0_400>0]) # 30 STATIONS REMOVED


# Get treatment period by group
mindate = subway_f %>%
  filter(bin_0_400>0)  %>%
  group_by(estacion_id) %>%
  slice(which.min(fecha))

mindate = mindate %>% filter(!(estacion_id%in%origstat))

mindate$year = substr(mindate$fecha,1,4)

# Define Groups: early= 2015-2016, medium = 2017-2018, late = 2019

mindate$group = ifelse(mindate$year %in% c('2015','2016'),'1',
                       ifelse(mindate$year %in% c('2017','2018'),'2','3'))


subway_f = subway_f %>% left_join(mindate[,c('estacion_id','group')])

subway_f$group = ifelse(is.na(subway_f$group),0,subway_f$group)

subway_f$g1 = ifelse(subway_f$group ==1 ,1,0)
subway_f$g2 = ifelse(subway_f$group ==2 ,1,0)
subway_f$g3 = ifelse(subway_f$group ==3 ,1,0)

subway_f$tg1 = subway_f$g1*subway_f$bin_0_400
subway_f$tg2 = subway_f$g2*subway_f$bin_0_400
subway_f$tg3 = subway_f$g3*subway_f$bin_0_400


## SAVE

saveRDS(subway_f,'Data Argentina/Final/final_daily.rds')

#### CREATE DATABASE AT A MONTHLY LEVEL (sum taps by month)

subway_f$pases_weakday= subway_f$pases * (1-subway_f$nolab)
subway_f$pases_nolab= subway_f$pases * (subway_f$nolab)

###### Create database
subway_f = subway_f %>%
  group_by(month,estacion_id) %>%
  mutate(
    pases_m = sum(pases,na.rm=T),
    days = n(),
    pases_weakday_m = sum(pases_weakday,na.rm=T),
    pases_nolab_m = sum(pases_nolab,na.rm=T)
  )

subway_f$pases_weakday_m = ifelse(subway_f$pases_weakday_m == 0 , NA , subway_f$pases_weakday_m)

subway_f$pases_nolab_m = ifelse(subway_f$pases_nolab_m == 0 , NA , subway_f$pases_nolab_m)

subway_f$log_pases_m = log(subway_f$pases_m)
subway_f$log_pases_weekday_m = log(subway_f$pases_weakday_m)                
subway_f$log_pases_nolab_m = log(subway_f$pases_nolab_m)


subway_f$idx_m = paste0(subway_f$estacion_id,' - ',subway_f$month)

subway_m = subway_f[!duplicated(subway_f$idx_m),]


subway_m$mes = substr(subway_m$month , nchar(subway_m$month)-1,nchar(subway_m$month))

saveRDS(subway_m ,'Data Argentina/Final/final_monthly.rds' )




