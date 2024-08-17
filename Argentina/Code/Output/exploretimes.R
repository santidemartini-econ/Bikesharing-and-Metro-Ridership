


#################################################################
##                        EXPLORE TIMES                        ##
#################################################################


rm(list=ls())

setwd('E:/My Drive/Transport Mode Choice')

library(sf)
library(dplyr)
library(mapview)
library(fuzzyjoin)
library(ggplot2)
library(lubridate)

#### 2019

trips2019 <- read.csv('Data Argentina/Trips/Raw Data/trips_2019.csv',fileEncoding = 'UTF-8')
trips2019$dia_orig <-  as.Date(substr(trips2019$fecha_origen_recorrido,1,10),'%Y-%m-%d')

trips_subs19 = trips2019[trips2019$dia_orig > as.Date('2019-6-30','%Y-%m-%d') & trips2019$dia_orig < as.Date('2019-10-01','%Y-%m-%d') , ]
trips_subs19$duracion_recorrido = as.numeric(gsub(',','',trips_subs19$duracion_recorrido))
trips_subs19$mins = trips_subs19$duracion_recorrido /60 
trips_subs_h19 = trips_subs19[trips_subs19$mins<100 ,]

trips_subs_h19$year = year(trips_subs_h19$dia_orig)

# hist(trips_subs_h19$mins,breaks = 36)


#### 2022

trips2022 <- read.csv('Data Argentina/Trips/Raw Data/trips_2022.csv',fileEncoding = 'UTF-8')
trips2022$dia_orig <- as.Date( substr(trips2022$fecha_origen_recorrido,1,10),'%Y-%m-%d')

trips_subs22 = trips2022[trips2022$dia_orig > as.Date('2022-6-30','%Y-%m-%d') & trips2022$dia_orig < as.Date('2022-10-01','%Y-%m-%d') , ]
trips_subs22$duracion_recorrido = as.numeric(gsub(',','',trips_subs22$duracion_recorrido))
trips_subs22$mins = trips_subs22$duracion_recorrido /60 
trips_subs_h22 = trips_subs22[trips_subs22$mins<100 ,]

trips_subs_h22$year = year(trips_subs_h22$dia_orig)
#hist(trips_subs_h22$mins,breaks = 50)

# summary(trips_subs_h22$mins)

plot.df = rbind(trips_subs_h22[,c('year','mins')] , trips_subs_h19[,c('year','mins')])


plotden = ggplot(plot.df, aes(x=mins,color=as.factor(year))) + 
  geom_density() +
  xlab('Minutes') +
  ylab('') +
  theme(legend.title = element_blank()) +
  labs(title='Travel Duration (density)') +
  geom_vline(xintercept = 30) +
  annotate('text',label="Pay if yr>2021" , x=40,y=0.05)+
  theme_classic()
  
ggsave('Output/density_length.png',plotden)

#### BY TIME OF THE DAY

reprweek = trips2019[trips2019$dia_orig %in% seq(as.Date('2019-10-14'),as.Date('2019-10-18'),by= '1 day') , ]
reprweek$horig = as.numeric(substr(reprweek$fecha_origen_recorrido,12,13))

horadepa =  data.frame(table(reprweek$horig))
horadepa$Var1 = as.numeric(horadepa$Var1)
horadepa$Freq = as.numeric(horadepa$Freq)

hdepart = ggplot(horadepa, aes(Var1,Freq)) +
  geom_line()+
  geom_vline(xintercept = 9) +
  geom_vline(xintercept = 18) +
  ylab('N Origin Rides') +
  xlab('Hour') +
  annotate('text',label="9 AM" , x=8,y=8200) +
  annotate('text',label="6 PM" , x=17,y=8200) +
  labs(title = 'Number of Weekday Rides in a Random Week' )












