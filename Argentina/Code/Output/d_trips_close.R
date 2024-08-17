

#################################################################
##               DISTANCE TRIPS CLOSE TO STATION               ##
#################################################################

rm(list=ls())

library(sf)
library(dplyr)
library(mapview)

setwd('G:/My Drive/Transport Mode Choice/Argentina')

# I want to understand the distance of the trips done from and to stations located close to the subway station

trips = readRDS('Data Argentina/Trips/Clean/trips2014_sh.rds')
trips = trips[trips$distance >0 & trips$distance< 10000000,c('distance','mes_orig','nombre_estacion_origen','nombre_estacion_destino')]

temp = readRDS('Data Argentina/Trips/Clean/trips2015_sh.rds')
temp = temp[temp$distance >0 & temp$distance< 10000000,c('distance','mes_orig','nombre_estacion_origen','nombre_estacion_destino')]

trips = rbind(trips,temp)

temp = readRDS('Data Argentina/Trips/Clean/trips2016_sh.rds')
temp = temp[temp$distance >0 & temp$distance< 10000000,c('distance','mes_orig','nombre_estacion_origen','nombre_estacion_destino')]
trips = rbind(trips,temp)

temp = readRDS('Data Argentina/Trips/Clean/trips2017_sh.rds')
temp = temp[temp$distance >0 & temp$distance< 10000000,c('distance','mes_orig','nombre_estacion_origen','nombre_estacion_destino')]
trips = rbind(trips,temp)

temp = readRDS('Data Argentina/Trips/Clean/trips2018_sh.rds')
temp = temp[temp$distance >0 & temp$distance< 10000000,c('distance','mes_orig','nombre_estacion_origen','nombre_estacion_destino')]
trips = rbind(trips,temp)

temp = readRDS('Data Argentina/Trips/Clean/trips2019_sh.rds')
temp = temp[temp$distance >0 & temp$distance< 10000000,c('distance','mes_orig','nombre_estacion_origen','nombre_estacion_destino')]
trips = rbind(trips,temp)

trips = trips[!is.na(trips$distance),]

trips$distance = as.numeric(trips$distance)

mean(as.numeric(trips$distance)) # Mean distance travel = 2500

hist(trips$distance)
summary(trips$distance) # Q1 = 1236 , Q2 = 1981 , Q3 =3190, mean = 2500

# PLOT DENSITY
plotden = ggplot(trips, aes(x=distance)) + 
  geom_density() +
  xlab('Distance (Meters)') +
  ylab('') +
  # scale_y_continuous(breaks = c())+
  theme(legend.title = element_blank())  +
  theme(axis.title =element_text(size=16)) +
  theme_classic() 

ggsave('Output/densi_lngth_mts.png',plotden,height = 4 , width = 6 , dpi=200)
