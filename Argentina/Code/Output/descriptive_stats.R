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
library(kableExtra)
library(ggplot2)
library(egg)

setwd('G:/My Drive/Transport Mode Choice/Argentina')

source("Code/remove_accents.R")

#### First check which months do not have missing data

open = readRDS('Data Argentina/Trips/open.rds')
open = open[,-which(names(open) %in% as.character(seq(as.Date('2020-01-01'),as.Date('2023-01-01'),by='1 month'))) ]

# retrieve months with no data
mes_nodata = names(open)[sapply(open, function(x) sum(is.na(x)) == length(x))]

# Stations in each month
stations_m = sapply(open, function(x) sum(!is.na(x)) )

df_stations = data.frame(month = substr(names(open),1,7) , number = stations_m)
df_stations$number = ifelse(df_stations$number ==0 , NA, df_stations$number)


m = ggplot(df_stations, aes(x=month, y=number,group=1)) + 
  geom_line() +
  theme_classic() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank() )+
  scale_x_discrete(breaks = c('2014-01','2016-01','2018-01') ) +
  annotate("rect", xmin = '2014-12', xmax = '2015-04', ymin = 0, ymax = 400,   alpha = .2,fill='gray')+
  annotate("rect", xmin = '2017-08', xmax = '2018-01', ymin = 0, ymax = 400,   alpha = .2,fill='gray')

ggsave('Output/stationsevol.png',m)


## Make plot of the 


#### Descriptive stats
# Let's set the dataframe that will be filled in each iteration

columns = c('Year','Months missing','Trips registered','Stations Registered','Mean trip (minutes)','SD trip (minutes)','Mean trip (meters)','SD trip (meters)')

df_stats = data.frame(matrix(nrow=length(2014:2019),ncol=(length(columns))))
names(df_stats) = columns

df_stats$Year = 2014:2019
df_stats[,2] = c(0,3,0,4,0,1) 


# Now, let's create a table that has the following: Year - Number of months with missing data - Trips done - Avg trip done in meters - Avg trip done in minutes 


#### 2014
trips = readRDS('Data Argentina/Trips/Clean/trips2014.rds')


trips$duracion_recorrido = difftime(trips$DESTINO_FECHA, trips$ORIGEN_FECHA)
trips$duracion_recorrido = as.numeric(trips$duracion_recorrido )

# In minutes 
trips$duracion_recorrido = trips$duracion_recorrido/60

trips = trips %>%
  filter(duracion_recorrido > 2 & duracion_recorrido < 60 )

# Careful with the distance, it happens that there are trips that start and finish in the same station. This is not a mistake, someone may take the bike and then come back. Something different are trips that last less than a minute or more than a day. 

df_stats[1,3] = nrow(trips)
df_stats[1,4] = length(unique(c(unique(trips$nombre_estacion_destino),unique(trips$nombre_estacion_origen))))
df_stats[1,5] = round(mean(trips$duracion_recorrido),2)
df_stats[1,6] = round(sd(trips$duracion_recorrido),2)

df_stats[1,7] = round(mean(trips$distance[trips$distance>0],na.rm = T),2)
df_stats[1,8] = round(sd(trips$distance[trips$distance>0],na.rm = T),2)

# Do also a time seriestderr()# Do also a time series of the number of trips done in each month

mtrips = trips %>% 
  group_by(mes_orig) %>%
  summarise(trips = n())

rm(trips)

#### 2015
trips = readRDS('Data Argentina/Trips/Clean/trips2015.rds')



trips$duracion_recorrido = difftime(trips$fecha_destino_recorrido, trips$fecha_origen_recorrido)
trips$duracion_recorrido = as.numeric(trips$duracion_recorrido )

# In minutes 
# trips$duracion_recorrido = trips$duracion_recorrido/60

trips = trips %>%
  filter(duracion_recorrido > 2 & duracion_recorrido < 60 )

# Careful with the distance, it happens that there are trips that start and finish in the same station. This is not a mistake, someone may take the bike and then come back. Something different are trips that last less than a minute or more than a day. 

df_stats[2,3] = nrow(trips)
df_stats[2,4] = length(unique(c(unique(trips$nombre_estacion_destino),unique(trips$nombre_estacion_origen))))
df_stats[2,5] = round(mean(trips$duracion_recorrido),2)
df_stats[2,6] = round(sd(trips$duracion_recorrido),2)

df_stats[2,7] = round(mean(trips$distance[trips$distance>0],na.rm = T),2)
df_stats[2,8] = round(sd(trips$distance[trips$distance>0],na.rm = T),2)

# Do also a time seriestderr()# Do also a time series of the number of trips done in each month

mtrips_t = trips %>% 
  group_by(mes_orig) %>%
  summarise(trips = n())

mtrips = rbind(mtrips,mtrips_t)

#### 2016
trips = readRDS('Data Argentina/Trips/Clean/trips2016.rds')

trips$duracion_recorrido = as.numeric(substr(trips$duracion_recorrido,0,1)) * 1440 + as.numeric(substr(trips$duracion_recorrido,8,9) ) *60  +  as.numeric(substr(trips$duracion_recorrido,11,12) ) +  as.numeric(substr(trips$duracion_recorrido,14,15) ) /60

# In minutes 
# trips$duracion_recorrido = trips$duracion_recorrido/60

trips = trips %>%
  filter(duracion_recorrido > 2 & duracion_recorrido < 60 )

# Careful with the distance, it happens that there are trips that start and finish in the same station. This is not a mistake, someone may take the bike and then come back. Something different are trips that last less than a minute or more than a day. 

df_stats[3,3] = nrow(trips)
df_stats[3,4] = length(unique(c(unique(trips$nombre_estacion_destino),unique(trips$nombre_estacion_origen))))
df_stats[3,5] = round(mean(trips$duracion_recorrido),2)
df_stats[3,6] = round(sd(trips$duracion_recorrido),2)

df_stats[3,7] = round(mean(trips$distance[trips$distance>0],na.rm = T),2)
df_stats[3,8] = round(sd(trips$distance[trips$distance>0],na.rm = T),2)


# Do also a time seriestderr()# Do also a time series of the number of trips done in each month

mtrips_t = trips %>% 
  group_by(mes_orig) %>%
  summarise(trips = n())

mtrips = rbind(mtrips,mtrips_t)

#### 2017
trips = readRDS('Data Argentina/Trips/Clean/trips2017.rds')


# There are a lot of missing 'destino' so I construct from the duration of the original database the minutes of the trip.
trips$duracion_recorrido = as.numeric(substr(trips$duracion_recorrido,0,1)) * 1440 + as.numeric(substr(trips$duracion_recorrido,8,9) ) *60  +  as.numeric(substr(trips$duracion_recorrido,11,12) ) +  as.numeric(substr(trips$duracion_recorrido,14,15) ) /60


# In minutes 
# trips$duracion_recorrido = trips$duracion_recorrido/60

trips = trips %>%
  filter(duracion_recorrido > 2 & duracion_recorrido < 60 )

# Careful with the distance, it happens that there are trips that start and finish in the same station. This is not a mistake, someone may take the bike and then come back. Something different are trips that last less than a minute or more than a day. 

df_stats[4,3] = nrow(trips)
df_stats[4,4] = length(unique(c(unique(trips$nombre_estacion_destino),unique(trips$nombre_estacion_origen))))
df_stats[4,5] = round(mean(trips$duracion_recorrido),2)
df_stats[4,6] = round(sd(trips$duracion_recorrido),2)

df_stats[4,7] = round(mean(trips$distance[trips$distance>0],na.rm = T),2)
df_stats[4,8] = round(sd(trips$distance[trips$distance>0],na.rm = T),2)


# Do also a time seriestderr()# Do also a time series of the number of trips done in each month

mtrips_t = trips %>% 
  group_by(mes_orig) %>%
  summarise(trips = n())

mtrips = rbind(mtrips,mtrips_t)

#### 2018
trips = readRDS('Data Argentina/Trips/Clean/trips2018.rds')


# There are a lot of missing 'destino' so I construct from the duration of the original database the minutes of the trip.
trips$duracion_recorrido = as.numeric(substr(trips$duracion_recorrido,0,1)) * 1440 + as.numeric(substr(trips$duracion_recorrido,8,9) ) *60  +  as.numeric(substr(trips$duracion_recorrido,11,12) ) +  as.numeric(substr(trips$duracion_recorrido,14,15) ) /60


# In minutes 
# trips$duracion_recorrido = trips$duracion_recorrido/60

trips = trips %>%
  filter(duracion_recorrido > 2 & duracion_recorrido < 60 )

# Careful with the distance, it happens that there are trips that start and finish in the same station. This is not a mistake, someone may take the bike and then come back. Something different are trips that last less than a minute or more than a day. 

df_stats[5,3] = nrow(trips)
df_stats[5,4] = length(unique(c(unique(trips$nombre_estacion_destino),unique(trips$nombre_estacion_origen))))
df_stats[5,5] = round(mean(trips$duracion_recorrido),2)
df_stats[5,6] = round(sd(trips$duracion_recorrido),2)

df_stats[5,7] = round(mean(trips$distance[trips$distance>0],na.rm = T),2)
df_stats[5,8] = round(sd(trips$distance[trips$distance>0],na.rm = T),2)


# Do also a time seriestderr()# Do also a time series of the number of trips done in each month

mtrips_t = trips %>% 
  group_by(mes_orig) %>%
  summarise(trips = n())

mtrips = rbind(mtrips,mtrips_t)


#### 2019
trips = readRDS('Data Argentina/Trips/Clean/trips2019.rds')


# There are a lot of missing 'destino' so I construct from the duration of the original database the minutes of the trip.
trips$duracion_recorrido = as.numeric(stringr::str_remove(trips$duracion_recorrido,',')) / 60


# In minutes 
# trips$duracion_recorrido = trips$duracion_recorrido/60

trips = trips %>%
  filter(duracion_recorrido > 2 & duracion_recorrido < 60 )

# Careful with the distance, it happens that there are trips that start and finish in the same station. This is not a mistake, someone may take the bike and then come back. Something different are trips that last less than a minute or more than a day. 

df_stats[6,3] = nrow(trips)
df_stats[6,4] = length(unique(c(unique(trips$nombre_estacion_destino),unique(trips$nombre_estacion_origen))))
df_stats[6,5] = round(mean(trips$duracion_recorrido),2)
df_stats[6,6] = round(sd(trips$duracion_recorrido),2)

df_stats[6,7] = round(mean(trips$distance[trips$distance>0],na.rm = T),2)
df_stats[6,8] = round(sd(trips$distance[trips$distance>0],na.rm = T),2)


# Do also a time seriestderr()# Do also a time series of the number of trips done in each month

mtrips_t = trips %>% 
  group_by(mes_orig) %>%
  summarise(trips = n())

mtrips = rbind(mtrips,mtrips_t)

#### SAVE OUTPUTS

saveRDS(mtrips,'Data Argentina/Temp/mtrips.rds')
saveRDS(df_stats,'Data Argentina/Temp/df_stats')

#### Output for tex

sink('Output/descstats_bikes.tex')

printabla = kbl(df_stats, booktabs = T,format = 'latex',row.names = FALSE, linesep = '',escape=F)  %>%
  kable_styling(latex_options = c("repeat_header","scale_down")) %>%
  column_spec(c(1),border_right = T)%>%
  #row_spec(c(30,34),hline_after = T) %>%
  kable_styling(latex_options = "hold_position") 

substr(printabla,103,nchar(printabla)-13)

sink()

#### Time series plot with the trips by month

dates = format(seq(as.Date('2014-01-01'), as.Date('2019-12-01'),by='1 month'),'%Y-%m')
plotdf =data.frame(mes_orig = dates,Trips = NA)
plotdf = plotdf %>% left_join(mtrips)
plotdf$Trips = NULL

plotdf$trips  = as.numeric(plotdf$trips)

# PLOT
p = ggplot(plotdf, aes(x=mes_orig, y=trips,group=1)) + 
  geom_line() +
  theme_classic() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank() )+
  scale_x_discrete(breaks = c('2014-01','2016-01','2018-01') ) +
  scale_y_continuous(breaks=c(250000,500000,750000) , labels = c('250K','500K','750K')) +
  annotate("rect", xmin = '2014-12', xmax = '2015-04', ymin = 0, ymax = 800000,   alpha = .2,fill='gray')+
  annotate("rect", xmin = '2017-08', xmax = '2018-01', ymin = 0, ymax = 800000,   alpha = .2,fill='gray')+
  annotate("rect", xmin = '2018-12', xmax = '2019-02', ymin = 0, ymax = 800000,   alpha = .2,fill='gray')

saveRDS(p,'Data Argentina/Temp/plot_tbici.rds')
# p = readRDS('Data Argentina/Temp/plot_tbici.rds')

# ggsave( 'Output/trips_m.png',p)

  
#### Bus trips
subway.t = readRDS('Data Argentina/Subway/grp_sbwy_daily.rds')
subway.t$mes = format(subway.t$fecha,'%Y-%m')

subway.t = subway.t %>%
  filter(fecha < as.Date('2020-01-01'))

subway.m = subway.t %>%
  group_by(mes) %>%
  summarise(pases = sum(pases,na.rm=T))

q = ggplot(subway.m, aes(x=mes, y=pases,group=1)) + 
  geom_line() +
  theme_classic() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank() )+
  scale_x_discrete(breaks = c('2014-01','2016-01','2018-01')) +
  scale_y_continuous(breaks=c(20000000,25000000,30000000) , labels = c('20Mill.','25Mill.','30Mill.')) 

saveRDS(q,'Data Argentina/Temp/plot_tsbwy.rds')

q =  saveRDS(q,'Data Argentina/Temp/plot_tsbwy.rds')

# q =  readRDS('Data Argentina/Temp/plot_tsbwy.rds')
ggsave( 'Output/tripssbwy_m.png',q)


pq = ggarrange(p,q,ncol=2)

ggsave('Output/tripsevol.png',pq)

