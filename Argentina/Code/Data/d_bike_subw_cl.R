
#################################################################
##                     CALCULATE DISTANCES                     ##
#################################################################

rm(list=ls())

library(sf)
library(dplyr)
library(mapview)

setwd('G:/My Drive/Transport Mode Choice/Argentina')


########### Create Distnace Matrix
# OPEN BIKESTOPS
bikestops <- st_read('Data Argentina/Geo/Bicicleteros/IE-Estaciones.shp')

# OPEN ESTACIONES
subte <- st_read('Data Argentina/Geo/Subte/estaciones-de-subte.shp')

# What I do now is to set all the epgs in meters

# Determine the central longitude of your data
central_longitude <- (st_bbox(bikestops)[1] + st_bbox(bikestops)[3]) / 2

# Determine the UTM zone based on the central longitude
utm_zone <- floor((central_longitude + 180) / 6) + 1

# Define the appropriate UTM CRS
utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m")

# CHANGE EPSPS
bikestops = st_transform(bikestops, utm_crs)
subte = st_transform(subte, utm_crs)

# Create matrix of distances between bike stops and subte stations
dists = as.matrix(st_distance(bikestops,subte))

saveRDS(dists, 'Data Argentina/Distances/d_bike_subw.rds' )

##### Now we create matrices that say how many subway stations are open in each day within different space intervals. In this case we take intervals of 200 meters. 

intvl = 200
ubound = 0
length=40

i=0

nroww = nrow(dists) # number of bikestops
ncoll = ncol(dists) # number of subway stations

while(i < length){
  
  name = paste0('m_',ubound,'_',(ubound+intvl) )
  name2 = name
  
  temp = as.numeric(as.numeric(dists)<(ubound + intvl)  & as.numeric(dists)>ubound )
  matriz = matrix(temp,nrow=nroww,ncol = ncoll)
  
  assign(name , matriz)
  
  saveRDS(get(name) ,paste0('Data Argentina/Distances/Intervals/',name2,'.rds'))
  
  ubound = ubound + intvl
  i = i+1
  
}

##### Clean opening dates

# This database contains which stations were opened in each day. There are perdiods with no observations will be shown as NA 
open = readRDS('Data Argentina/Trips/open.rds')
open = open[,-which(names(open) %in% as.character(seq(as.Date('2020-01-01'),as.Date('2023-01-01'),by='1 month'))) ]

# retrieve months with no data
mes_nodata = names(open)[sapply(open, function(x) sum(is.na(x)) == length(x))]

# dates relevant to our study (pre-covid and starting from where we have bike tris data)
dates = seq(as.Date('2014-01-01') , as.Date('2019-12-31') ,by = '1 day' )
dates2 = paste0(format(dates,'%Y-%m'),'-01')
fechas = matrix(0 , nrow = length(dates), ncol = nrow(open) )



for(i in 1:length(dates)){
  idx = which(bikestops$NOMBRE %in% open[,which(names(open)==dates2[i])][!is.na(open[,which(names(open)==dates2[i])])]) 
  
  fechas[i,idx] = 1

  }

# Replace with NA in all those dates where we don't have data

idx = dates2 %in% mes_nodata
fechas[idx,] = NA

# AS BEFORE, WE GET THE NUMBER OF STATIONS IN A GIVEN INTERVAL IN EACH PERIOD

filos = list.files('Data Argentina/Distances/Intervals/')

for(n in filos){
  nom = substr(n,1,nchar(n)-4)
  temp = readRDS(paste0('Data Argentina/Distances/Intervals/',n))
  assign(nom,temp)
}


## VECTOR TO MATRIX

# 0 to 400

m_0_400 = m_0_200 + m_200_400

# 400 to 3000 

m_400_3000 = m_400_600 + m_600_800 +m_800_1000+m_1000_1200+m_1200_1400+m_1400_1600+m_1600_1800+m_1800_2000+m_2000_2200+m_2200_2400+m_2400_2600+m_2600_2800+m_2800_3000

# 800 to 3000

m_800_3000 = m_800_1000+m_1000_1200+m_1200_1400+m_1400_1600+m_1600_1800+m_1800_2000+m_2000_2200+m_2200_2400+m_2400_2600+m_2600_2800+m_2800_3000

# 800 to 2600

m_800_2600 = m_800_1000+m_1000_1200+m_1200_1400+m_1400_1600+m_1600_1800+m_1800_2000+m_2000_2200+m_2200_2400+m_2400_2600

# 800 to 2800

m_800_2800 = m_800_1000+m_1000_1200+m_1200_1400+m_1400_1600+m_1600_1800+m_1800_2000+m_2000_2200+m_2200_2400+m_2400_2600+m_2600_2800

# 800 to 3400

m_800_3400 = m_800_1000+m_1000_1200+m_1200_1400+m_1400_1600+m_1600_1800+m_1800_2000+m_2000_2200+m_2200_2400+m_2400_2600+m_2600_2800 + +m_2800_3000 + m_3000_3200 + +m_3200_3400 

# 1400 to 2800

m_1400_2800 = m_1400_1600+m_1600_1800+m_1800_2000+m_2000_2200+m_2200_2400+m_2400_2600+m_2600_2800

# 1000 to 2000

m_1000_2000 = m_1000_1200+m_1200_1400+m_1400_1600+m_1600_1800+m_1800_2000

# 1200 to 3200

m_1200_3200 = m_1200_1400+ m_1400_1600+m_1600_1800+m_1800_2000+m_2000_2200+m_2200_2400+m_2400_2600+m_2600_2800 + +m_2800_3000 +m_3000_3200 




# NOW I WANT TO CREATE TWO DATAFRAMES. ONE WILL TELL ME AT A GIVEN DAY t, HOW MANY OPENED BIKESTATIONS WERE WITHIN 400 m OF THE SUBWAY. THE OTHER ONE WILL GIVE ME BETWEEN 400 AND 3000


# Element i,j will say how many stations were opened within 400m of station i at date j. 

final_0_400 = fechas %*% m_0_400
final_0_400 = data.frame(final_0_400)
final_0_400$day = dates

saveRDS(final_0_400,'Data Argentina/Distances/Intervals x Open/m_0_400.rds')

# Same for within 400m and 3000m of station i at date j. 

final_400_3000 = fechas %*% m_400_3000
final_400_3000 = data.frame(final_400_3000)
final_400_3000$day = dates


saveRDS(final_400_3000,'Data Argentina/Distances/Intervals x Open/m_400_3000.rds')

# Within 0 and 200 meters

final_0_200 = fechas %*% m_0_200
final_0_200 = data.frame(final_0_200)
final_0_200$day = dates


saveRDS(final_0_200,'Data Argentina/Distances/Intervals x Open/m_0_200.rds')


# Same for within 800m and 3000m of station i at date j. 

final_800_3000 = fechas %*% m_800_3000
final_800_3000 = data.frame(final_800_3000)
final_800_3000$day = dates


saveRDS(final_800_3000,'Data Argentina/Distances/Intervals x Open/m_800_3000.rds')


# Same for within 800m and 3000m of station i at date j. 

final_800_2600 = fechas %*% m_800_2600
final_800_2600 = data.frame(final_800_2600)
final_800_2600$day = dates


saveRDS(final_800_2600,'Data Argentina/Distances/Intervals x Open/m_800_2600.rds')

# Same for within 800m and 2800m of station i at date j. 

final_800_2800 = fechas %*% m_800_2800
final_800_2800 = data.frame(final_800_2800)
final_800_2800$day = dates


saveRDS(final_800_2800,'Data Argentina/Distances/Intervals x Open/m_800_2800.rds')

# Same for within 800m and 3000m of station i at date j. 

final_800_3400 = fechas %*% m_800_3400
final_800_3400 = data.frame(final_800_3400)
final_800_3400$day = dates

# Same for within 800m and 3000m of station i at date j. 

final_1400_2800 = fechas %*% m_1400_2800
final_1400_2800 = data.frame(final_1400_2800)
final_1400_2800$day = dates


saveRDS(final_1400_2800,'Data Argentina/Distances/Intervals x Open/m_1400_2800.rds')

    
# Same for within 1000m and 2000m of station i at date j. 

final_1000_2000 = fechas %*% m_1000_2000
final_1000_2000 = data.frame(final_1000_2000)
final_1000_2000$day = dates


saveRDS(final_1000_2000,'Data Argentina/Distances/Intervals x Open/m_1000_2000.rds')


# Same for within 1200m and 3200m of station i at date j. 

final_1200_3200 = fechas %*% m_1200_3200
final_1200_3200 = data.frame(final_1200_3200)
final_1200_3200$day = dates


saveRDS(final_1200_3200,'Data Argentina/Distances/Intervals x Open/m_1200_3200.rds')



