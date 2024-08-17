


#################################################################
##                     GET CLOSE STATIONS                     ##
#################################################################

rm(list=ls())

library(sf)
library(dplyr)
library(mapview)
library(fuzzyjoin)

setwd('G:/My Drive/Transport Mode Choice/Argentina')
source("Code/remove_accents.R")

# ########### Get which are the bike stations that are within 400m of the bike stations
dists = readRDS('Data Argentina/Distances/d_bike_subw.rds')
# 
mindists = apply(dists, 1, FUN = min, na.rm = TRUE)
idx_min = which(mindists < 400)


########### BIKE STATIONS - I HAVE TO CALCUALTE THE DISTANCES BETWEEN STATIONS

bikestops <- st_read('Data Argentina/Geo/Bicicleteros/IE-Estaciones.shp')
bikestops$NUMERO = 1:nrow(bikestops)


# Determine the central longitude of your data
central_longitude <- (st_bbox(bikestops)[1] + st_bbox(bikestops)[3]) / 2

# Determine the UTM zone based on the central longitude
utm_zone <- floor((central_longitude + 180) / 6) + 1

# Define the appropriate UTM CRS
utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m")

# CHANGE EPSPS
bikestops = st_transform(bikestops, utm_crs)

dists_b = as.matrix(st_distance(bikestops,bikestops))

dfdists = data.frame(dists_b)

# Transofrm the matrix of distances to a vector for an easy left join

o = rep(1:nrow(bikestops),nrow(bikestops))
d = rep(1:nrow(bikestops),each= nrow(bikestops))

od = paste0(o,'-',d)
distance_mg = data.frame(idx = od,distance = c(as.numeric(dists_b)))

######### 


bikestops$NOMBRE <- remove.accents(toupper(bikestops$NOMBRE))

change_src <- c('RETIRO','PUERTO MADERO - UCA','PZA. ROMA','DERECHO','PLAZA EMILIO MITRE','PZA. DE MAYO','PZA. SAN MARTIN','UCA PUERTO MADERO','JUANA MANSO','URQUIZA','PIEDRAS','ZOOLOGICO','GALERIAS PACIFICO','COLEGIO NACIONAL BUENOS AIRES','TREINTA Y TRES ORIENTALES','CONSTITUCION I','CONSTITUCION II','RETIRO III','PINZON','HUMBERTO PRIMO','FITZ ROY Y GORRTITI','PERON Y FRANCISCO ACU?A DE FIGUEROA','CATAMARCA','AV PAVON Y 33 ORIENTALES','PLAZA HIPOLITO BOUCHARD','ORO',"- CIUDAD UNIVERSITARIA II","037- CLAUDIA PIA BRAUDRACCO","048 -PERON","131- HOSPITAL DE CLINICAS","148 -LA PATERNAL","175","177 -PLANETARIO","227 -CLUB CIUDAD DE BUENOS AIRES","239 -VIDAL","288- 11 DE SEPTIEMBRE","343 POMAR","359 PB",'CLAUDIA PIA BRAUDRACCO','ERRONEA',"FITZ ROY & GORRITI","CEMENTERIO DE RECOLETA","JUAN MANUEL DE BLANES","OFF BRAUDACCO",'EX BALCARCE','EX LEGISLATURA',"F.J.SANTAMARIA DE ORO", "CORPORACION BS AS SUR",'REPARACION DE K7','ROCA Y BELGRANO EX','PARQUE CHACABUCO 2 / OFF LINE','REGIMIENTOS DE PATRICIOS',"PARQUE CHACABUCO  I","234",'MARGARI?OS CERVANTES Y ARGERICH','PLAZA ALBERDI','PLANEADA','MARGARI?OS CERVANTES','EX PARQUE AVELLANEDA II','BALBOA DEFINITIVO','SAN JOSE DE FLORES VIEJA','REGIMIENTOS PATRICIOS','REGIMIENTOS DE PATRICIOS','CERRETTI',"SEC. DE INNOVACION Y TRANSFORMACION","PLAZA  SAN MARTIN" ,'LAUDIA PIA BAUDRACCO','FILOSOFIA Y LETRAS')
change_to <- c('RETIRO I','MADERO UCA','PLAZA ROMA','FACULTAD DE DERECHO','EMILIO MITRE','PLAZA PRIMERO DE MAYO','PLAZA SAN MARTIN','MADERO UCA','JUANA MANSO I','URQUIZA Y RONDEAU','CLAUDIA PIA BRAUDACCO','ECOPARQUE','PACIFICO','COLEGIO NACIONAL DE BUENOS AIRES','33 ORIENTALES','CONSTITUCION','CONSTITUCION','RETIRO I','PINZON Y MARTIN RODRIGUEZ','HUMBERTO 1?','FITZ ROY Y GORRITI','PERON Y ACU?A DE FIGUEROA','MANZANA 66','AV. PAVON Y 33 ORIENTALES','PARQUE DEL BAJO','F. J. SANTAMARIA DE ORO','CIUDAD UNIVERSITARIA II','CLAUDIA PIA BRAUDACCO',"PARQUE DEL BAJO" ,"HOSPITAL DE CLINICAS","PATERNAL","ESMERALDA Y CORRIENTES","PLANETARIO","CLUB CIUDAD DE BUENOS AIRES","VIDAL",'11 DE SEPTIEMBRE','POMAR','SECRETARIA DE DEPORTES','CLAUDIA PIA BRAUDACCO','FACULTAD DE DERECHO','FITZ ROY Y GORRITI','CEMENTERIO DE LA RECOLETA','JUAN MANUEL BLANES','CLAUDIA PIA BRAUDACCO','BALCARCE','LEGISLATURA','F. J. SANTAMARIA DE ORO', "CORPORACION BUENOS AIRES SUR",'UNIVERSIDAD DE BELGRANO','BELGRANO Y ROCA','PARQUE CHACABUCO II','REGIMIENTOS PATRICIOS',"PARQUE CHACABUCO I",'AVENIDA TRIUNVIRATO','MAGARI?OS CERVANTES Y ARGERICH','PLAZA ALBERTDI','BEIRO Y SAN MARTIN','MAGARI?OS CERVANTES','PARQUE AVELLANEDA II','JOSE ARTIGAS','SAN JOSE DE FLORES','REGIMIENTO PATRICIOS','REGIMIENTO PATRICIOS','CERETTI','SECRETARIA DE INNOVACION',"PLAZA SAN MARTIN" ,'CLAUDIA PIA BRAUDACCO','OEA')

# INPUTADO A CLOSEST BIKESTOP: FILOSOFIA Y LETRAS 
mergedf <- data.frame(nombre_estacion_origen = change_src , nombre_correct = change_to)
mergedf2 = mergedf
names(mergedf2)[1] = 'nombre_estacion_destino'

# I also want to create an index of which stations appear in each month, and see which ones were closed afterwards

meses =seq(as.Date('2014-01-01'),as.Date('2023-01-01'),by='1 month')

open = data.frame(matrix(ncol=length(meses),nrow=nrow(bikestops)))
names(open) = meses



######### READ TRIPS DONE IN A GIVEN YEAR

# HERE I WANT TO READ ALL THE TRIPS DONE IN A GIVEN YEAR, CALCULATE THE DISTANCE TRAVELLED IN EACH TRIP


#### 2014

trips2014 <- read.csv('Data Argentina/Trips/Raw Data/recorridos-realizados-2014.csv',fileEncoding = 'UTF-8')
names(trips2014)[names(trips2014) == 'NOMBRE_ORIGEN'] <- 'nombre_estacion_origen'
names(trips2014)[names(trips2014) == 'DESTINO_ESTACION'] <- 'nombre_estacion_destino'


trips2014$nombre_estacion_origen <- remove.accents(toupper(trips2014$nombre_estacion_origen))
trips2014$nombre_estacion_destino <- remove.accents(toupper(trips2014$nombre_estacion_destino))

trips2014$dia_orig <-  substr(trips2014$ORIGEN_FECHA,1,10)

# NORMALIZE STATIONS' NAMES
trips2014 <- regex_left_join(trips2014 , mergedf , by = 'nombre_estacion_origen')

trips2014$nombre_estacion_origen  <- ifelse(!is.na(trips2014$nombre_correct),trips2014$nombre_correct,trips2014$nombre_estacion_origen.x)

trips2014$nombre_estacion_origen.x = NULL
trips2014$nombre_estacion_origen.y = NULL
trips2014$nombre_correct = NULL

# NOW FOR DESTINATION 

trips2014 <- regex_left_join(trips2014 , mergedf2 , by ='nombre_estacion_destino')

trips2014$nombre_estacion_destino  <- ifelse(!is.na(trips2014$nombre_correct),trips2014$nombre_correct,trips2014$nombre_estacion_destino.x)

trips2014$nombre_estacion_destino.x = NULL
trips2014$nombre_estacion_destino.y = NULL
trips2014$nombre_correct = NULL


# CALCULATE THE DISTANCE OF EACH TRIP

trips2014 = trips2014 %>% left_join( st_drop_geometry(bikestops[,c('NUMERO','NOMBRE')]) , by = c('nombre_estacion_origen'='NOMBRE'))
names(trips2014)[names(trips2014)=='NUMERO']= 'o'

trips2014 = trips2014 %>% left_join( st_drop_geometry(bikestops[,c('NUMERO','NOMBRE')]) , by = c('nombre_estacion_destino'='NOMBRE'))
names(trips2014)[names(trips2014)=='NUMERO']= 'd'

trips2014$idx = paste0(trips2014$o,'-',trips2014$d)

trips2014 = trips2014 %>% left_join(distance_mg)

# See which stations actually have one trip done in each month (origin or destination)

trips2014$mes_orig = format(as.Date(trips2014$dia_orig),'%Y-%m')
meses_in = unique(trips2014$mes_orig)

for(m in meses_in){
  temp = trips2014[trips2014$mes_orig %in% m , ]
  stat_temp = unique(c(temp$nombre_estacion_origen,temp$nombre_estacion_destino))
  open[1:length(stat_temp) , which(names(open)==paste0(m,'-01'))] = stat_temp
}

short_sub = trips2014[trips2014$o %in% idx_min |trips2014$d %in% idx_min ,]
saveRDS(short_sub,'Data Argentina/Trips/Clean/trips2014_sh.rds')

saveRDS(trips2014,'Data Argentina/Trips/Clean/trips2014.rds')

rm(trips2014)



#### 2015

trips2015 <- read.csv('Data Argentina/Trips/Raw Data/recorridos-realizados-2015.csv',fileEncoding = 'UTF-8')

trips2015$nombre_estacion_origen <- remove.accents(toupper(trips2015$nombre_estacion_origen))
trips2015$nombre_estacion_destino <- remove.accents(toupper(trips2015$nombre_estacion_destino))

trips2015$dia_orig <-  substr(trips2015$fecha_origen_recorrido,1,10)

# NORMALIZE STATIONS' NAMES
trips2015 <- regex_left_join(trips2015 , mergedf , by = 'nombre_estacion_origen')

trips2015$nombre_estacion_origen  <- ifelse(!is.na(trips2015$nombre_correct),trips2015$nombre_correct,trips2015$nombre_estacion_origen.x)

trips2015$nombre_estacion_origen.x = NULL
trips2015$nombre_estacion_origen.y = NULL
trips2015$nombre_correct = NULL

# NOW FOR DESTINATION 

trips2015 <- regex_left_join(trips2015 , mergedf2 , by ='nombre_estacion_destino')

trips2015$nombre_estacion_destino  <- ifelse(!is.na(trips2015$nombre_correct),trips2015$nombre_correct,trips2015$nombre_estacion_destino.x)

trips2015$nombre_estacion_destino.x = NULL
trips2015$nombre_estacion_destino.y = NULL
trips2015$nombre_correct = NULL


# CALCULATE THE DISTANCE OF EACH TRIP

trips2015 = trips2015 %>% left_join( st_drop_geometry(bikestops[,c('NUMERO','NOMBRE')]) , by = c('nombre_estacion_origen'='NOMBRE'))
names(trips2015)[names(trips2015)=='NUMERO']= 'o'

trips2015 = trips2015 %>% left_join( st_drop_geometry(bikestops[,c('NUMERO','NOMBRE')]) , by = c('nombre_estacion_destino'='NOMBRE'))
names(trips2015)[names(trips2015)=='NUMERO']= 'd'

trips2015$idx = paste0(trips2015$o,'-',trips2015$d)

trips2015 = trips2015 %>% left_join(distance_mg)

# See which stations actually have one trip done in each month (origin or destination)

trips2015$mes_orig = format(as.Date(trips2015$dia_orig),'%Y-%m')
meses_in = unique(trips2015$mes_orig)

for(m in meses_in){
  temp = trips2015[trips2015$mes_orig %in% m , ]
  stat_temp = unique(c(temp$nombre_estacion_origen,temp$nombre_estacion_destino))
  open[1:length(stat_temp) , which(names(open)==paste0(m,'-01'))] = stat_temp
}

trips2015 = trips2015[,- which(names(trips2015) %in% c('id_estacion_origen','long_estacion_origen','lat_estacion_origen','domicilio_estacion_origen','id_estacion_destino','long_estacion_destino','lat_estacion_destino','domicilio_estacion_destino','X','Id_recorrido','direccion_estacion_origen','direccion_estacion_destino'))]

short_sub = trips2015[trips2015$o %in% idx_min |trips2015$d %in% idx_min ,]
saveRDS(short_sub,'Data Argentina/Trips/Clean/trips2015_sh.rds')

saveRDS(trips2015,'Data Argentina/Trips/Clean/trips2015.rds')

rm(trips2015)




#### 2016

trips2016 <- read.csv('Data Argentina/Trips/Raw Data/recorridos-realizados-2016.csv',fileEncoding = 'UTF-8')

trips2016$nombre_estacion_origen <- remove.accents(toupper(trips2016$nombre_estacion_origen))
trips2016$nombre_estacion_destino <- remove.accents(toupper(trips2016$nombre_estacion_destino))

trips2016$dia_orig <-  substr(trips2016$fecha_origen_recorrido,1,10)

# NORMALIZE STATIONS' NAMES
trips2016 <- regex_left_join(trips2016 , mergedf , by = 'nombre_estacion_origen')

trips2016$nombre_estacion_origen  <- ifelse(!is.na(trips2016$nombre_correct),trips2016$nombre_correct,trips2016$nombre_estacion_origen.x)

trips2016$nombre_estacion_origen.x = NULL
trips2016$nombre_estacion_origen.y = NULL
trips2016$nombre_correct = NULL

# NOW FOR DESTINATION 

trips2016 <- regex_left_join(trips2016 , mergedf2 , by ='nombre_estacion_destino')

trips2016$nombre_estacion_destino  <- ifelse(!is.na(trips2016$nombre_correct),trips2016$nombre_correct,trips2016$nombre_estacion_destino.x)

trips2016$nombre_estacion_destino.x = NULL
trips2016$nombre_estacion_destino.y = NULL
trips2016$nombre_correct = NULL


# CALCULATE THE DISTANCE OF EACH TRIP

trips2016 = trips2016 %>% left_join( st_drop_geometry(bikestops[,c('NUMERO','NOMBRE')]) , by = c('nombre_estacion_origen'='NOMBRE'))
names(trips2016)[names(trips2016)=='NUMERO']= 'o'

trips2016 = trips2016 %>% left_join( st_drop_geometry(bikestops[,c('NUMERO','NOMBRE')]) , by = c('nombre_estacion_destino'='NOMBRE'))
names(trips2016)[names(trips2016)=='NUMERO']= 'd'

trips2016$idx = paste0(trips2016$o,'-',trips2016$d)

trips2016 = trips2016 %>% left_join(distance_mg)

# See which stations actually have one trip done in each month (origin or destination)

trips2016$mes_orig = format(as.Date(trips2016$dia_orig),'%Y-%m')
meses_in = unique(trips2016$mes_orig)

for(m in meses_in){
  temp = trips2016[trips2016$mes_orig %in% m , ]
  stat_temp = unique(c(temp$nombre_estacion_origen,temp$nombre_estacion_destino))
  open[1:length(stat_temp) , which(names(open)==paste0(m,'-01'))] = stat_temp
}

trips2016 = trips2016[,- which(names(trips2016) %in% c('periodo','id_estacion_origen','long_estacion_origen','lat_estacion_origen','domicilio_estacion_origen','id_estacion_destino','long_estacion_destino','lat_estacion_destino','domicilio_estacion_destino','X','Id_recorrido','direccion_estacion_origen','direccion_estacion_destino'))]

short_sub = trips2016[trips2016$o %in% idx_min |trips2016$d %in% idx_min ,]
saveRDS(short_sub,'Data Argentina/Trips/Clean/trips2016_sh.rds')

saveRDS(trips2016,'Data Argentina/Trips/Clean/trips2016.rds')

rm(trips2016)




#### 2017

trips2017 <- read.csv('Data Argentina/Trips/Raw Data/recorridos-realizados-2017.csv',fileEncoding = 'UTF-8')

trips2017$nombre_estacion_origen <- remove.accents(toupper(trips2017$nombre_estacion_origen))
trips2017$nombre_estacion_destino <- remove.accents(toupper(trips2017$nombre_estacion_destino))

trips2017$dia_orig <-  substr(trips2017$fecha_origen_recorrido,1,10)

# NORMALIZE STATIONS' NAMES
trips2017 <- regex_left_join(trips2017 , mergedf , by = 'nombre_estacion_origen')

trips2017$nombre_estacion_origen  <- ifelse(!is.na(trips2017$nombre_correct),trips2017$nombre_correct,trips2017$nombre_estacion_origen.x)

trips2017$nombre_estacion_origen.x = NULL
trips2017$nombre_estacion_origen.y = NULL
trips2017$nombre_correct = NULL

# NOW FOR DESTINATION 

trips2017 <- regex_left_join(trips2017 , mergedf2 , by ='nombre_estacion_destino')

trips2017$nombre_estacion_destino  <- ifelse(!is.na(trips2017$nombre_correct),trips2017$nombre_correct,trips2017$nombre_estacion_destino.x)

trips2017$nombre_estacion_destino.x = NULL
trips2017$nombre_estacion_destino.y = NULL
trips2017$nombre_correct = NULL


# CALCULATE THE DISTANCE OF EACH TRIP

trips2017 = trips2017 %>% left_join( st_drop_geometry(bikestops[,c('NUMERO','NOMBRE')]) , by = c('nombre_estacion_origen'='NOMBRE'))
names(trips2017)[names(trips2017)=='NUMERO']= 'o'

trips2017 = trips2017 %>% left_join( st_drop_geometry(bikestops[,c('NUMERO','NOMBRE')]) , by = c('nombre_estacion_destino'='NOMBRE'))
names(trips2017)[names(trips2017)=='NUMERO']= 'd'

trips2017$idx = paste0(trips2017$o,'-',trips2017$d)

trips2017 = trips2017 %>% left_join(distance_mg)

# See which stations actually have one trip done in each month (origin or destination)

trips2017$mes_orig = format(as.Date(trips2017$dia_orig),'%Y-%m')
meses_in = unique(trips2017$mes_orig)

for(m in meses_in){
  temp = trips2017[trips2017$mes_orig %in% m , ]
  stat_temp = unique(c(temp$nombre_estacion_origen,temp$nombre_estacion_destino))
  open[1:length(stat_temp) , which(names(open)==paste0(m,'-01'))] = stat_temp
}

trips2017 = trips2017[,- which(names(trips2017) %in% c('periodo','id_estacion_origen','long_estacion_origen','lat_estacion_origen','domicilio_estacion_origen','id_estacion_destino','long_estacion_destino','lat_estacion_destino','domicilio_estacion_destino','X','Id_recorrido','direccion_estacion_origen','direccion_estacion_destino'))]

short_sub = trips2017[trips2017$o %in% idx_min |trips2017$d %in% idx_min ,]
saveRDS(short_sub,'Data Argentina/Trips/Clean/trips2017_sh.rds')

saveRDS(trips2017,'Data Argentina/Trips/Clean/trips2017.rds')

rm(trips2017)


#### 2018

trips2018 <- read.csv('Data Argentina/Trips/Raw Data/recorridos-realizados-2018.csv',fileEncoding = 'UTF-8')

trips2018$nombre_estacion_origen <- remove.accents(toupper(trips2018$nombre_estacion_origen))
trips2018$nombre_estacion_destino <- remove.accents(toupper(trips2018$nombre_estacion_destino))

trips2018$dia_orig <-  substr(trips2018$fecha_origen_recorrido,1,10)

# NORMALIZE STATIONS' NAMES
trips2018 <- regex_left_join(trips2018 , mergedf , by = 'nombre_estacion_origen')

trips2018$nombre_estacion_origen  <- ifelse(!is.na(trips2018$nombre_correct),trips2018$nombre_correct,trips2018$nombre_estacion_origen.x)

trips2018$nombre_estacion_origen.x = NULL
trips2018$nombre_estacion_origen.y = NULL
trips2018$nombre_correct = NULL

# NOW FOR DESTINATION 

trips2018 <- regex_left_join(trips2018 , mergedf2 , by ='nombre_estacion_destino')

trips2018$nombre_estacion_destino  <- ifelse(!is.na(trips2018$nombre_correct),trips2018$nombre_correct,trips2018$nombre_estacion_destino.x)

trips2018$nombre_estacion_destino.x = NULL
trips2018$nombre_estacion_destino.y = NULL
trips2018$nombre_correct = NULL


# CALCULATE THE DISTANCE OF EACH TRIP

trips2018 = trips2018 %>% left_join( st_drop_geometry(bikestops[,c('NUMERO','NOMBRE')]) , by = c('nombre_estacion_origen'='NOMBRE'))
names(trips2018)[names(trips2018)=='NUMERO']= 'o'

trips2018 = trips2018 %>% left_join( st_drop_geometry(bikestops[,c('NUMERO','NOMBRE')]) , by = c('nombre_estacion_destino'='NOMBRE'))
names(trips2018)[names(trips2018)=='NUMERO']= 'd'

trips2018$idx = paste0(trips2018$o,'-',trips2018$d)

trips2018 = trips2018 %>% left_join(distance_mg)

# See which stations actually have one trip done in each month (origin or destination)

trips2018$mes_orig = format(as.Date(trips2018$dia_orig),'%Y-%m')
meses_in = unique(trips2018$mes_orig)

for(m in meses_in){
  temp = trips2018[trips2018$mes_orig %in% m , ]
  stat_temp = unique(c(temp$nombre_estacion_origen,temp$nombre_estacion_destino))
  open[1:length(stat_temp) , which(names(open)==paste0(m,'-01'))] = stat_temp
}

trips2018 = trips2018[,- which(names(trips2018) %in% c('periodo','id_estacion_origen','long_estacion_origen','lat_estacion_origen','domicilio_estacion_origen','id_estacion_destino','long_estacion_destino','lat_estacion_destino','domicilio_estacion_destino','X','Id_recorrido','direccion_estacion_origen','direccion_estacion_destino'))]

short_sub = trips2018[trips2018$o %in% idx_min |trips2018$d %in% idx_min ,]
saveRDS(short_sub,'Data Argentina/Trips/Clean/trips2018_sh.rds')

saveRDS(trips2018,'Data Argentina/Trips/Clean/trips2018.rds')

rm(trips2018)


###### 2019

# Its commented, but i created sub-bases because the full data is too heavy and it takes forever to be read

# trips2019 <- read.csv('Data Argentina/Trips/Raw Data/trips_2019.csv',fileEncoding = 'UTF-8')
# 
# a = trips2019
# a$dia_orig <-  substr(a$fecha_origen_recorrido,1,7)
# a1 = a[a$dia_orig %in% c('2019-01','2019-02','2019-03','2019-04','2019-05','2019-06'),]
# a2= a[a$dia_orig %in% c('2019-07','2019-08','2019-09'),]
# a3 = a[a$dia_orig %in% c('2019-10','2019-11','2019-12'),]
# 
# a1$dia_orig = NULL
# a2$dia_orig = NULL
# a3$dia_orig = NULL
# 
# saveRDS(a1 , 'Data Argentina/Trips/Raw Data/trips_2019_1.csv')
# saveRDS(a2 , 'Data Argentina/Trips/Raw Data/trips_2019_2.csv')
# saveRDS(a3 , 'Data Argentina/Trips/Raw Data/trips_2019_3.csv')
# 
# a1 = readRDS( 'Data Argentina/Trips/Raw Data/trips_2019_1.csv')
# a2 = readRDS( 'Data Argentina/Trips/Raw Data/trips_2019_2.csv')
# a3 = readRDS( 'Data Argentina/Trips/Raw Data/trips_2019_3.csv')

trips2019 = rbind(a1,a2,a3)

rm(a1,a2,a3)

# a =trips2019[trips2019$dia_orig %in% c('2019-02','2019-03','2019-04'),]

# From the stations, there are some weird names, we have to set the correct names

vecweird <- c('037- Claudia P?a Braudracco',"048 -PERON","131- HOSPITAL DE CL?NICAS" ,"148 -La Paternal","175","177 -PLANETARIO" ,"227 -Club Ciudad de Buenos Aires", "234","239 -VIDAL" ,"288- 11 de septiembre","343 POMAR" ,"359 PB")

estaciones19 <- sort(unique(trips2019$nombre_estacion_origen))

# For those names that start with the number of the station (almost all of them), extract the number of the station, for the others keep the original name
trips2019$nombre_estacion_origen <- ifelse(trips2019$nombre_estacion_origen %in% estaciones19[!is.na(as.numeric(substr(estaciones19,1,3)))]  & !(trips2019$nombre_estacion_origen %in% vecweird) ,substr(trips2019$nombre_estacion_origen,7,10000) ,  trips2019$nombre_estacion_origen)

trips2019$nombre_estacion_destino <- ifelse(trips2019$nombre_estacion_destino %in% estaciones19[!is.na(as.numeric(substr(estaciones19,1,3)))]  & !(trips2019$nombre_estacion_destino %in% vecweird) ,substr(trips2019$nombre_estacion_destino,7,10000) ,  trips2019$nombre_estacion_destino)

# Clean names
trips2019$nombre_estacion_origen <- remove.accents(toupper(trips2019$nombre_estacion_origen))
trips2019$nombre_estacion_destino <- remove.accents(toupper(trips2019$nombre_estacion_destino))

trips2019$dia_orig <-  substr(trips2019$fecha_origen_recorrido,1,10)

# NORMALIZE STATIONS' NAMES
trips2019 <- regex_left_join(trips2019 , mergedf , by = 'nombre_estacion_origen')

trips2019$nombre_estacion_origen  <- ifelse(!is.na(trips2019$nombre_correct),trips2019$nombre_correct,trips2019$nombre_estacion_origen.x)

trips2019$nombre_estacion_origen.x = NULL
trips2019$nombre_estacion_origen.y = NULL
trips2019$nombre_correct = NULL

# NOW FOR DESTINATION 

trips2019 <- regex_left_join(trips2019 , mergedf2 , by ='nombre_estacion_destino')

trips2019$nombre_estacion_destino  <- ifelse(!is.na(trips2019$nombre_correct),trips2019$nombre_correct,trips2019$nombre_estacion_destino.x)

trips2019$nombre_estacion_destino.x = NULL
trips2019$nombre_estacion_destino.y = NULL
trips2019$nombre_correct = NULL


# CALCULATE THE DISTANCE OF EACH TRIP

trips2019 = trips2019 %>% left_join( st_drop_geometry(bikestops[,c('NUMERO','NOMBRE')]) , by = c('nombre_estacion_origen'='NOMBRE'))
names(trips2019)[names(trips2019)=='NUMERO']= 'o'

trips2019 = trips2019 %>% left_join( st_drop_geometry(bikestops[,c('NUMERO','NOMBRE')]) , by = c('nombre_estacion_destino'='NOMBRE'))
names(trips2019)[names(trips2019)=='NUMERO']= 'd'

trips2019$idx = paste0(trips2019$o,'-',trips2019$d)

trips2019 = trips2019 %>% left_join(distance_mg)

# See which stations actually have one trip done in each month (origin or destination)

trips2019$mes_orig = format(as.Date(trips2019$dia_orig),'%Y-%m')
meses_in = unique(trips2019$mes_orig)

for(m in meses_in){
  temp = trips2019[trips2019$mes_orig %in% m , ]
  stat_temp = unique(c(temp$nombre_estacion_origen,temp$nombre_estacion_destino))
  open[1:length(stat_temp) , which(names(open)==paste0(m,'-01'))] = stat_temp
}


saveRDS(open,'Data Argentina/Trips/open2.rds')

# There are some weird stuff going on from february 2019 to april 2020. In particular, the number of stations decrease by a lot. This could be due to the fact that the ownership of Ecobico changed in february 2020, and maybe they changed the structure of many of the stations and hence some of the bike stations were not available in those months. However, this can also be a problem of data entry. Consequently, I do an alternative database of 'open' where I assume that none of thee stations close in these months, and I add those stations who have not appeared before

## ENE - we have no data
open[,names(open) == '2019-01-01'] = open[,names(open) == '2018-12-01']

## FEB

# Get those stations who did not appear in the previous month
open02 = open[!open[,names(open) == '2019-02-01'] %in% open[,names(open) == '2019-01-01'],names(open) == '2019-02-01']     
# Clean
open02 = open02[-c(4,7,8)]

# Create the new vector of opened stations in that month
temp =  open[,names(open) == '2019-01-01']
temp = c(temp[!is.na(temp)],open02)

# Input
open[1:length(temp),names(open) == '2019-02-01'] = temp

## MAR

open03 = open[!open[,names(open) == '2019-03-01'] %in% open[,names(open) == '2019-02-01'],names(open) == '2019-03-01']     
open03 = open02[-c(1,2,4)]

temp =  open[,names(open) == '2019-02-01']
temp = c(temp[!is.na(temp)],open03)

open[1:length(temp),names(open) == '2019-03-01'] = temp

# ABR

open04 = open[!open[,names(open) == '2019-04-01'] %in% open[,names(open) == '2019-03-01'],names(open) == '2019-04-01']     
open04 = open04[-c(1,3,5)]

temp =  open[,names(open) == '2019-03-01']
temp = c(temp[!is.na(temp)],open04)

open[1:length(temp),names(open) == '2019-04-01'] = temp


##


trips2019 = trips2019[,- which(names(trips2019) %in% c('periodo','id_estacion_origen','long_estacion_origen','lat_estacion_origen','domicilio_estacion_origen','id_estacion_destino','long_estacion_destino','lat_estacion_destino','domicilio_estacion_destino','X','Id_recorrido','direccion_estacion_origen','direccion_estacion_destino'))]

short_sub = trips2019[trips2019$o %in% idx_min |trips2019$d %in% idx_min ,]
saveRDS(short_sub,'Data Argentina/Trips/Clean/trips2019_sh.rds')

saveRDS(trips2019,'Data Argentina/Trips/Clean/trips2019.rds')

rm(trips2019)


### Save the open df

saveRDS(open,'Data Argentina/Trips/open.rds')






