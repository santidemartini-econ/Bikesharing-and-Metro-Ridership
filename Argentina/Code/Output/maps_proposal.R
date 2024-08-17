
##################################################################
##                     MAPA ARGENTINA                           ##
##################################################################

rm(list=ls())


library(sf)
library(dplyr)
library(mapview)
library(lubridate)
library(tmap)
library(leaflegend)
library(RColorBrewer)
library(FNN)
library(ggplot2)

# setwd('H:/Mi unidad/Transport Mode Choice/Data Argentina')
setwd('E:/My Drive/Transport Mode Choice')

source("Code/remove_accents.R")

########### BIKE STATIONS
bikestops <- st_read('Data Argentina/Geo/Bicicleteros/IE-Estaciones.shp')

############### OPEN ESTACIONES
subte <- st_read('Data Argentina/Geo/Subte/estaciones-de-subte.shp')
mapview(subte)



#### I want to connect the lines of metro in baires

### Line A
A = subte[subte$LINEA=='A',]

# Get the order of connection by visualizing the map
tempA1 <- c(17,18,15,16,13,12,11,10,9,8,7,6,5,4,3,2,1)
tempA2 <- c(18,15,16,13,12,11,10,9,8,7,6,5,4,3,2,1,14)

# Change coordinates
A = st_as_sf(A,crs=4326)

# Do a new df
Alines <- cbind(A[tempA1,],A[tempA2,])

# Connect lines
geometry <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, Alines$geometry, Alines$geometry.1, SIMPLIFY=FALSE))

# Change format
linesA <- st_sf(geometry,crs = 4326)
linesA$linea <- 'A'
# mapview(linesA)


### Line B
B = subte[subte$LINEA=='B',]

# Get the order of connection by visualizing the map
tempB1 <- c(17,16,15,14,12,11,10,9,8,7,6,5,4,3,2,1)
tempB2 <- c(16,15,14,12,11,10,9,8,7,6,5,4,3,2,1,13)

# Change coordinates
B = st_as_sf(B,crs=4326)

# Do a new df
Blines <- cbind(B[tempB1,],B[tempB2,])

# Connect lines
geometry <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, Blines$geometry, Blines$geometry.1, SIMPLIFY=FALSE))

# Change format
linesB <- st_sf(geometry,crs = 4326)
linesB$linea <- 'B'
# mapview(linesB)


### Line C
C = subte[subte$LINEA=='C',]

# Get the order of connection by visualizing the map
tempC1 <- c(7,9,6,5,4,3,2,8)
tempC2 <- c(9,6,5,4,3,2,8,1)

# Change coordinates
C = st_as_sf(C,crs=4326)

# Do a new df
Clines <- cbind(C[tempC1,],C[tempC2,])

# Connect lines
geometry <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, Clines$geometry, Clines$geometry.1, SIMPLIFY=FALSE))

# Change format
linesC <- st_sf(geometry,crs = 4326)
linesC$linea <- 'C'
# mapview(linesC)

### Line D
D = subte[subte$LINEA=='D',]

# Get the order of connection by visualizing the map
tempD1 <- c(16,14,13,12,11,7,6,5,8,4,9,2,10,3,1)
tempD2 <- c(14,13,12,11,7,6,5,8,4,9,2,10,3,1,15)

# Change coordinates
D = st_as_sf(D,crs=4326)

# Do a new df
Dlines <- cbind(D[tempD1,],D[tempD2,])

# Connect lines
geometry <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, Dlines$geometry, Dlines$geometry.1, SIMPLIFY=FALSE))

# Change format
linesD <- st_sf(geometry,crs = 4326)
linesD$linea <- 'D'
# mapview(linesD)




### Line E
E = subte[subte$LINEA=='E',]
# mapview(E)
# Get the order of connection by visualizing the map
tempE1 <- c(14,15,11,10,9,12,13,8,7,6,5,4,3,2,1,18,17)
tempE2 <- c(15,11,10,9,12,13,8,7,6,5,4,3,2,1,18,17,16)

# Change coordinates
E = st_as_sf(E,crs=4326)

# Do a new df
Elines <- cbind(E[tempE1,],E[tempE2,])

# Connect lines
geometry <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, Elines$geometry, Elines$geometry.1, SIMPLIFY=FALSE))

# Change format
linesE <- st_sf(geometry,crs = 4326)
linesE$linea <- 'E'
# mapview(linesE)



### Line H
H = subte[subte$LINEA=='H',]
# mapview(H)
# Get the order of connection by visualizing the map
tempH1 <- c(8,7,1,2,3,4,5,6,9,11,10)
tempH2 <- c(7,1,2,3,4,5,6,9,11,10,12)

# Change coordinates
H = st_as_sf(H,crs=4326)

# Do a new df
Hlines <- cbind(H[tempH1,],H[tempH2,])

# Connect lines
geometry <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, Hlines$geometry, Hlines$geometry.1, SIMPLIFY=FALSE))

# Change format
linesH <- st_sf(geometry,crs = 4326)
linesH$linea <- 'H'
# mapview(linesH)

### Join all the lines into one plot

subway <- rbind(linesA,linesB,linesC,linesD,linesE,linesH)


########## TRAIN

tren =  st_read('Data Argentina/Geo/Ferrocarril/estaciones_ferrocarril.shp')


### Line Mitre
mitre = tren[tren$linea=='Mitre',]

a1 = c(19,1,13,17,20,24,18)
a2 = c(1,13,17,20,24,18,42)

b1 = c(19,8,4,2,3 ,10 ,7,6 ,5,9,43,37,42,40)
b2 = c(8,4,2,3 ,10 ,7,6 ,5,9,43,37,42,40,41)

c1 = c(19 ,8 ,4,2 ,51,49,54,53,56, 55,52, 57,48)
c2 = c(8 ,4,2 ,51,49,54,53,56, 55, 52,57, 48,50)


tempA1 = c(a1,b1,c1)
tempA2 = c(a2,b2,c2)

# Change coordinates
mitre = st_as_sf(mitre,crs=4326)

# Do a new df
mitrelines <- cbind(mitre[tempA1,],mitre[tempA2,])

# Connect lines
geometry <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, mitrelines$geometry, mitrelines$geometry.1, SIMPLIFY=FALSE))

# Change format
linesmitre <- st_sf(geometry,crs = 4326)
linesmitre$linea <- 'Mitre'
# mapview(linesmitre)


### Line Belgrano Sur
belgrano = tren[tren$linea=='Belgrano Sur',]


tempA1 = c(2,16,10,14,15,8,13,1,12,4,5)
tempA2 = c(16,10,14,15,8,13,1,12,4,5,3)

# Change coordinates
belgrano = st_as_sf(belgrano,crs=4326)

# Do a new df
belgranolines <- cbind(belgrano[tempA1,],belgrano[tempA2,])

# Connect lines
geometry <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, belgranolines$geometry, belgranolines$geometry.1, SIMPLIFY=FALSE))

# Change format
linesbelgrano <- st_sf(geometry,crs = 4326)
linesbelgrano$linea <- 'Belgrano'
# mapview(linesbelgrano)


### Line Roca
roca = tren[tren$linea=='Roca',]


a1 = c(56,51,66,67,69,71,57,54)
a2 = c(51,66,67,69,71,57,54,64)

b1 = c(56,51,66,50,45)
b2 = c(51,66,50,45,52)

tempA1 = c(a1,b1)
tempA2 = c(a2,b2)


# Change coordinates
roca = st_as_sf(roca,crs=4326)

# Do a new df
rocalines <- cbind(roca[tempA1,],roca[tempA2,])

# Connect lines
geometry <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, rocalines$geometry, rocalines$geometry.1, SIMPLIFY=FALSE))

# Change format
linesroca <- st_sf(geometry,crs = 4326)
linesroca$linea <- 'Belgrano'
# mapview(linesroca)




### Line San Martin
sanmar = tren[tren$linea=='San Martin',]


tempA1 = c(20,19,15,2,14,7,13,3)
tempA2 = c(19,15,2,14,7,13,3,4)


# Change coordinates
sanmar = st_as_sf(sanmar,crs=4326)

# Do a new df
sanmarlines <- cbind(sanmar[tempA1,],sanmar[tempA2,])

# Connect lines
geometry <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, sanmarlines$geometry, sanmarlines$geometry.1, SIMPLIFY=FALSE))

# Change format
linessanmar <- st_sf(geometry,crs = 4326)
linessanmar$linea <- 'San Martin'
# mapview(linessanmar)


### Line Sarmiento
sarmiento = tren[tren$linea=='Sarmiento',]


tempA1 = c(8,1,3,4,10,6,2,9,5,7,11)
tempA2 = c(1,3,4,10,6,2,9,5,7,11,12)


# Change coordinates
sarmiento = st_as_sf(sarmiento,crs=4326)

# Do a new df
sarmientolines <- cbind(sarmiento[tempA1,],sarmiento[tempA2,])

# Connect lines
geometry <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, sarmientolines$geometry, sarmientolines$geometry.1, SIMPLIFY=FALSE))

# Change format
linessarmiento<- st_sf(geometry,crs = 4326)
linessarmiento$linea <- 'Sarmiento'
# mapview(linessarmiento)

trenes <- rbind(linesroca,linesmitre,linessanmar,linesbelgrano,linessarmiento)
mapview(trenes)


############### METROBUS


metrobus =  st_read('Data Argentina/Geo/Metrobus/Recorridos_MTB_SEP23.shp')


######### BIND ALL
metrobus$type = 'BRT'
trenes$type = 'Train'
subway$type = 'Subway'

transportmodes = rbind(metrobus[,c('geometry','type')],trenes[,c('geometry','type')],subway[,c('geometry','type')] )
paleta = brewer.pal(3,'BrBG')

names(transportmodes)[1] = 'Mode'

mapa <-   tm_shape(transportmodes   )+
  tm_lines( 'Mode' , legend.show=T ,pal = c('blue','green','black') )+
  tm_view(set.view = c(-58.42, -34.62, 12))  +
  tm_shape(bikestops) +
  tm_dots(size=0.1 , col = 'red')


printmap = tmap_leaflet(mapa) 

mapview::mapshot(printmap, file = "Output/fullmap.png")


#####  MAPA ROLLOUT

bikestops <- st_read('Data Argentina/Geo/Bicicleteros/IE-Estaciones.shp')
bikestops$NOMBRE <- remove.accents(toupper(bikestops$NOMBRE))

df_estac <- read.csv('Data Argentina/Trips/inauguracionesh22.csv')

bikestops <- left_join(bikestops,df_estac , by = c('NOMBRE'= 'estacion'))
bikestops$inauguracion <- as.Date(bikestops$inauguracion)

bikestops$y_inauguracion <- year(bikestops$inauguracion)


bikestops14 <- bikestops[bikestops$y_inauguracion < 2015,]
bikestops16 <- bikestops[bikestops$y_inauguracion < 2017,]
bikestops18 <- bikestops[bikestops$y_inauguracion < 2019,]
bikestops20 <- bikestops[bikestops$y_inauguracion < 2021,]
bikestops22 <- bikestops[bikestops$y_inauguracion < 2023,]



mapa14 <-   tm_shape(transportmodes   )+
  tm_lines( 'Mode' , legend.show=T ,pal = c('blue','green','black') )+
  tm_view(set.view = c(-58.42, -34.62, 12))  +
  tm_shape(bikestops14) +
  tm_dots(size=0.1 , col = 'red')

printmap14 = tmap_leaflet(mapa14) 

mapa16 <-   tm_shape(transportmodes   )+
  tm_lines( 'Mode' , legend.show=T ,pal = c('blue','green','black') )+
  tm_view(set.view = c(-58.42, -34.62, 12))  +
  tm_shape(bikestops16) +
  tm_dots(size=0.1 , col = 'red')

printmap16 = tmap_leaflet(mapa16)

mapa18 <-   tm_shape(transportmodes   )+
  tm_lines( 'Mode' , legend.show=T ,pal = c('blue','green','black') )+
  tm_view(set.view = c(-58.42, -34.62, 12))  +
  tm_shape(bikestops18) +
  tm_dots(size=0.1 , col = 'red')

printmap18 = tmap_leaflet(mapa18) 


mapa20 <-   tm_shape(transportmodes   )+
  tm_lines( 'Mode' , legend.show=T ,pal = c('blue','green','black') )+
  tm_view(set.view = c(-58.42, -34.62, 12))  +
  tm_shape(bikestops20) +
  tm_dots(size=0.1 , col = 'red')

printmap20 = tmap_leaflet(mapa20) 

mapview::mapshot(printmap14, file = "Output/map14.png")
mapview::mapshot(printmap16, file = "Output/map16.png")
mapview::mapshot(printmap18, file = "Output/map18.png")
mapview::mapshot(printmap20, file = "Output/map20.png")




#### CDF of openings


dates = format(seq(as.Date('2010-01-01'),as.Date('2024-12-01'),by='1 month'),'%Y-%m')

bikestops$inaug_m = format(bikestops$inauguracion,'%Y-%m')

tabinaug = data.frame(table(bikestops$inaug_m))
names(tabinaug)[1] = 'inaug_m'

ts_inaug = data.frame( inaug_m =  dates , vecs = 0  )
ts_inaug = ts_inaug %>% left_join(tabinaug)

ts_inaug$number= ifelse(!is.na(ts_inaug$Freq),ts_inaug$Freq,0)
ts_inaug$csum = cumsum(ts_inaug$number)
ts_inaug$datecomp = as.Date(paste0(ts_inaug$inaug_m , '-01') )

ts_inaug_f = ts_inaug[ts_inaug$datecomp > '2013-12-01' & ts_inaug$datecomp < '2024-12-01', ]


plot_opening <- ggplot(ts_inaug_f, aes(datecomp,csum)) +
  geom_line() +
  xlab('') +
  ylab('N Stations')  +
  labs(title = 'Number of Stations by month')+
  theme_classic()

ggsave('Output/tseries_open.png' ,plot = plot_opening)




