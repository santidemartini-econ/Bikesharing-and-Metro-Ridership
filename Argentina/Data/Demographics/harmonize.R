
##################################################################
##                       SURVEY HARMONIZE                       ##
##################################################################

rm(list=ls())

library(sf)
library(dplyr)
library(mapview)

setwd('G:/My Drive/Transport Mode Choice/Argentina')

source('Code/remove_accents.R')
# Read Data

# I want to create: avg age, share male, yrs schooling, share working
yrs = c('2014','2015','2016','2017','2018','2019')

# Loop to create variables by neighborhood
for(y in yrs){

path = paste0('Data Argentina/Demographics/encuesta-anual-hogares-',y,'.csv')

data = read.csv(path,check.names = F)

data$ocupado = ifelse(data$estado_ocupacional == 'Ocupado',1,0)
data$male = ifelse(data$estado_ocupacional == 'Mujer',0,1)
if(y %in% c('2018','2019')) {
  data$años_escolaridad = as.numeric(data[,27])
}else{
  
  data$años_escolaridad = as.numeric(data$años_escolaridad)
}
data1 = data %>%
  group_by(comuna) %>%
  summarise(
    age = mean(edad,na.rm=T),
    male = mean(male,na.rm=T),
    yrschool = mean(años_escolaridad,na.rm = T),
    share_work = mean(ocupado , na.rm = T),
    
  )

data1$year = y

if(y=='2014'){
  data_f = data1
}else{
  data_f = rbind(data1,data_f)
  
}

}

# Now map each station to a nbhd ('comuna')


# Subways
subte <- st_read('Data Argentina/Geo/Subte/estaciones-de-subte.shp')

subte$ESTACION = remove.accents(subte$ESTACION)


# Comunas
comunas = st_read('Data Argentina/Geo/comunas_caba_censo_2010/comunas_censo_2010.shp')

comunas = st_transform(comunas, 4326)


indexes = st_join(comunas,subte)

indexes$estacion_id = paste0(indexes$ESTACION,' - ',indexes$LINEA)

names(indexes)[1] = 'comuna'

indexes = indexes[!is.na(indexes$ESTACION),]

data_u = st_drop_geometry(indexes[,c('comuna','estacion_id')]) %>% left_join(data_f , relationship = 'many-to-many')



write.csv(data_u,'Data Argentina/Demographics/demos_final.csv')

