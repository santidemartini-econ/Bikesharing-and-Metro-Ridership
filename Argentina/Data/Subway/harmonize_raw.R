


#################################################################
##                    HARMONIZE    SUBWAYS                     ##
#################################################################

rm(list=ls())

library(sf)
library(dplyr)
library(mapview)
library(fuzzyjoin)

setwd('G:/My Drive/Transport Mode Choice/Argentina')
source("Code/remove_accents.R")

# NAMES IN SOURCE

subte.o = st_read('Data Argentina/Geo/Subte/estaciones-de-subte.shp') 
subte.o$ESTACION =   remove.accents(toupper(subte.o$ESTACION))



subte.o$estacion_id = paste0(subte.o$ESTACION,' - ',subte.o$LINEA)
v.subte.o = sort(subte.o$estacion_id)
 
sort(unique(subte.o$estacion_id))

## REPLACEMENT VECTORS

change_src <- c('LOS INCAS - B','MALABIA - B','MEDRANO - B','PASTEUR - B','ROSAS - B','TRONADOR - B','CALLAO - B' ,'PELLEGRINI - B',"MARIANO MORENO - C","GENERAL SAN MARTIN - C","AVENIDA DE MAYO - C","MINISTRO CARRANZA - D","PUEYRREDON. - D" ,"SCALABRINI ORTIZ - D","TRIBUNALES - D","CALLAO. - D","GENERAL BELGRANO - E","PZA. DE LOS VIRREYES - E","AVENIDA LA PLATA - E","ENTRE RIOS - E", "INDEPENDENCIA. - E" ,"INCLAN - H" ,"ONCE - H","HUMBERTO I - H", "PATRICIOS - H" , "PLAZA MISERERE - A","CONGRESO - A","FLORES - A", "SAENZ PE<D1>A - A", "PUEYRREDON.D - D" , "INDEPENDENCIA.H - E" ,"CALLAO.B - B" ,  "SAENZ PEÑA  - A" ,"SANTA FE - H", "FACULTAD DE DERECHO - H", "RETIRO E - E","AGÃ¼ERO - D")

change_to <- c('DE LOS INCAS -PQUE. CHAS - B','MALABIA - OSVALDO PUGLIESE - B','ALMAGRO - MEDRANO - B','PASTEUR - AMIA - B','JUAN MANUEL DE ROSAS - VILLA URQUIZA - B', "TRONADOR - VILLA ORTUZAR - B","CALLAO - MAESTRO ALFREDO BRAVO - B","C. PELLEGRINI - B","MORENO - C","SAN MARTIN - C","AV. DE MAYO - C","MINISTRO CARRANZA - MIGUEL ABUELO - D","PUEYRREDON - D", "R.SCALABRINI ORTIZ - D","TRIBUNALES - TEATRO COLON - D" ,'CALLAO - D',"BELGRANO - E","PLAZA DE LOS VIRREYES - EVA PERON - E","AV. LA PLATA - E","ENTRE RIOS - RODOLFO WALSH - E", "INDEPENDENCIA - E","INCLAN - MEZQUITA AL AHMAD - H" ,"ONCE - 30 DE DICIEMBRE - H" ,"HUMBERTO 1° - H" ,"PARQUE PATRICIOS - H","PLAZA DE MISERERE - A" ,  "CONGRESO - PDTE. DR. RAUL R. ALFONSIN - A"   ,"SAN JOSE DE FLORES - A","SAENZ PEÑA - A" , "PUEYRREDON - D" , "INDEPENDENCIA - E","CALLAO - MAESTRO ALFREDO BRAVO - B","SAENZ PEÑA - A","SANTA FE - CARLOS JAUREGUI - H", "FACULTAD DE DERECHO - JULIETA LANTERI - H",'RETIRO - E',"AGUERO - D" )

# Horas pico de 7 a 10 y de 16 a 19

mergedf <- data.frame(estacion_id = change_src , nombre_correct = change_to)

# FUNCTION TO EXTRACT ONLY LAST CHARACTER
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# 2014 
subte = read.csv('Data Argentina/Subway/Raw Data/molinetes_2014.csv',encoding = 'UTF-8')
subte$ESTACION = remove.accents(toupper(subte$ESTACION))
subte$estacion_id = paste0(subte$ESTACION,' - ',subte$LINEA)

# NORMALIZE VARS

subte <- regex_left_join(subte , mergedf , by = 'estacion_id')
subte$estacion_id  <- ifelse(!is.na(subte$nombre_correct),subte$nombre_correct,subte$estacion_id.x)

v.subte = sort(unique(subte$ESTACION))

unique(subte$estacion_id[which(!(subte$estacion_id %in% sort(subte.o$estacion_id)))])

# Set peak and nopeak
subte$hdesde = as.numeric(substr(subte$DESDE ,1,2))
subte$hhasta = as.numeric(substr(subte$HASTA ,1,2))

subte$peak = ifelse( (subte$hdesde %in% c(7,8,9) &  subte$hhasta %in% c(7,8,9)) |
                       (subte$hdesde %in% c(16,17,18) &  subte$hhasta %in% c(16,17,18)) ,1,0)

subte$nopeak = 1-subte$peak

subte$pases_peak = subte$PAX_TOTAL * subte$peak
subte$pases_nopeak = subte$PAX_TOTAL * subte$nopeak

# GROUP
grouped_f = subte %>% 
  group_by(FECHA,estacion_id) %>%
  summarise(pases = sum(PAX_TOTAL,na.rm=T),
            pases_peak = sum(pases_peak,na.rm=T),
            pases_nopeak = sum(pases_nopeak,na.rm=T))

names(grouped_f)[which(names(grouped_f)== 'FECHA')] ='fecha'


#### 2015
subte = read.csv('Data Argentina/Subway/Raw Data/molinetes-2015.csv',encoding = 'UTF-8')

## NORMALIZE NAMES
subte$estacion = remove.accents(toupper(subte$estacion))
subte$estacion_id = paste0(subte$estacion,' - ',substrRight(subte$linea,1))

# NORMALIZE VARS
subte <- regex_left_join(subte , mergedf , by = 'estacion_id')
subte$estacion_id  <- ifelse(!is.na(subte$nombre_correct),subte$nombre_correct,subte$estacion_id.x)

unique(subte$estacion_id[which(!(subte$estacion_id %in% sort(subte.o$estacion_id)))]) 

# Set peak and nopeak
subte$hdesde = as.numeric(substr(subte$desde ,1,2))
subte$hhasta = as.numeric(substr(subte$hasta ,1,2))

subte$peak = ifelse( (subte$hdesde %in% c(7,8,9) &  subte$hhasta %in% c(7,8,9)) |
                       (subte$hdesde %in% c(16,17,18) &  subte$hhasta %in% c(16,17,18)) ,1,0)

subte$nopeak = 1-subte$peak

subte$pases_peak = subte$total * subte$peak
subte$pases_nopeak = subte$total * subte$nopeak

## GROUP
grouped = subte %>% 
  group_by(fecha,estacion_id) %>%
  summarise(pases = sum(total,na.rm=T),
            pases_peak = sum(pases_peak,na.rm=T),
            pases_nopeak = sum(pases_nopeak,na.rm=T))


grouped_f = rbind(grouped_f,grouped)

# 2016 

subte = read.csv('Data Argentina/Subway/Raw Data/molinetes_2016.csv',encoding = 'UTF-8')

## NORMALIZE NAMES
subte$ESTACION = ifelse(subte$ESTACION =="SAENZ PE\xd1A" , 'SAENZ PEÑA' , subte$ESTACION)

subte$ESTACION = remove.accents(toupper(subte$ESTACION))
subte$estacion_id = paste0(subte$ESTACION,' - ',substrRight(subte$LINEA,1))

# NORMALIZE VARS
subte <- regex_left_join(subte , mergedf , by = 'estacion_id')
subte$estacion_id  <- ifelse(!is.na(subte$nombre_correct),subte$nombre_correct,subte$estacion_id.x)

unique(subte$estacion_id[which(!(subte$estacion_id %in% sort(subte.o$estacion_id)))]) 

# Set peak and nopeak
subte$hdesde = as.numeric(substr(subte$DESDE ,1,2))
subte$hhasta = as.numeric(substr(subte$HASTA ,1,2))

subte$peak = ifelse( (subte$hdesde %in% c(7,8,9) &  subte$hhasta %in% c(7,8,9)) |
                       (subte$hdesde %in% c(16,17,18) &  subte$hhasta %in% c(16,17,18)) ,1,0)

subte$nopeak = 1-subte$peak

subte$pases_peak = subte$TOTAL * subte$peak
subte$pases_nopeak = subte$TOTAL * subte$nopeak

# GROUP BY DAY
grouped = subte %>% 
  group_by(FECHA,estacion_id) %>%
  summarise(pases = sum(TOTAL,na.rm=T),
            pases_peak = sum(pases_peak,na.rm=T),
            pases_nopeak = sum(pases_nopeak,na.rm=T))

names(grouped)[which(names(grouped)== 'FECHA')] ='fecha'

grouped_f$fecha = as.Date(grouped_f$fecha)
grouped$fecha = as.Date(grouped$fecha,'%d/%m/%Y')

grouped_f = rbind(grouped_f,grouped)

# 2017

subte = read.csv('Data Argentina/Subway/Raw Data/molinetes_2017.csv',encoding = 'UTF-8')

## NORMALIZE NAMES
subte$ESTACION = ifelse(subte$ESTACION =="SAENZ PE\xd1A" , 'SAENZ PEÑA' , subte$ESTACION)

subte$ESTACION = remove.accents(toupper(subte$ESTACION))
subte$estacion_id = paste0(subte$ESTACION,' - ',substrRight(subte$LINEA,1))

# NORMALIZE VARS
subte <- regex_left_join(subte , mergedf , by = 'estacion_id')
subte$estacion_id  <- ifelse(!is.na(subte$nombre_correct),subte$nombre_correct,subte$estacion_id.x)

unique(subte$estacion_id[which(!(subte$estacion_id %in% sort(subte.o$estacion_id)))]) 

# Set peak and nopeak
subte$hdesde = as.numeric(substr(subte$DESDE ,1,2))
subte$hhasta = as.numeric(substr(subte$HASTA ,1,2))

subte$peak = ifelse( (subte$hdesde %in% c(7,8,9) &  subte$hhasta %in% c(7,8,9)) |
                       (subte$hdesde %in% c(16,17,18) &  subte$hhasta %in% c(16,17,18)) ,1,0)

subte$nopeak = 1-subte$peak

subte$pases_peak = subte$TOTAL * subte$peak
subte$pases_nopeak = subte$TOTAL * subte$nopeak


# GROUP VARS
grouped = subte %>% 
  group_by(FECHA,estacion_id) %>%
  summarise(pases = sum(TOTAL,na.rm=T),
            pases_peak = sum(pases_peak,na.rm=T),
            pases_nopeak = sum(pases_nopeak,na.rm=T))


names(grouped)[which(names(grouped)== 'FECHA')] ='fecha'

grouped$fecha = as.Date(grouped$fecha,'%d/%m/%Y')
grouped_f = rbind(grouped_f,grouped)

#### 2018

subte = read.csv('Data Argentina/Subway/Raw Data/molinetes-subte-18.csv',encoding = 'UTF-8')

## NORMALIZE NAMES
subte$estacion = ifelse(subte$estacion =="SAENZ PE\xd1A" , 'SAENZ PEÑA' , subte$estacion)

subte$estacion = remove.accents(toupper(subte$estacion))
subte$estacion_id = paste0(subte$estacion,' - ',substrRight(subte$linea,1))

# NORMALIZE VARS
subte <- regex_left_join(subte , mergedf , by = 'estacion_id')
subte$estacion_id  <- ifelse(!is.na(subte$nombre_correct),subte$nombre_correct,subte$estacion_id.x)

unique(subte$estacion_id[which(!(subte$estacion_id %in% sort(subte.o$estacion_id)))]) 

# Set peak and nopeak
subte$hdesde = as.numeric(substr(subte$desde ,1,2))
subte$hhasta = as.numeric(substr(subte$hasta ,1,2))

subte$peak = ifelse( (subte$hdesde %in% c(7,8,9) &  subte$hhasta %in% c(7,8,9)) |
                       (subte$hdesde %in% c(16,17,18) &  subte$hhasta %in% c(16,17,18)) ,1,0)

subte$nopeak = 1-subte$peak

subte$pases_peak = subte$total * subte$peak
subte$pases_nopeak = subte$total * subte$nopeak

# GROUP VARS

grouped = subte %>% 
  group_by(fecha,estacion_id) %>%
  summarise(pases = sum(total,na.rm=T),
            pases_peak = sum(pases_peak,na.rm=T),
            pases_nopeak = sum(pases_nopeak,na.rm=T))

# grouped$fecha = as.Date(grouped$fecha,'%d/%m/%Y')

grouped$fecha = as.Date(grouped$fecha)

grouped_f = rbind(grouped_f,grouped)


#### 2019

subte = read.csv('Data Argentina/Subway/Raw Data/datahistorica122019.csv', encoding = 'UTF-8')
# subte2 = read_csv('Data Argentina/Subway/Raw Data/datahistorica122019.csv')

## NORMALIZE NAMES
subte$estacion = ifelse(subte$estacion %in% c("SAENZ PE\xd1A","Saenz PeÃ\u0083Â±a ","Saenz PeÃ±a ") , 'SAENZ PEÑA' , 
                        ifelse(subte$estacion == "AgÃ\u0083Â¼ero" , 'AGUERO', subte$estacion))

subte$estacion = remove.accents(toupper(subte$estacion))
subte$estacion_id = paste0(subte$estacion,' - ',substrRight(subte$linea,1))

# NORMALIZE VARS
subte <- regex_left_join(subte , mergedf , by = 'estacion_id')
subte$estacion_id  <- ifelse(!is.na(subte$nombre_correct),subte$nombre_correct,subte$estacion_id.x)

unique(subte$estacion_id[which(!(subte$estacion_id %in% sort(subte.o$estacion_id)))]) 

# Set peak and nopeak
subte$hdesde = as.numeric(substr(subte$desde ,1,2))
subte$hhasta = as.numeric(substr(subte$hasta ,1,2))

subte$peak = ifelse( (subte$hdesde %in% c(7,8,9) &  subte$hhasta %in% c(7,8,9)) |
                       (subte$hdesde %in% c(16,17,18) &  subte$hhasta %in% c(16,17,18)) ,1,0)

subte$nopeak = 1-subte$peak

subte$pases_peak = subte$total * subte$peak
subte$pases_nopeak = subte$total * subte$nopeak



# GROUP VARS

grouped = subte %>% 
  group_by(fecha,estacion_id) %>%
  summarise(pases = sum(total,na.rm=T),
            pases_peak = sum(pases_peak,na.rm=T),
            pases_nopeak = sum(pases_nopeak,na.rm=T))

# grouped$fecha = as.Date(grouped$fecha,'%d/%m/%Y')

grouped$fecha = as.Date(grouped$fecha)

# names(grouped)[which(names(grouped)== 'FECHA')] ='fecha'

grouped_f = rbind(grouped_f,grouped)


# 2020

subte = read.csv('Data Argentina/Subway/Raw Data/historico2.csv',encoding = 'latin1')

## NORMALIZE NAMES
subte$ESTACION = ifelse(subte$ESTACION %in% c("SAENZ PE\xd1A","Saenz PeÃ\u0083Â±a ","Saenz PeÃ±a ","SAENZ PEÏ¿½A  - A") , 'SAENZ PEÑA' , 
                        ifelse(subte$ESTACION %in% c("AgÃ\u0083Â¼ero","AGÏ¿½ERO - D") , 'AGUERO', subte$ESTACION))

subte$ESTACION = remove.accents(toupper(subte$ESTACION))
subte$estacion_id = paste0(subte$ESTACION,' - ',substrRight(subte$LINEA,1))

# NORMALIZE VARS
subte <- regex_left_join(subte , mergedf , by = 'estacion_id')
subte$estacion_id  <- ifelse(!is.na(subte$nombre_correct),subte$nombre_correct,subte$estacion_id.x)

unique(subte$estacion_id[which(!(subte$estacion_id %in% sort(subte.o$estacion_id)))]) 

# Set peak and nopeak
subte$hdesde = as.numeric(substr(subte$DESDE ,1,2))
subte$hhasta = as.numeric(substr(subte$HASTA ,1,2))

subte$peak = ifelse( (subte$hdesde %in% c(7,8,9) &  subte$hhasta %in% c(7,8,9)) |
                       (subte$hdesde %in% c(16,17,18) &  subte$hhasta %in% c(16,17,18)) ,1,0)

subte$nopeak = 1-subte$peak

subte$pases_peak = subte$pax_TOTAL * subte$peak
subte$pases_nopeak = subte$pax_TOTAL * subte$nopeak


# GROUP VARS

grouped = subte %>% 
  group_by(FECHA,estacion_id) %>%
  summarise(pases = sum(pax_TOTAL,na.rm=T),
            pases_peak = sum(pases_peak,na.rm=T),
            pases_nopeak = sum(pases_nopeak,na.rm=T))

names(grouped)[which(names(grouped)== 'FECHA')] ='fecha'

grouped$fecha = as.Date(grouped$fecha,'%d/%m/%Y')
# grouped$fecha = as.Date(grouped$fecha)

grouped_f = rbind(grouped_f,grouped)

# 2021

subte = read.csv('Data Argentina/Subway/Raw Data/historico_2021.csv',sep = ';' ,fileEncoding  = 'latin1')


## NORMALIZE NAMES
subte$ESTACION = ifelse(subte$ESTACION %in% c("SAENZ PE\xd1A","Saenz PeÃ\u0083Â±a ","Saenz PeÃ±a ","Saenz PeÂ¤a "  ,"Saenz Peï¿½a" ,"Saenz PeÂ±a " ), 'SAENZ PEÑA' , 
                        ifelse(subte$ESTACION%in% c("AgÂ³ero" , "Agï¿½ero", "AgÂ\u0081ero", "AgÃ¼ero") ,  'AGUERO', subte$ESTACION))

subte$ESTACION = remove.accents(toupper(subte$ESTACION))
subte$estacion_id = paste0(subte$ESTACION,' - ',substrRight(subte$LINEA,1))

# NORMALIZE VARS
subte <- regex_left_join(subte , mergedf , by = 'estacion_id')
subte$estacion_id  <- ifelse(!is.na(subte$nombre_correct),subte$nombre_correct,subte$estacion_id.x)

unique(subte$estacion_id[which(!(subte$estacion_id %in% sort(subte.o$estacion_id)))]) 

# Set peak and nopeak
subte$hdesde = as.numeric(substr(subte$DESDE ,1,2))
subte$hhasta = as.numeric(substr(subte$HASTA ,1,2))

subte$peak = ifelse( (subte$hdesde %in% c(7,8,9) &  subte$hhasta %in% c(7,8,9)) |
                       (subte$hdesde %in% c(16,17,18) &  subte$hhasta %in% c(16,17,18)) ,1,0)

subte$nopeak = 1-subte$peak

subte$pases_peak = subte$pax_TOTAL * subte$peak
subte$pases_nopeak = subte$pax_TOTAL * subte$nopeak

# GROUP VARS

grouped = subte %>% 
  group_by(FECHA,estacion_id) %>%
  summarise(pases = sum(pax_TOTAL,na.rm=T),
            pases_peak = sum(pases_peak,na.rm=T),
            pases_nopeak = sum(pases_nopeak,na.rm=T))

names(grouped)[which(names(grouped)== 'FECHA')] ='fecha'
grouped$fecha = as.Date(grouped$fecha,'%d/%m/%Y')
# grouped$fecha = as.Date(grouped$fecha)

grouped_f = rbind(grouped_f,grouped)



# SAVE


saveRDS(grouped_f,'Data Argentina/Subway/grp_sbwy_daily.rds')

# 2022
# its a pain bc i have to merge many different databases. skip for now/ 


