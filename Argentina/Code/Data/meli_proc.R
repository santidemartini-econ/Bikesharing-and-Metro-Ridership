
##################################################################
##                             MELI                             ##
##################################################################


rm(list=ls())

library(dplyr)
library(broom)
library(ggplot2)
library(forcats)
library(sf)
library(mapview)
library(lubridate)

setwd('G:/My Drive/Transport Mode Choice/Argentina')

source("Code/remove_accents.R")

##### 

inmuebles = c('Casas','Consultorios','Departamentos','Locales','Oficinas','PH')
colkeep = c('MesListing','ITE_BASE_CURRENT_PRICE','ITE_CURRENT_PRICE','ITE_SITE_CURRENT_PRICE','SIT_CURRENCY_ID','OPERACION','TIPOPROPIEDAD','TIPOPROPIEDADNORM','STotalM2','SConstrM2','ITE_ADD_CITY_NAME','ITE_ADD_STATE_NAME','ITE_ADD_NEIGHBORHOOD_NAME','LATITUDE','LONGITUDE','VISITASSTD','VISITASIOS','VISITASANDROID','VISITASMOBILE','FAVORITOS','PREGUNTAS','CAT_L2')
operacion = c('Alquiler','Venta')

## 2017
data = read.csv('Data Argentina/MELI/MLA_RE_2017.csv', encoding = 'UTF-8')

data = data[,colkeep]
filbase1 = quantile(data$ITE_BASE_CURRENT_PRICE,p=0.01)
filbase2 = quantile(data$ITE_BASE_CURRENT_PRICE,p=0.99)

fil_base1 = quantile(data$ITE_CURRENT_PRICE,p=0.01)
fil_base2 = quantile(data$ITE_CURRENT_PRICE,p=0.99)

data = data %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
           )

## 2018
data2 = read.csv('Data Argentina/MELI/MLA_RE_2018.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)

#### 2019

# ene

data2 = read.csv('Data Argentina/MELI/REALESTATE_MLA_AMBA_201901.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)


# feb
data2 = read.csv('Data Argentina/MELI/REALESTATE_MLA_AMBA_201902.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)




# mar
data2 = read.csv('Data Argentina/MELI/REALESTATE_MLA_AMBA_201903.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)



# apr
data2 = read.csv('Data Argentina/MELI/REALESTATE_MLA_AMBA_201904.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01,na.rm = T)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99,na.rm = T)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01,na.rm = T)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99,na.rm = T)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)



# may
data2 = read.csv('Data Argentina/MELI/REALESTATE_MLA_AMBA_201905.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01,na.rm = T)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99,na.rm = T)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01,na.rm = T)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99,na.rm = T)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)



# jun
data2 = read.csv('Data Argentina/MELI/REALESTATE_MLA_AMBA_201906.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01,na.rm = T)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99,na.rm = T)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01,na.rm = T)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99,na.rm = T)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)



# jul
data2 = read.csv('Data Argentina/MELI/REALESTATE_MLA_AMBA_201907.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01,na.rm = T)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99,na.rm = T)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01,na.rm = T)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99,na.rm = T)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)



# aug
data2 = read.csv('Data Argentina/MELI/REALESTATE_MLA_AMBA_201908.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01,na.rm = T)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99,na.rm = T)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01,na.rm = T)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99,na.rm = T)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)



# sep
data2 = read.csv('Data Argentina/MELI/REALESTATE_MLA_AMBA_201909.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01,na.rm = T)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99,na.rm = T)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01,na.rm = T)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99,na.rm = T)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)



# oct
data2 = read.csv('Data Argentina/MELI/REALESTATE_MLA_AMBA_201910.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01,na.rm = T)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99,na.rm = T)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01,na.rm = T)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99,na.rm = T)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)



# nov
data2 = read.csv('Data Argentina/MELI/REALESTATE_MLA_AMBA_201911.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01,na.rm = T)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99,na.rm = T)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01,na.rm = T)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99,na.rm = T)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)



# dic
data2 = read.csv('Data Argentina/MELI/REALESTATE_MLA_AMBA_201912.csv', encoding = 'UTF-8')

data2 = data2[,colkeep]
filbase1 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.01,na.rm = T)
filbase2 = quantile(data2$ITE_BASE_CURRENT_PRICE,p=0.99,na.rm = T)

fil_base1 = quantile(data2$ITE_CURRENT_PRICE,p=0.01,na.rm = T)
fil_base2 = quantile(data2$ITE_CURRENT_PRICE,p=0.99,na.rm = T)

data2 = data2 %>%
  filter(CAT_L2 %in% inmuebles &
           ITE_ADD_STATE_NAME == 'Capital Federal' &
           STotalM2 >=  SConstrM2 &
           !is.na(ITE_CURRENT_PRICE) &
           !is.na(ITE_BASE_CURRENT_PRICE) &
           ITE_BASE_CURRENT_PRICE >= filbase1 &
           ITE_BASE_CURRENT_PRICE <= filbase2 &
           ITE_CURRENT_PRICE >= fil_base1 &
           ITE_CURRENT_PRICE <= fil_base2 &
           OPERACION %in% operacion
  )

data = rbind(data,data2)

data = data %>% filter(ITE_ADD_STATE_NAME == 'Capital Federal')

## Some further rearrangements

# Read Exchange rata 

er = read.csv('Data Argentina/Argentina/er.csv')

er$Compra = as.numeric(gsub( ',','\\.',er$Compra))
er$Venta = as.numeric(gsub( ',','\\.',er$Venta))


er$exch =( as.numeric(er$Compra) + er$Venta ) /2

er$Fecha = as.Date(er$Fecha,'%d/%m/%Y')

er$mes = paste0(year(er$Fecha),' - ', month(er$Fecha))

er = er %>%
  group_by(mes) %>% 
  summarise(excha = mean(exch,na.rm=T))


## Set everything in dollars 
data$date = as.Date(data$MesListing,'%Y-%m-%d')

data$mes =   paste0(year(data$date),' - ', month(data$date))
data = left_join(data,er)

data = data %>% filter(!is.na(SIT_CURRENCY_ID))

data$price = ifelse(data$SIT_CURRENCY_ID == 'ARG   ', data$ITE_CURRENT_PRICE/data$excha ,data$ITE_CURRENT_PRICE )

data = data %>% filter(!(OPERACION =='Venta' & SIT_CURRENCY_ID == 'ARG   ' ) & ITE_ADD_STATE_NAME == 'Capital Federal')

write.csv(data,'Data Argentina/MELI/Clean/fullcsv.csv')

## Set it to sf
# data2 = data
data = data %>% filter(!(is.na(data$LONGITUDE) | is.na(data$LATITUDE) ))

data.alq = data %>% filter(OPERACION == 'Alquiler')
data.vta = data %>% filter(OPERACION == 'Venta')

data.alq = st_as_sf(data.alq , coords=c('LONGITUDE','LATITUDE'),crs=4326)
data.vta = st_as_sf(data.vta , coords=c('LONGITUDE','LATITUDE'),crs=4326)

st_write(data.alq,'Data Argentina/MELI/Clean/cleanalq.gpkg',delete_dsn = T)
st_write(data.vta,'Data Argentina/MELI/Clean/cleanvta.gpkg',delete_dsn = T)
