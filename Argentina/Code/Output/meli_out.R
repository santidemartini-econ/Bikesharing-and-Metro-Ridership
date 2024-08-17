

########################################################################
##                             MELI - RENT                            ##
########################################################################


rm(list=ls())

library(dplyr)
library(broom)
library(ggplot2)
library(forcats)
library(sf)
library(mapview)
library(lubridate)
library(data.table)
library(lmtest)
library(kableExtra)

setwd('G:/My Drive/Transport Mode Choice/Argentina')

data.alq = st_read('Data Argentina/MELI/Clean/cleanalq.gpkg')
subway = st_read('Data Argentina/Geo/Subte/estaciones-de-subte.shp')

# Determine the central longitude of your data
central_longitude <- (st_bbox(data.alq)[1] + st_bbox(data.alq)[3]) / 2

# Determine the UTM zone based on the central longitude
utm_zone <- floor((central_longitude + 180) / 6) + 1

# Define the appropriate UTM CRS
utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m")

# CHANGE EPSPS
data.alq = st_transform(data.alq, utm_crs)
subway = st_transform(subway, utm_crs)

# data.alq = data.alq %>% filter(ITE_ADD_STATE_NAME == 'Capital Federal')
## Calculate distances

matdists = st_distance(data.alq,subway)
# a = data.frame(matdists)

# Drop geometry
data.alq = st_drop_geometry(data.alq)

# Now we want to create a binary matrix to get discrete measures
matdists_400 = matrix(as.numeric(as.numeric(matdists) < 400),nrow=nrow(matdists),ncol = ncol(matdists)) 

# Get an index of how the columns will be named after cbinding
subway$estacion_id = paste0(subway$ESTACION , ' - ',subway$LINEA)

idx_vars= paste0(subway$estacion_id,' - 400')

# Merge the within 400 meters
gc()
data.alq = cbind(data.alq,matdists_400)
names(data.alq)[(ncol(data.alq)-ncol(matdists_400)+1):ncol(data.alq)] = idx_vars


# Create additional variables
data.alq$pm2 = data.alq$price/(data.alq$SConstrM2 + (data.alq$STotalM2 - data.alq$SConstrM2)/2)

data.alq$visitas = data.alq$VISITASANDROID + data.alq$VISITASIOS + data.alq$VISITASMOBILE + data.alq$VISITASSTD 

# Wide to long

data.alq.l = melt(setDT(data.alq), id.vars = c('MesListing','pm2','visitas','TIPOPROPIEDADNORM'), measure.vars = which(names(data.alq) %in% idx_vars), variable.name = "Station")

data.alq.l = as.data.frame(data.alq.l)

# Read data with index of when each station was open
tdates = readRDS('Data Argentina/Temp/treat_dates.RDS')
tdates$f = ifelse(is.na(tdates$f),'2025-01',tdates$f)

tdates$f_d = as.Date(paste0(tdates$f,'-01'))


outlist = tdates$estacion_id[tdates$f_d< as.Date('2017-02-01') | tdates$f_d> as.Date('2019-12-01')]

# Keep only commercial buildings
data_com = data.alq.l %>% filter(TIPOPROPIEDADNORM %in% c('Local','Oficina'))


# Stats

data.comf = data_com %>% filter(value==1)

stats.com = data.comf %>%
  group_by(MesListing,Station) %>%
  summarise(
    pm2 = median(pm2,na.rm=T),
    visitas = median(visitas,na.rm=T) ,
    ene = n()
  )

stats.com$Station = as.character(stats.com$Station)

stats.com$linea = substr(stats.com$Station,nchar(stats.com$Station)-6 ,nchar(stats.com$Station)-5)

stats.com$pm2 <- ifelse(is.infinite(stats.com$pm2),NA,stats.com$pm2)
stats.com$visitas = ifelse(is.na(stats.com$visitas),0,stats.com$visitas) 

stats.com$estacion_id = substr(as.character(stats.com$Station), 1, nchar(as.character(stats.com$Station))-6)


stats.com = stats.com %>% left_join(tdates)
stats.com$MesListing = as.Date(stats.com$MesListing)


stats.com$t = ifelse(stats.com$f_d >stats.com$MesListing , 0 , 1)

# Models
mod.com.pm2 = lm(pm2~t + as.factor(estacion_id) + as.factor(MesListing) , stats.com[!(stats.com$estacion_id %in% outlist), ])

mod.com.n = lm(ene~t + as.factor(estacion_id) + as.factor(MesListing) , stats.com[!(stats.com$estacion_id %in% outlist), ])

mod.com.vis = lm(visitas~t + as.factor(estacion_id) + as.factor(MesListing) , stats.com[!(stats.com$estacion_id %in% outlist), ])

##  Cluster SE
# 1
mod.com.pm2.cl = coeftest(mod.com.pm2,
                             vcov = vcovCL,
                             cluster = ~linea)

mod.com.pm2.tidy = tidy(mod.com.pm2.cl) %>% 
  filter(term %in% c('t'))

# 2
mod.com.n.cl = coeftest(mod.com.n,
                          vcov = vcovCL,
                          cluster = ~linea)

mod.com.n.tidy = tidy(mod.com.n.cl) %>% 
  filter(term %in% c('t'))

# 3
mod.com.vis.cl = coeftest(mod.com.vis,
                        vcov = vcovCL,
                        cluster = ~linea)

mod.com.vis.tidy = tidy(mod.com.vis.cl) %>% 
  filter(term %in% c('t'))



#### Repeat similar thing but for non-commercial properties

# Keep only commercial buildings - AVOID HOUSES BC BIAS AND ARE THE LEAST
data_res = data.alq.l %>% filter(TIPOPROPIEDADNORM %in% c('Departamento'))


data.resf = data_res %>% filter(value==1)

stats.res = data.resf %>%
  group_by(MesListing,Station) %>%
  summarise(
    pm2 = median(pm2,na.rm=T),
    visitas = median(visitas,na.rm=T) ,
    ene = n()
  )

stats.res$Station = as.character(stats.res$Station)

stats.res$linea = substr(stats.res$Station,nchar(stats.res$Station)-6 ,nchar(stats.res$Station)-5)

stats.res$pm2 <- ifelse(is.infinite(stats.res$pm2),NA,stats.res$pm2)
stats.res$visitas = ifelse(is.na(stats.res$visitas),0,stats.res$visitas) 

stats.res$estacion_id = substr(as.character(stats.res$Station), 1, nchar(as.character(stats.res$Station))-6)

# Read data with index of when each station was open
stats.res = stats.res %>% left_join(tdates)
stats.res$MesListing = as.Date(stats.res$MesListing)


stats.res$t = ifelse(stats.res$f_d >stats.res$MesListing , 0 , 1)

# Models
mod.res.pm2 = lm(pm2~t + as.factor(estacion_id) + as.factor(MesListing) , stats.res[!(stats.res$estacion_id %in% outlist), ])

mod.res.n = lm(ene~t + as.factor(estacion_id) + as.factor(MesListing) , stats.res[!(stats.res$estacion_id %in% outlist), ])

mod.res.vis = lm(visitas~t + as.factor(estacion_id) + as.factor(MesListing) , stats.res[!(stats.res$estacion_id %in% outlist), ])


##  Cluster SE
# 1
mod.res.pm2.cl = coeftest(mod.res.pm2,
                          vcov = vcovCL,
                          cluster = ~linea)

mod.res.pm2.tidy = tidy(mod.res.pm2.cl) %>% 
  filter(term %in% c('t'))

# 2
mod.res.n.cl = coeftest(mod.res.n,
                        vcov = vcovCL,
                        cluster = ~linea)

mod.res.n.tidy = tidy(mod.res.n.cl) %>% 
  filter(term %in% c('t'))

# 3
mod.res.vis.cl = coeftest(mod.res.vis,
                          vcov = vcovCL,
                          cluster = ~linea)

mod.res.vis.tidy = tidy(mod.res.vis.cl) %>% 
  filter(term %in% c('t'))

###### DO TABLE

### COMMERCIAL PREPARE

mod.com.pm2.tidy$estimate = round(mod.com.pm2.tidy$estimate,3)
mod.com.pm2.tidy$std.error = round(mod.com.pm2.tidy$std.error,3)

mod.com.n.tidy$estimate = round(mod.com.n.tidy$estimate,3)
mod.com.n.tidy$std.error = round(mod.com.n.tidy$std.error,3)

mod.com.vis.tidy$estimate = round(mod.com.vis.tidy$estimate,3)
mod.com.vis.tidy$std.error = round(mod.com.vis.tidy$std.error,3)

## Add Stars

# PM2
mod.com.pm2.tidy$estimate = ifelse(mod.com.pm2.tidy$p.value<0.01,paste0(mod.com.pm2.tidy$estimate,'***') ,
                                   ifelse(mod.com.pm2.tidy$p.value>0.01 & mod.com.pm2.tidy$p.value<0.05, paste0(mod.com.pm2.tidy$estimate,'**') , 
                                          ifelse( mod.com.pm2.tidy$p.value>0.05 & mod.com.pm2.tidy$p.value<0.1, paste0(mod.com.pm2.tidy$estimate,'*'),
                                                  mod.com.pm2.tidy$estimate)))  

# N
mod.com.n.tidy$estimate = ifelse(mod.com.n.tidy$p.value<0.01,paste0(mod.com.n.tidy$estimate,'***') ,
                                     ifelse(mod.com.n.tidy$p.value>0.01 & mod.com.n.tidy$p.value<0.05, paste0(mod.com.n.tidy$estimate,'**') , 
                                            ifelse( mod.com.n.tidy$p.value>0.05 & mod.com.n.tidy$p.value<0.1, paste0(mod.com.n.tidy$estimate,'*'),
                                                    mod.com.n.tidy$estimate)))  

# VISITAS
mod.com.vis.tidy$estimate = ifelse(mod.com.vis.tidy$p.value<0.01,paste0(mod.com.vis.tidy$estimate,'***') ,
                                  ifelse(mod.com.vis.tidy$p.value>0.01 & mod.com.vis.tidy$p.value<0.05, paste0(mod.com.vis.tidy$estimate,'**') , 
                                         ifelse( mod.com.vis.tidy$p.value>0.05 & mod.com.vis.tidy$p.value<0.1, paste0(mod.com.vis.tidy$estimate,'*'),
                                                 mod.com.vis.tidy$estimate)))  

### RESIDENTIAL

mod.res.pm2.tidy$estimate = round(mod.res.pm2.tidy$estimate,3)
mod.res.pm2.tidy$std.error = round(mod.res.pm2.tidy$std.error,3)

mod.res.n.tidy$estimate = round(mod.res.n.tidy$estimate,3)
mod.res.n.tidy$std.error = round(mod.res.n.tidy$std.error,3)

mod.res.vis.tidy$estimate = round(mod.res.vis.tidy$estimate,3)
mod.res.vis.tidy$std.error = round(mod.res.vis.tidy$std.error,3)

## Add Stars

# PM2
mod.res.pm2.tidy$estimate = ifelse(mod.res.pm2.tidy$p.value<0.01,paste0(mod.res.pm2.tidy$estimate,'***') ,
                                   ifelse(mod.res.pm2.tidy$p.value>0.01 & mod.res.pm2.tidy$p.value<0.05, paste0(mod.res.pm2.tidy$estimate,'**') , 
                                          ifelse( mod.res.pm2.tidy$p.value>0.05 & mod.res.pm2.tidy$p.value<0.1, paste0(mod.res.pm2.tidy$estimate,'*'),
                                                  mod.res.pm2.tidy$estimate)))  

# N
mod.res.n.tidy$estimate = ifelse(mod.res.n.tidy$p.value<0.01,paste0(mod.res.n.tidy$estimate,'***') ,
                                 ifelse(mod.res.n.tidy$p.value>0.01 & mod.res.n.tidy$p.value<0.05, paste0(mod.res.n.tidy$estimate,'**') , 
                                        ifelse( mod.res.n.tidy$p.value>0.05 & mod.res.n.tidy$p.value<0.1, paste0(mod.res.n.tidy$estimate,'*'),
                                                mod.res.n.tidy$estimate)))  

# VISITAS
mod.res.vis.tidy$estimate = ifelse(mod.res.vis.tidy$p.value<0.01,paste0(mod.res.vis.tidy$estimate,'***') ,
                                   ifelse(mod.res.vis.tidy$p.value>0.01 & mod.res.vis.tidy$p.value<0.05, paste0(mod.res.vis.tidy$estimate,'**') , 
                                          ifelse( mod.res.vis.tidy$p.value>0.05 & mod.res.vis.tidy$p.value<0.1, paste0(mod.res.vis.tidy$estimate,'*'),
                                                  mod.res.vis.tidy$estimate)))




#### Do table output

reg_out_alq <- data.frame(variables = c('Within 400m of Treated Station','','Month FE','Station FE','N'),
                       main = c(mod.com.pm2.tidy$estimate[[1]],paste0('(',mod.com.pm2.tidy$std.error[1],')'),'Yes','Yes',length(mod.com.pm2$residuals)),
                       main200 = c(mod.com.n.tidy$estimate[[1]],paste0('(',mod.com.n.tidy$std.error[1],')'), 'Yes','Yes',length(mod.com.n$residuals)),
                       robout = c(mod.com.vis.tidy$estimate[[1]],paste0('(',mod.com.vis.tidy$std.error[1],')')  ,'Yes','Yes',length(mod.com.vis$residuals)),
                       main = c(mod.res.pm2.tidy$estimate[[1]],paste0('(',mod.res.pm2.tidy$std.error[1],')'),'Yes','Yes',length(mod.res.pm2$residuals)),
                       main200 = c(mod.res.n.tidy$estimate[[1]],paste0('(',mod.res.n.tidy$std.error[1],')'), 'Yes','Yes',length(mod.res.n$residuals)),
                       robout = c(mod.res.vis.tidy$estimate[[1]],paste0('(',mod.res.vis.tidy$std.error[1],')')  ,'Yes','Yes',length(mod.res.vis$residuals))
)                      

names(reg_out_alq) <- c('','(1)','(2)','(3)','(4)','(5)','(6)')                                                                   
## REPLICATION
sink(file = "Output/regalq.tex")

tab_reg_alq <- kbl(reg_out_alq, booktabs = T,format = 'latex',row.names = FALSE, linesep = '',escape=F , align =c('l', rep('c,6')))  %>%
  add_header_above(c(" " = 1,"Commercial Properties" = 3,"Residential Properties" = 3)) %>%
  add_header_above(c(" " = 1,'P M2' =1,'N Ads'=1,'Visits'=1,'P M2'=1,'N Ads'=1,'Visits'=1 )) %>%
  kable_styling(latex_options = c("repeat_header","scale_down")) %>%
  row_spec(c(2),hline_after = T)%>%
  #row_spec(c(30,34),hline_after = T) %>%
  kable_styling(latex_options = "hold_position") %>% 
  column_spec (1,border_left = F, border_right = T) 

substr(tab_reg_alq,103,nchar(tab_reg_alq)-13)


sink(file = NULL)



########################################################################
##                             MELI - SALE                            ##
########################################################################

rm(list=ls())

library(dplyr)
library(broom)
library(ggplot2)
library(forcats)
library(sf)
library(mapview)
library(lubridate)
library(data.table)
library(lmtest)
library(kableExtra)

setwd('G:/My Drive/Transport Mode Choice/Argentina')

data.sale = st_read('Data Argentina/MELI/Clean/cleanvta.gpkg')
subway = st_read('Data Argentina/Geo/Subte/estaciones-de-subte.shp')

# Determine the central longitude of your data
central_longitude <- (st_bbox(data.sale)[1] + st_bbox(data.sale)[3]) / 2

# Determine the UTM zone based on the central longitude
utm_zone <- floor((central_longitude + 180) / 6) + 1

# Define the appropriate UTM CRS
utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m")

# CHANGE EPSPS
data.sale = st_transform(data.sale, utm_crs)
subway = st_transform(subway, utm_crs)

# data.sale = data.sale %>% filter(ITE_ADD_STATE_NAME == 'Capital Federal')
## Calculate distances

matdists = st_distance(data.sale,subway)
# a = data.frame(matdists)

# Drop geometry
data.sale = st_drop_geometry(data.sale)

# Now we want to create a binary matrix to get discrete measures
matdists_400 = matrix(as.numeric(as.numeric(matdists) < 400),nrow=nrow(matdists),ncol = ncol(matdists)) 

# Get an index of how the columns will be named after cbinding
subway$estacion_id = paste0(subway$ESTACION , ' - ',subway$LINEA)

idx_vars= paste0(subway$estacion_id,' - 400')

# Merge the within 400 meters
gc()
data.sale = cbind(data.sale,matdists_400)
names(data.sale)[(ncol(data.sale)-ncol(matdists_400)+1):ncol(data.sale)] = idx_vars


# Create additional variables
data.sale$pm2 = data.sale$price/(data.sale$SConstrM2 + (data.sale$STotalM2 - data.sale$SConstrM2)/2)

data.sale$visitas = data.sale$VISITASANDROID + data.sale$VISITASIOS + data.sale$VISITASMOBILE + data.sale$VISITASSTD 

# Wide to long

data.sale.l = melt(setDT(data.sale), id.vars = c('MesListing','pm2','visitas','TIPOPROPIEDADNORM'), measure.vars = which(names(data.sale) %in% idx_vars), variable.name = "Station")

data.sale.l = as.data.frame(data.sale.l)

# Read data with index of when each station was open
tdates = readRDS('Data Argentina/Temp/treat_dates.RDS')
tdates$f = ifelse(is.na(tdates$f),'2025-01',tdates$f)

tdates$f_d = as.Date(paste0(tdates$f,'-01'))


outlist = tdates$estacion_id[tdates$f_d< as.Date('2017-02-01') | tdates$f_d> as.Date('2019-12-01')]

# Keep only commercial buildings
data_com = data.sale.l %>% filter(TIPOPROPIEDADNORM %in% c('Local','Oficina'))


# Stats

data.comf = data_com %>% filter(value==1)

stats.com = data.comf %>%
  group_by(MesListing,Station) %>%
  summarise(
    pm2 = median(pm2,na.rm=T),
    visitas = median(visitas,na.rm=T) ,
    ene = n()
  )

stats.com$Station = as.character(stats.com$Station)

stats.com$linea = substr(stats.com$Station,nchar(stats.com$Station)-6 ,nchar(stats.com$Station)-5)

stats.com$pm2 <- ifelse(is.infinite(stats.com$pm2),NA,stats.com$pm2)
stats.com$visitas = ifelse(is.na(stats.com$visitas),0,stats.com$visitas) 

stats.com$estacion_id = substr(as.character(stats.com$Station), 1, nchar(as.character(stats.com$Station))-6)


stats.com = stats.com %>% left_join(tdates)
stats.com$MesListing = as.Date(stats.com$MesListing)


stats.com$t = ifelse(stats.com$f_d >stats.com$MesListing , 0 , 1)

# Models
mod.com.pm2 = lm(pm2~t + as.factor(estacion_id) + as.factor(MesListing) , stats.com[!(stats.com$estacion_id %in% outlist), ])

mod.com.n = lm(ene~t + as.factor(estacion_id) + as.factor(MesListing) , stats.com[!(stats.com$estacion_id %in% outlist), ])

mod.com.vis = lm(visitas~t + as.factor(estacion_id) + as.factor(MesListing) , stats.com[!(stats.com$estacion_id %in% outlist), ])

##  Cluster SE
# 1
mod.com.pm2.cl = coeftest(mod.com.pm2,
                          vcov = vcovCL,
                          cluster = ~linea)

mod.com.pm2.tidy = tidy(mod.com.pm2.cl) %>% 
  filter(term %in% c('t'))

# 2
mod.com.n.cl = coeftest(mod.com.n,
                        vcov = vcovCL,
                        cluster = ~linea)

mod.com.n.tidy = tidy(mod.com.n.cl) %>% 
  filter(term %in% c('t'))

# 3
mod.com.vis.cl = coeftest(mod.com.vis,
                          vcov = vcovCL,
                          cluster = ~linea)

mod.com.vis.tidy = tidy(mod.com.vis.cl) %>% 
  filter(term %in% c('t'))



#### Repeat similar thing but for non-commercial properties

# Keep only commercial buildings - AVOID HOUSES BC BIAS AND ARE THE LEAST
data_res = data.sale.l %>% filter(TIPOPROPIEDADNORM %in% c('Departamento'))

data.resf = data_res %>% filter(value==1)

stats.res = data.resf %>%
  group_by(MesListing,Station) %>%
  summarise(
    pm2 = median(pm2,na.rm=T),
    visitas = median(visitas,na.rm=T) ,
    ene = n()
  )

stats.res$Station = as.character(stats.res$Station)

stats.res$linea = substr(stats.res$Station,nchar(stats.res$Station)-6 ,nchar(stats.res$Station)-5)

stats.res$pm2 <- ifelse(is.infinite(stats.res$pm2),NA,stats.res$pm2)
stats.res$visitas = ifelse(is.na(stats.res$visitas),0,stats.res$visitas) 

stats.res$estacion_id = substr(as.character(stats.res$Station), 1, nchar(as.character(stats.res$Station))-6)

# Read data with index of when each station was open
stats.res = stats.res %>% left_join(tdates)
stats.res$MesListing = as.Date(stats.res$MesListing)


stats.res$t = ifelse(stats.res$f_d >stats.res$MesListing , 0 , 1)

# Models
mod.res.pm2 = lm(pm2~t + as.factor(estacion_id) + as.factor(MesListing) , stats.res[!(stats.res$estacion_id %in% outlist), ])

mod.res.n = lm(ene~t + as.factor(estacion_id) + as.factor(MesListing) , stats.res[!(stats.res$estacion_id %in% outlist), ])

mod.res.vis = lm(visitas~t + as.factor(estacion_id) + as.factor(MesListing) , stats.res[!(stats.res$estacion_id %in% outlist), ])


##  Cluster SE
# 1
mod.res.pm2.cl = coeftest(mod.res.pm2,
                          vcov = vcovCL,
                          cluster = ~linea)

mod.res.pm2.tidy = tidy(mod.res.pm2.cl) %>% 
  filter(term %in% c('t'))

# 2
mod.res.n.cl = coeftest(mod.res.n,
                        vcov = vcovCL,
                        cluster = ~linea)

mod.res.n.tidy = tidy(mod.res.n.cl) %>% 
  filter(term %in% c('t'))

# 3
mod.res.vis.cl = coeftest(mod.res.vis,
                          vcov = vcovCL,
                          cluster = ~linea)

mod.res.vis.tidy = tidy(mod.res.vis.cl) %>% 
  filter(term %in% c('t'))

###### DO TABLE

### COMMERCIAL PREPARE

mod.com.pm2.tidy$estimate = round(mod.com.pm2.tidy$estimate,3)
mod.com.pm2.tidy$std.error = round(mod.com.pm2.tidy$std.error,3)

mod.com.n.tidy$estimate = round(mod.com.n.tidy$estimate,3)
mod.com.n.tidy$std.error = round(mod.com.n.tidy$std.error,3)

mod.com.vis.tidy$estimate = round(mod.com.vis.tidy$estimate,3)
mod.com.vis.tidy$std.error = round(mod.com.vis.tidy$std.error,3)

## Add Stars

# PM2
mod.com.pm2.tidy$estimate = ifelse(mod.com.pm2.tidy$p.value<0.01,paste0(mod.com.pm2.tidy$estimate,'***') ,
                                   ifelse(mod.com.pm2.tidy$p.value>0.01 & mod.com.pm2.tidy$p.value<0.05, paste0(mod.com.pm2.tidy$estimate,'**') , 
                                          ifelse( mod.com.pm2.tidy$p.value>0.05 & mod.com.pm2.tidy$p.value<0.1, paste0(mod.com.pm2.tidy$estimate,'*'),
                                                  mod.com.pm2.tidy$estimate)))  

# N
mod.com.n.tidy$estimate = ifelse(mod.com.n.tidy$p.value<0.01,paste0(mod.com.n.tidy$estimate,'***') ,
                                 ifelse(mod.com.n.tidy$p.value>0.01 & mod.com.n.tidy$p.value<0.05, paste0(mod.com.n.tidy$estimate,'**') , 
                                        ifelse( mod.com.n.tidy$p.value>0.05 & mod.com.n.tidy$p.value<0.1, paste0(mod.com.n.tidy$estimate,'*'),
                                                mod.com.n.tidy$estimate)))  

# VISITAS
mod.com.vis.tidy$estimate = ifelse(mod.com.vis.tidy$p.value<0.01,paste0(mod.com.vis.tidy$estimate,'***') ,
                                   ifelse(mod.com.vis.tidy$p.value>0.01 & mod.com.vis.tidy$p.value<0.05, paste0(mod.com.vis.tidy$estimate,'**') , 
                                          ifelse( mod.com.vis.tidy$p.value>0.05 & mod.com.vis.tidy$p.value<0.1, paste0(mod.com.vis.tidy$estimate,'*'),
                                                  mod.com.vis.tidy$estimate)))  

### RESIDENTIAL

mod.res.pm2.tidy$estimate = round(mod.res.pm2.tidy$estimate,3)
mod.res.pm2.tidy$std.error = round(mod.res.pm2.tidy$std.error,3)

mod.res.n.tidy$estimate = round(mod.res.n.tidy$estimate,3)
mod.res.n.tidy$std.error = round(mod.res.n.tidy$std.error,3)

mod.res.vis.tidy$estimate = round(mod.res.vis.tidy$estimate,3)
mod.res.vis.tidy$std.error = round(mod.res.vis.tidy$std.error,3)

## Add Stars

# PM2
mod.res.pm2.tidy$estimate = ifelse(mod.res.pm2.tidy$p.value<0.01,paste0(mod.res.pm2.tidy$estimate,'***') ,
                                   ifelse(mod.res.pm2.tidy$p.value>0.01 & mod.res.pm2.tidy$p.value<0.05, paste0(mod.res.pm2.tidy$estimate,'**') , 
                                          ifelse( mod.res.pm2.tidy$p.value>0.05 & mod.res.pm2.tidy$p.value<0.1, paste0(mod.res.pm2.tidy$estimate,'*'),
                                                  mod.res.pm2.tidy$estimate)))  

# N
mod.res.n.tidy$estimate = ifelse(mod.res.n.tidy$p.value<0.01,paste0(mod.res.n.tidy$estimate,'***') ,
                                 ifelse(mod.res.n.tidy$p.value>0.01 & mod.res.n.tidy$p.value<0.05, paste0(mod.res.n.tidy$estimate,'**') , 
                                        ifelse( mod.res.n.tidy$p.value>0.05 & mod.res.n.tidy$p.value<0.1, paste0(mod.res.n.tidy$estimate,'*'),
                                                mod.res.n.tidy$estimate)))  

# VISITAS
mod.res.vis.tidy$estimate = ifelse(mod.res.vis.tidy$p.value<0.01,paste0(mod.res.vis.tidy$estimate,'***') ,
                                   ifelse(mod.res.vis.tidy$p.value>0.01 & mod.res.vis.tidy$p.value<0.05, paste0(mod.res.vis.tidy$estimate,'**') , 
                                          ifelse( mod.res.vis.tidy$p.value>0.05 & mod.res.vis.tidy$p.value<0.1, paste0(mod.res.vis.tidy$estimate,'*'),
                                                  mod.res.vis.tidy$estimate)))




#### Do table output

reg_out_sale <- data.frame(variables = c('Within 400m of Treated Station','','Month FE','Station FE','N'),
                          main = c(mod.com.pm2.tidy$estimate[[1]],paste0('(',mod.com.pm2.tidy$std.error[1],')'),'Yes','Yes',length(mod.com.pm2$residuals)),
                          main200 = c(mod.com.n.tidy$estimate[[1]],paste0('(',mod.com.n.tidy$std.error[1],')'), 'Yes','Yes',length(mod.com.n$residuals)),
                          robout = c(mod.com.vis.tidy$estimate[[1]],paste0('(',mod.com.vis.tidy$std.error[1],')')  ,'Yes','Yes',length(mod.com.vis$residuals)),
                          main = c(mod.res.pm2.tidy$estimate[[1]],paste0('(',mod.res.pm2.tidy$std.error[1],')'),'Yes','Yes',length(mod.res.pm2$residuals)),
                          main200 = c(mod.res.n.tidy$estimate[[1]],paste0('(',mod.res.n.tidy$std.error[1],')'), 'Yes','Yes',length(mod.res.n$residuals)),
                          robout = c(mod.res.vis.tidy$estimate[[1]],paste0('(',mod.res.vis.tidy$std.error[1],')')  ,'Yes','Yes',length(mod.res.vis$residuals))
)                      

names(reg_out_sale) <- c('','(1)','(2)','(3)','(4)','(5)','(6)')                                     
## REPLICATION
sink(file = "Output/regsale.tex")

tab_reg_alq <- kbl(reg_out_sale, booktabs = T,format = 'latex',row.names = FALSE, linesep = '',escape=F , align =c('l', rep('c,6')))  %>%
  add_header_above(c(" " = 1,'P M2' =1,'N Ads'=1,'Visits'=1,'P M2'=1,'N Ads'=1,'Visits'=1 )) %>% 
  add_header_above(c(" " = 1,"Commercial Properties" = 3,"Residential Properties" = 3)) %>% 
  kable_styling(latex_options = c("repeat_header","scale_down")) %>%
  row_spec(c(2),hline_after = T)%>%
  #row_spec(c(30,34),hline_after = T) %>%
  kable_styling(latex_options = "hold_position") %>% 
  column_spec (1,border_left = F, border_right = T) 

substr(tab_reg_alq,103,nchar(tab_reg_alq)-13)


sink(file = NULL)


