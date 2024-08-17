


#################################################################
##                          GROUP ATT                          ##
#################################################################


rm(list=ls())

library(sf)
library(dplyr)
library(data.table)
library(stringr)
library(stargazer)
library(lubridate)
library(broom)
library(ggplot2)
library(forcats)
library(did)

setwd('G:/My Drive/Transport Mode Choice/Argentina')

### 

data_d = readRDS('Data Argentina/Final/final_daily.rds')
data = readRDS('Data Argentina/Final/final_monthly.rds')

### This to get the ones that are treated since day 1
data_t = data_d[data_d$bin_0_400==1,]
data_t$fecha = as.Date(data_t$fecha)

origstat =  data$estacion_id[data$month=='2014-01' &data$bin_0_400>0]
nevert =  data$estacion_id[data$month == '2019-01' &data$bin_0_400==0]

### I want to define the stations in groups
mindate = data_t %>%
  group_by(estacion_id) %>%
  slice(which.min(fecha))

mindate = mindate %>% filter(!(estacion_id%in%origstat))

mindate$year = substr(mindate$fecha,1,4)
table(mindate$year)

# Define Groups: early= 2015-2016, medium = 2017-2018, late = 2019

mindate$group = ifelse(mindate$year %in% c('2015','2016'),'1',
                       ifelse(mindate$year %in% c('2017','2018'),'2','3'))

temp = mindate[,c('estacion_id','group','fecha')]
saveRDS(temp,'Data Argentina/Temp/treat_dates.rds')

# data = data %>% left_join(mindate[,c('estacion_id','group')])
# data$group = ifelse(data$estacion_id %in% nevert , '0',data$group)
# 
# # Remove those always treated and clean data as the command requires
# data_cl = data %>% filter(!(estacion_id %in% origstat))
# 
# # TIME HAS TO BE NUMERIC
# time_m = data.frame(month = as.character(sort(unique(data_cl$month))) , tidx = 1:length(sort(unique(data_cl$month))))
# data_cl = data_cl %>% left_join(time_m)
# 
# # GROUP HAS TO BE NUMERIC
# grp_m = data.frame(estacion_id = as.character(sort(unique(data_cl$estacion_id))) , iidx = 1:length(sort(unique(data_cl$estacion_id))))
# data_cl = data_cl %>% left_join(grp_m)
# 
# # Other vars have to be numeric
# data_cl$group = as.numeric(data_cl$group)
# data_cl$treat = data_cl$bin_0_400
# 
# 
# 
# att_gt(yname = 'log_pases', tname = 'tidx' , idname = 'iidx' , gname = 'group',data = data_cl[,c('log_pases','tidx','iidx','group','treat')] , allow_unbalanced_panel = T , control_group = c( "notyettreated"),)



##################### try descriptive stats of the waves. Think of a) Distance to the city center and B) average rides per day during 2014

# Read subway 

subte <- st_read('Data Argentina/Geo/Subte/estaciones-de-subte.shp')

# What I do now is to set all the epgs in meters

# Determine the central longitude of your data
central_longitude <- (st_bbox(subte)[1] + st_bbox(subte)[3]) / 2

# Determine the UTM zone based on the central longitude
utm_zone <- floor((central_longitude + 180) / 6) + 1

# Define the appropriate UTM CRS
utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m")

obelisco = st_sfc(st_point(c(-58.382058,-34.603846)),crs=4326)

dists = as.matrix(st_distance(subte,obelisco))

subte$estacion_id = paste0(subte$ESTACION ,' - ',subte$LINEA )

subte = cbind(subte,dists)

subte = subte %>% left_join(mindate,by = 'estacion_id')

tabdists = subte %>% 
  group_by(group) %>%
summarise(
  mdist = mean(dists),
  sddist = sd(dists)
)
tabdists = st_drop_geometry(tabdists)

#### Now we want to see the 'pases' in 2014

data_d = data_d  %>% left_join(mindate[c('estacion_id','group')],by = 'estacion_id')

data_14 = data_d[substr(data_d$fecha,1,4)== 2014 , ]

tabpases = data_14 %>% 
  group_by(group) %>%
  summarise(
    mpases = mean(pases,na.rm=T),
    sdpases = sd(pases,na.rm=T)
  )

pval_pases = aov(pases~group,data = data_14)
pval_dists = aov(dists~group,data = subte)

outab = matrix(ncol=7,nrow=2)



outab[1,] = c(round(as.numeric(c(tabdists[1,2],tabdists[1,3],tabdists[2,2],tabdists[2,3],tabdists[3,2],tabdists[3,3])),2),'<0.001')

outab[2,] = c(round(as.numeric(c(tabpases[1,2],tabpases[1,3],tabpases[2,2],tabpases[2,3],tabpases[3,2],tabpases[3,3])),2),'<0.001')

outab = data.frame(outab)


names(outab) = c('Mean','SD','Mean','SD','Mean','SD','p-value')
rownames(outab) = c('Dist to Center','Taps (2014)')


# print table

sink(file = "Output/grouphet.tex")

tab_reg2 <- kbl(outab, booktabs = T,format = 'latex',row.names = T, linesep = '',escape=F , align =c('l', rep('c,6')))  %>%
  kable_styling(latex_options = c("repeat_header","scale_down")) %>%
  add_header_above(c(" " = 1,'Group 1' =2,'Group 2' =2,'Group 3' =2,' '=1))%>%
  # row_spec(c(8),hline_after = T)%>%
  #row_spec(c(30,34),hline_after = T) %>%
  kable_styling(latex_options = "hold_position") %>% 
  column_spec (1,border_left = F, border_right = T) 

substr(tab_reg2,103,nchar(tab_reg2)-13)


sink(file = NULL)

