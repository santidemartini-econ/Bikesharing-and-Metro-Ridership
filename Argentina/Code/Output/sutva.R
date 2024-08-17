

#################################################################
##                       SUTVA VIOLATION                       ##
#################################################################


rm(list=ls())

library(dplyr)
library(ggplot2)
library(forcats)
library(sf)
library(lmtest)
library(kableExtra)

setwd('G:/My Drive/Transport Mode Choice/Argentina')

source("Code/remove_accents.R")

# Follow Muralidharan et. al (2022) and Egger et. al (2022), for example

#### Subways - Read Data
subway.t = readRDS('Data Argentina/Subway/grp_sbwy_daily.rds')
subway.t$estacion_id = ifelse(subway.t$estacion_id == 'AGÏ¿½ERO - D','AGUERO - D',subway.t$estacion_id)
subway.t = subway.t %>% filter(pases>0 & !is.na(fecha))

subway.o = st_read('Data Argentina/Geo/Subte/estaciones-de-subte.shp')
subway.o$ESTACION =   remove.accents(toupper(subway.o$ESTACION))
subway.o$estacion_id = paste0(subway.o$ESTACION,' - ',subway.o$LINEA)

##### Exercise to determine which are the closest stations within line

# Determine the central longitude of your data
central_longitude <- (st_bbox(subway.o)[1] + st_bbox(subway.o)[3]) / 2

# Determine the UTM zone based on the central longitude
utm_zone <- floor((central_longitude + 180) / 6) + 1

# Define the appropriate UTM CRS
utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m")

# CHANGE EPSPS
subte = st_transform(subway.o, utm_crs)

# There are 6 subway lines: A, B, C, D, E, H
subte.a = subte %>% filter(LINEA=='A')
subte.b = subte %>% filter(LINEA=='B')
subte.c = subte %>% filter(LINEA=='C')
subte.d = subte %>% filter(LINEA=='D')
subte.e = subte %>% filter(LINEA=='E')
subte.h = subte %>% filter(LINEA=='H')

# Create matrix of distances between stations of the same line
dists.a = as.matrix(st_distance(subte.a,subte.a))
dists.b = as.matrix(st_distance(subte.b,subte.b))
dists.c = as.matrix(st_distance(subte.c,subte.c))
dists.d = as.matrix(st_distance(subte.d,subte.d))
dists.e = as.matrix(st_distance(subte.e,subte.e))
dists.h = as.matrix(st_distance(subte.h,subte.h))


# # I want to get the closest 2 stations for each station EXCEPT for the extreme ones, for which I only consider the 1 closest one
# edge.a = which(subte.a$ESTACION %in% c('PLAZA DE MAYO','SAN PEDRITO'))
# edge.b = which(subte.b$ESTACION %in%c('LEANDRO N. ALEM', 'JUAN MANUEL DE ROSAS - VILLA URQUIZA'))
# edge.c = which(subte.c$ESTACION %in% c('RETIRO','CONSTITUCION'))
# edge.d = which(subte.d$ESTACION %in%c('CONGRESO DE TUCUMAN','CATEDRAL'))
# edge.e = which(subte.e$ESTACION %in%c('PLAZA DE LOS VIRREYES - EVA PERON','RETIRO'))
# edge.h = which(subte.h$ESTACION %in%c('FACULTAD DE DERECHO - JULIETA LANTERI','HOSPITALES'))


list.mats = list(dists.a,dists.b,dists.c,dists.d,dists.e,dists.h)


list.out = list()

for(i in 1:length(list.mats)){
  df = list.mats[i]
  smallest_two <- data.frame(lapply(data.frame(df), function(col) c(which(col %in% sort(col)[2]),which(col %in% sort(col)[3]),which(col %in% sort(col)[4]),which(col %in% sort(col)[5]),which(col %in% sort(col)[6]) ) ))
  
  list.out[[i]] = smallest_two
}

closest.a = list.out[[1]]
closest.b = list.out[[2]]
closest.c = list.out[[3]]
closest.d = list.out[[4]]
closest.e = list.out[[5]]
closest.h = list.out[[6]]

# # For the edges placed NA in the second closesst
# closest.a[4,edge.a] = NA
# closest.b[4,edge.b] = NA
# closest.c[4,edge.c] = NA
# closest.d[4,edge.d] = NA
# closest.e[4,edge.e] = NA
# closest.h[4,edge.h] = NA

#### ADD COLUMN OF NEIGHBORS FOR EACH SUBWAY LINE
subte.a$nbr1 = subte.a$estacion_id[as.numeric(closest.a[1,])]
subte.a$nbr2 = subte.a$estacion_id[as.numeric(closest.a[2,])]
subte.a$nbr3 = subte.a$estacion_id[as.numeric(closest.a[3,])]
subte.a$nbr4 = subte.a$estacion_id[as.numeric(closest.a[4,])]
subte.a$nbr5 = subte.a$estacion_id[as.numeric(closest.a[5,])]

subte.b$nbr1 = subte.b$estacion_id[as.numeric(closest.b[1,])]
subte.b$nbr2 = subte.b$estacion_id[as.numeric(closest.b[2,])]
subte.b$nbr3 = subte.b$estacion_id[as.numeric(closest.b[3,])]
subte.b$nbr4 = subte.b$estacion_id[as.numeric(closest.b[4,])]
subte.b$nbr5 = subte.b$estacion_id[as.numeric(closest.b[5,])]

subte.c$nbr1 = subte.c$estacion_id[as.numeric(closest.c[1,])]
subte.c$nbr2 = subte.c$estacion_id[as.numeric(closest.c[2,])]
subte.c$nbr3 = subte.c$estacion_id[as.numeric(closest.c[3,])]
subte.c$nbr4 = subte.c$estacion_id[as.numeric(closest.c[4,])]
subte.c$nbr5 = subte.c$estacion_id[as.numeric(closest.c[5,])]

subte.d$nbr1 = subte.d$estacion_id[as.numeric(closest.d[1,])]
subte.d$nbr2 = subte.d$estacion_id[as.numeric(closest.d[2,])]
subte.d$nbr3 = subte.d$estacion_id[as.numeric(closest.d[3,])]
subte.d$nbr4 = subte.d$estacion_id[as.numeric(closest.d[4,])]
subte.d$nbr5 = subte.d$estacion_id[as.numeric(closest.d[5,])]

subte.e$nbr1 = subte.e$estacion_id[as.numeric(closest.e[1,])]
subte.e$nbr2 = subte.e$estacion_id[as.numeric(closest.e[2,])]
subte.e$nbr3 = subte.e$estacion_id[as.numeric(closest.e[3,])]
subte.e$nbr4 = subte.e$estacion_id[as.numeric(closest.e[4,])]
subte.e$nbr5 = subte.e$estacion_id[as.numeric(closest.e[5,])]

subte.h$nbr1 = subte.h$estacion_id[as.numeric(closest.h[1,])]
subte.h$nbr2 = subte.h$estacion_id[as.numeric(closest.h[2,])]
subte.h$nbr3 = subte.h$estacion_id[as.numeric(closest.h[3,])]
subte.h$nbr4 = subte.h$estacion_id[as.numeric(closest.h[4,])]
subte.h$nbr5 = subte.h$estacion_id[as.numeric(closest.h[5,])]

subte = rbind(subte.a,subte.b,subte.c,subte.d,subte.e,subte.h)


# Read daily data
data = readRDS('Data Argentina/Final/final_daily.rds')
data_m= readRDS('Data Argentina/Final/final_monthly.rds')

# As we did in regs_v2, get which is the first treated day of each station

data = data %>%
  group_by(estacion_id) %>%
  arrange(fecha) %>%
  mutate(
    csbin_0_400 = cumsum(bin_0_400)
  )

data = data %>%
  group_by(estacion_id) %>%
  arrange(fecha) %>%
  mutate(
    cs2bin_0_400 = cumsum(csbin_0_400)
  )

idx_treat = data %>%
  filter(cs2bin_0_400==1)

idx_treat = idx_treat[,c('estacion_id','month')]
names(idx_treat)[2] = 'f'

saveRDS(idx_treat , 'Data Argentina/Temp/treat_dates.rds')

# Merge opening dates of each neighbor to 'subte' database
subte = subte %>% left_join(idx_treat , by = c('nbr1' = 'estacion_id'))
subte = subte %>% left_join(idx_treat , by = c('nbr2' = 'estacion_id'))
subte = subte %>% left_join(idx_treat , by = c('nbr3' = 'estacion_id'))
subte = subte %>% left_join(idx_treat , by = c('nbr4' = 'estacion_id'))
subte = subte %>% left_join(idx_treat , by = c('nbr5' = 'estacion_id'))

subte = st_drop_geometry(subte)

names(subte)[(ncol(subte)-4):ncol(subte)] = c('f_nb1','f_nb2','f_nb3','f_nb4','f_nb5')

# There are some stations that are never treated, replace those with a date such that it counts as 0 in what we do next
subte$f_nb1 = ifelse(is.na(subte$f_nb1) , '2024-01',subte$f_nb1)
subte$f_nb2 = ifelse(is.na(subte$f_nb2) , '2024-01',subte$f_nb2)
subte$f_nb3 = ifelse(is.na(subte$f_nb3) , '2024-01',subte$f_nb3)
subte$f_nb4 = ifelse(is.na(subte$f_nb4) , '2024-01',subte$f_nb4)
subte$f_nb5 = ifelse(is.na(subte$f_nb5) , '2024-01',subte$f_nb5)

# # Same as before but for edge stations we want to just consider the first three neighbors
# edges = c('PLAZA DE MAYO - A','SAN PEDRITO - A','LEANDRO N. ALEM - B', 'JUAN MANUEL DE ROSAS - VILLA URQUIZA - B','RETIRO - C','CONSTITUCION - C' ,'CONGRESO DE TUCUMAN - D','CATEDRAL - D','PLAZA DE LOS VIRREYES - EVA PERON - E','RETIRO - E','FACULTAD DE DERECHO - JULIETA LANTERI - H','HOSPITALES - H')
# 
# 
# subte$f_nb4 = ifelse(is.na(subte$f_nb4) & !(subte$estacion_id %in% edges), '2024-01',subte$f_nb4)


## Let's start with 3 neighbors


# subte$denom = rowSums(!is.na(subte[,c('f_nb1','f_nb2','f_nb3','f_nb4')] ))

# Now that we know the denominator replace the date of the last neighbors of the edges to something that will always be zero
# subte$f_nb4 = ifelse(is.na(subte$f_nb4), '2024-01',subte$f_nb4)

# Replace to daily format
subte$f_nb1 = as.Date(paste0(subte$f_nb1,'-01')) 
subte$f_nb2 = as.Date(paste0(subte$f_nb2,'-01'))  
subte$f_nb3 = as.Date(paste0(subte$f_nb3,'-01'))  
subte$f_nb4 = as.Date(paste0(subte$f_nb4,'-01'))  
subte$f_nb5 = as.Date(paste0(subte$f_nb5,'-01'))  

##  Merge to daily data
# data = data %>% left_join(subte[,c('estacion_id','f_nb1','f_nb2','f_nb3','f_nb4','denom')])

data = data %>% left_join(subte[,c('estacion_id','f_nb1','f_nb2','f_nb3','f_nb4','f_nb5')])

data_m = data_m %>% left_join(subte[,c('estacion_id','f_nb1','f_nb2','f_nb3','f_nb4','f_nb5')])

data$t_nb1 = ifelse(data$fecha>data$f_nb1 ,1,0)
data$t_nb2 = ifelse(data$fecha>data$f_nb2 ,1,0)
data$t_nb3 = ifelse(data$fecha>data$f_nb3 ,1,0)
data$t_nb4 = ifelse(data$fecha>data$f_nb4 ,1,0)
data$t_nb5 = ifelse(data$fecha>data$f_nb4 ,1,0)

# Get the shares
data$t_n2 = (data$t_nb1 + data$t_nb2  )/2
data$t_n3 = (data$t_nb1 + data$t_nb2 + data$t_nb3 )/3
data$t_n4 = (data$t_nb1 + data$t_nb2 + data$t_nb3 + data$t_nb4)/4
data$t_n5 = (data$t_nb1 + data$t_nb2 + data$t_nb3 + data$t_nb4 +  data$t_nb5)/5

# For monthly

data_m$t_nb1 = ifelse(data_m$fecha>data_m$f_nb1 ,1,0)
data_m$t_nb2 = ifelse(data_m$fecha>data_m$f_nb2 ,1,0)
data_m$t_nb3 = ifelse(data_m$fecha>data_m$f_nb3 ,1,0)
data_m$t_nb4 = ifelse(data_m$fecha>data_m$f_nb4 ,1,0)
data_m$t_nb5 = ifelse(data_m$fecha>data_m$f_nb4 ,1,0)

# Get the shares
data_m$t_n2 = (data_m$t_nb1 + data_m$t_nb2 )/2
data_m$t_n3 = (data_m$t_nb1 + data_m$t_nb2 + data_m$t_nb3 )/3
data_m$t_n4 = (data_m$t_nb1 + data_m$t_nb2 + data_m$t_nb3 + data_m$t_nb4)/4
data_m$t_n5 = (data_m$t_nb1 + data_m$t_nb2 + data_m$t_nb3 + data_m$t_nb4 +  data_m$t_nb5)/5

# Regression
# origstat =  c(data$estacion_id[data$fecha == min(data$fecha) &data$bin_0_400>0],data$estacion_id[data$fecha == max(data$fecha) &data$bin_0_400==0]) # 37 STATIONS REMOVED
origstat =  c(data$estacion_id[data$fecha == min(data$fecha) &data$bin_0_400>0]) # 37

# Main
# REMOVE THE CONTROL VARIABLES TO ATTENUATE THE COEFFICIENT

regspil2 = lm(log_pases ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(fecha) + t_n2 + age + male + yrschool + share_work , data = data[!(data$estacion_id%in% origstat),])


regspil2_clustered = coeftest(regspil2,
                             vcov = vcovCL,
                             cluster = ~estacion_id)

regspil2_tidy = tidy(regspil2_clustered) %>% 
  filter(term %in% c('bin_0_400', 't_n2'))

saveRDS(regspil2_clustered , 'Data Argentina/Regs/regspil2_clustered.rds')

# p-value = 0.0580

# Consider 3 neighbors
regspil3 = lm(log_pases ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(fecha) + t_n3  + age + male + yrschool + share_work, data = data[!(data$estacion_id%in% origstat),])


regspil3_clustered2 = coeftest(regspil3,
                             vcov = vcovCL,
                             cluster = ~estacion_id)

regspil3_tidy = tidy(regspil3_clustered2) %>% 
  filter(term %in% c('bin_0_400','t_n3'))

saveRDS(regspil3_clustered2 , 'Data Argentina/Regs/regspil3_clustered2.rds')

# p-value = 0.0890

# Consider 4 neighbors
regspil4 = lm(log_pases ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(fecha) + t_n4+ age + male + yrschool + share_work, data = data[!(data$estacion_id%in% origstat),])


regspil4_clustered2 = coeftest(regspil4,
                               vcov = vcovCL,
                               cluster = ~estacion_id)

regspil4_tidy = tidy(regspil4_clustered2) %>% 
  filter(term %in% c('bin_0_400','t_n4'))

saveRDS(regspil4_clustered2 , 'Data Argentina/Regs/regspil4_clustered2.rds')

# p-value = 0.101

# Consider 5 neighbors
regspil5 = lm(log_pases ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(fecha) + t_n5  + age + male + yrschool + share_work, data = data[!(data$estacion_id%in% origstat),])


regspil5_clustered2 = coeftest(regspil5,
                               vcov = vcovCL,
                               cluster = ~estacion_id)

regspil5_tidy = tidy(regspil5_clustered2) %>% 
  filter(term %in% c('bin_0_400','t_n5'))
# p-value = 0.101

saveRDS(regspil5_clustered2 , 'Data Argentina/Regs/regspil5_clustered2.rds')

### I also want to study the effect at the subway-line level 

data_lin = data %>%
  group_by(linea,fecha) %>%
  summarise(
    pases = sum(pases, na.rm=T),
    n_0_400 = sum(n_0_400 , na.rm=T),
    n_1200_3200 = sum(n_1200_3200,na.rm=T),
    bin_0_400 =sum(bin_0_400,na.rm=T),
    nstations = n()
  )


data_lin$month = substr(data_lin$fecha,1,7)

data_lin$log_pases = log(data_lin$pases)


# Montly freq

data_lin_m = data_m %>%
  group_by(linea,fecha) %>%
  summarise(
    pases = sum(pases_m, na.rm=T),
    n_0_400 = sum(n_0_400 , na.rm=T),
    n_1200_3200 = sum(n_1200_3200,na.rm=T),
    bin_0_400 =sum(bin_0_400,na.rm=T),
    nstations = n()
  )


data_lin_m$month = substr(data_lin_m$fecha,1,7)

data_lin_m$log_pases = log(data_lin_m$pases)

# Reg1
# reglin = lm(log_pases ~ n_0_400 + n_1200_3200  + as.factor(linea) + as.factor(fecha)  , data = data_lin )

# reglin_clustered2 = coeftest(reglin,
#                                vcov = vcovCL,
#                                cluster = ~linea)
# 
# reglin_tidy = tidy(reglin_clustered2) %>% 
#   filter(term %in% c('n_0_400'))

# # Reg2
#data_lin$sharetr = data_lin$bin_0_400 / data_lin$nstations
# 

# I cannot add the spatial controls in this specification because the line passes through many neighborhoods (comunas)
reglin2 = lm(log_pases ~ bin_0_400 + n_1200_3200  + as.factor(linea) + as.factor(fecha)   , data = data_lin )

reglin2_clustered2 = coeftest(reglin2,
                             vcov = vcovCL,
                             cluster = ~linea)

reglin2_tidy = tidy(reglin2_clustered2) %>%
  filter(term %in% c('bin_0_400'))

saveRDS(reglin2_tidy , 'Data Argentina/Regs/reglin2_tidy.rds')
# P-value = 0.0000001
#### OUT REGRESSION

regspil2_tidy$estimate = format(round(regspil2_tidy$estimate,3),nfill=3)
regspil2_tidy$std.error = format(round(regspil2_tidy$std.error,3),nfill=3)

regspil3_tidy$estimate = format(round(regspil3_tidy$estimate,3),nfill=3)
regspil3_tidy$std.error = format(round(regspil3_tidy$std.error,3),nfill=3)

regspil4_tidy$estimate = format(round(regspil4_tidy$estimate,3),nfill=3)
regspil4_tidy$std.error = format(round(regspil4_tidy$std.error,3),nfill=3)

regspil5_tidy$estimate = format(round(regspil5_tidy$estimate,3),nfill=3)
regspil5_tidy$std.error = format(round(regspil5_tidy$std.error,3),nfill=3)

reglin2_tidy$estimate = format(round(reglin2_tidy$estimate,3),nfill=3)
reglin2_tidy$std.error = format(round(reglin2_tidy$std.error,3),nfill=3)


regspil2_tidy$estimate = ifelse(regspil2_tidy$p.value<0.01,paste0(regspil2_tidy$estimate,'***') ,
                               ifelse(regspil2_tidy$p.value>0.01 & regspil2_tidy$p.value<0.05, paste0(regspil2_tidy$estimate,'**') , 
                                      ifelse( regspil2_tidy$p.value>0.05 & regspil2_tidy$p.value<0.1, paste0(regspil2_tidy$estimate,'*'),
                                              regspil2_tidy$estimate)))  


regspil3_tidy$estimate = ifelse(regspil3_tidy$p.value<0.01,paste0(regspil3_tidy$estimate,'***') ,
                                ifelse(regspil3_tidy$p.value>0.01 & regspil3_tidy$p.value<0.05, paste0(regspil3_tidy$estimate,'**') , 
                                       ifelse( regspil3_tidy$p.value>0.05 & regspil3_tidy$p.value<0.1, paste0(regspil3_tidy$estimate,'*'),
                                               regspil3_tidy$estimate)))  

regspil4_tidy$estimate = ifelse(regspil4_tidy$p.value<0.01,paste0(regspil4_tidy$estimate,'***') ,
                                ifelse(regspil4_tidy$p.value>0.01 & regspil4_tidy$p.value<0.05, paste0(regspil4_tidy$estimate,'**') , 
                                       ifelse( regspil4_tidy$p.value>0.05 & regspil4_tidy$p.value<0.1, paste0(regspil4_tidy$estimate,'*'),
                                               regspil4_tidy$estimate)))  

regspil5_tidy$estimate = ifelse(regspil5_tidy$p.value<0.01,paste0(regspil5_tidy$estimate,'***') ,
                                ifelse(regspil5_tidy$p.value>0.01 & regspil5_tidy$p.value<0.05, paste0(regspil5_tidy$estimate,'**') , 
                                       ifelse( regspil5_tidy$p.value>0.05 & regspil5_tidy$p.value<0.1, paste0(regspil5_tidy$estimate,'*'),
                                               regspil5_tidy$estimate)))  


reglin2_tidy$estimate = ifelse(reglin2_tidy$p.value<0.01,paste0(reglin2_tidy$estimate,'***') ,
                                ifelse(reglin2_tidy$p.value>0.01 & reglin2_tidy$p.value<0.05, paste0(reglin2_tidy$estimate,'**') , 
                                       ifelse( reglin2_tidy$p.value>0.05 & reglin2_tidy$p.value<0.1, paste0(reglin2_tidy$estimate,'*'),
                                               reglin2_tidy$estimate))) 

# Do table output

reg_out1 <- data.frame(variables = c('1(Bike-Stations within 400m of a Station)',' ','$N^2$', '','$N^3$', '' , '$N^4$',' ','$N^5$',' ','\\#(Treated Stations by Line)','','Unit of Analysis','Day FE','Station FE','Controls','N'),
                       main2 = c(regspil2_tidy$estimate[[1]],paste0('(',regspil2_tidy$std.error[1],')') , regspil2_tidy$estimate[[2]],paste0('(',regspil2_tidy$std.error[2],')'),'','','','','','','','','Station','Yes','Yes','Yes',nobs(regspil2)),
                       main3 = c(regspil3_tidy$estimate[[1]],paste0('(',regspil3_tidy$std.error[1],')'),  '', '', regspil3_tidy$estimate[[2]],paste0('(',regspil3_tidy$std.error[2],')'),'','','','','','','Station','Yes','Yes','Yes',nobs(regspil3)),
                       main4 = c(regspil4_tidy$estimate[[1]],paste0('(',regspil4_tidy$std.error[1],')'),  '', '','','', regspil4_tidy$estimate[[2]],paste0('(',regspil4_tidy$std.error[2],')'),'','','','','Station','Yes','Yes','Yes',nobs(regspil4)),
                       main5 = c(regspil5_tidy$estimate[[1]],paste0('(',regspil5_tidy$std.error[1],')'),'','','','','','', regspil5_tidy$estimate[[2]],paste0('(',regspil5_tidy$std.error[2],')'),'','','Station','Yes','Yes','Yes',nobs(regspil5)),
                       main6 = c('','','','','','','','', '','',reglin2_tidy$estimate[[1]],paste0('(',reglin2_tidy$std.error[1],')'),'Line','Yes','Yes','No',nobs(reglin2))
                       
)                      

names(reg_out1) <- c('','(1)','(2)','(3)','(4)','(5)')                                                                   
## REPLICATION
sink(file = "Output/regspill.tex")

tab_reg1 <- kbl(reg_out1, booktabs = T,format = 'latex',row.names = FALSE, linesep = '',escape=F , align =c('lcccccc'))  %>%
  kable_styling(latex_options = c("repeat_header","scale_down")) %>%
  row_spec(c(12),hline_after = T)%>%
  #row_spec(c(30,34),hline_after = T) %>%
  kable_styling(latex_options = "hold_position") %>% 
  column_spec(1,border_left = F, border_right = T) 

substr(tab_reg1,103,nchar(tab_reg1)-13)


sink(file = NULL)

