


##################################################################
##                       MAIN REGRESSIONS                       ##
##################################################################


rm(list=ls())

library(sf)
library(dplyr)
library(mapview)
library(data.table)
library(lmtest)
library(sandwich)
library(stargazer)
library(lubridate)
library(broom)
library(ggplot2)
library(forcats)
library(kableExtra)
library(plm)

library(doParallel)
cl <- makeCluster(detectCores() - 1) 
registerDoParallel(cl)

setwd('G:/My Drive/Transport Mode Choice/CDMX')
# setwd('E:/My Drive/Transport Mode Choice/CDMX')

source("Code/remove_accents.R")

subway_f = readRDS('Data/Final/final_daily.rds')

days = seq(as.Date('2014-01-01'),as.Date('2019-12-01'),by='1 day')

subway_f = subway_f %>% filter(fecha %in% days)

#### Regressions

#### GET LIST OF STATIONS WHICH ARE NOT TREATED WITHIN 2014 AND 2019 (i.e., remove never-treated and always-treated)

# origstat =  c(subway_f$estacion_id[subway_f$fecha == min(subway_f$fecha) &subway_f$bin_0_400>0],subway_f$estacion_id[subway_f$fecha == max(subway_f$fecha) &subway_f$bin_0_400==0]) # 37 STATIONS REMOVED

origstat =  c(subway_f$estacion_id[subway_f$fecha == min(subway_f$fecha) &subway_f$bin_0_400>0]) # 30 STATIONS REMOVED

# Main
# regmain_clustered = readRDS('Data/Regs/regmain_clustered.rds')

regmain = lm(log_pases ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(fecha)  , data = subway_f[!(subway_f$estacion_id%in% origstat),])

regmain_clustered = coeftest(regmain,
                             vcov = vcovCL,
                             cluster = ~estacion_id)


regmain_tidy = tidy(regmain_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1200_3200','inter_1200_3200'))

saveRDS(regmain_clustered , 'Data/Regs/regmain_clustered.rds')

regmain_clustered = readRDS('Data/Regs/regmain_clustered.rds')
# 200m
# reg_200_clustered = readRDS('Data/Regs/reg_200_clustered.rds')

reg200 = lm(log_pases ~ bin_0_200 + n_1200_3200 + inter_1200_3200_2 + as.factor(estacion_id) + as.factor(fecha)  , data = subway_f[!(subway_f$estacion_id%in% origstat),])


reg_200_clustered = coeftest(reg200,
                             vcov = vcovCL,
                             cluster = ~estacion_id)

reg_200_tidy = tidy(reg_200_clustered) %>% 
  filter(term %in% c('bin_0_200', 'n_1200_3200','inter_1200_3200_2'))

saveRDS(reg_200_clustered , 'Data/Regs/reg_200_clustered.rds')
reg_200_clustered =  readRDS('Data/Regs/reg_200_clustered.rds')

# Robustness for the outer-ring outside

# regrobout_clustered = readRDS('Data/Regs/regrobout_clustered.rds')

regrobout = lm(log_pases ~ bin_0_400 + n_1000_2000  + inter_1000_2000 + as.factor(estacion_id) + as.factor(fecha) , data = subway_f[!(subway_f$estacion_id%in% origstat),])

regrobout_clustered = coeftest(regrobout,
                               vcov = vcovCL,
                               cluster = ~estacion_id)


regrobout_tidy =  tidy(regrobout_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1000_2000','inter_1000_2000'))

saveRDS(regrobout_clustered , 'Data/Regs/regrobout_clustered.rds')

regrobout_clustered = readRDS('Data/Regs/regrobout_clustered.rds')

####### Now add controls (for appendix)

# regmain_rob_clustered = readRDS('Data/Regs/regmain_rob_clustered.rds')

regmain_rob = lm(log_pases ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(fecha) + nolab, data = subway_f[!(subway_f$estacion_id%in% origstat),])

regmain_rob_clustered = coeftest(regmain_rob,
                                 vcov = vcovCL,
                                 cluster = ~estacion_id)

regmain_rob_tidy = tidy(regmain_rob_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1200_3200','inter_1200_3200'))

saveRDS(regmain_rob_clustered , 'regmain_rob_clustered.rds')

# 200m treatment

# reg_200_rob_clustered = readRDS('Data/Regs/reg_200_rob_clustered.rds')

reg200_rob = lm(log_pases ~ bin_0_200 + n_1200_3200 + inter_1200_3200_2 + as.factor(estacion_id) + as.factor(fecha) + nolab  , data = subway_f[!(subway_f$estacion_id%in% origstat),])


reg_200_rob_clustered = coeftest(reg200_rob,
                                 vcov = vcovCL,
                                 cluster = ~estacion_id)

reg_200_rob_tidy = tidy(reg_200_rob_clustered) %>% 
  filter(term %in% c('bin_0_200', 'n_1200_3200','inter_1200_3200_2'))

saveRDS(reg_200_rob_clustered , 'reg_200_rob_clustered.rds')

# Robustness for the outer-ring outside

# regrobout_rob_clustered = readRDS('Data/Regs/regrobout_rob_clustered.rds')

regrobout_rob = lm(log_pases ~ bin_0_400 + n_1000_2000  + inter_1000_2000 + as.factor(estacion_id) + as.factor(fecha) + nolab , data = subway_f[!(subway_f$estacion_id%in% origstat),])

regrobout_rob_clustered = coeftest(regrobout_rob,
                                   vcov = vcovCL,
                                   cluster = ~estacion_id)


regrobout_rob_tidy =  tidy(regrobout_rob_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1000_2000','inter_1000_2000'))

saveRDS(regrobout_rob_clustered , 'regrobout_rob_clustered.rds')


#### OUT REGRESSION

# With no controls
regmain_tidy$estimate = format(round(regmain_tidy$estimate,3),nsmall=3)
regmain_tidy$std.error = format(round(regmain_tidy$std.error,3),nsmall=3)

reg_200_tidy$estimate = format(round(reg_200_tidy$estimate,3),nsmall=3)
reg_200_tidy$std.error = format(round(reg_200_tidy$std.error,3),nsmall=3)

regrobout_tidy$estimate = format(round(regrobout_tidy$estimate,3),nsmall=3)
regrobout_tidy$std.error = format(round(regrobout_tidy$std.error,3),nsmall=3)

regmain_tidy$estimate = ifelse(regmain_tidy$p.value<0.01,paste0(regmain_tidy$estimate,'***') ,
                               ifelse(regmain_tidy$p.value>0.01 & regmain_tidy$p.value<0.05, paste0(regmain_tidy$estimate,'**') , 
                                      ifelse( regmain_tidy$p.value>0.05 & regmain_tidy$p.value<0.1, paste0(regmain_tidy$estimate,'*'),
                                              regmain_tidy$estimate)))  


reg_200_tidy$estimate = ifelse(reg_200_tidy$p.value<0.01,paste0(reg_200_tidy$estimate,'***') ,
                              ifelse(reg_200_tidy$p.value>0.01 & reg_200_tidy$p.value<0.05, paste0(reg_200_tidy$estimate,'**') , 
                                     ifelse( reg_200_tidy$p.value>0.05 & reg_200_tidy$p.value<0.1, paste0(reg_200_tidy$estimate,'*'),
                                             reg_200_tidy$estimate)))  


reg_200_tidy$estimate = ifelse(reg_200_tidy$p.value<0.01,paste0(reg_200_tidy$estimate,'***') ,
                              ifelse(reg_200_tidy$p.value>0.01 & reg_200_tidy$p.value<0.05, paste0(reg_200_tidy$estimate,'**') , 
                                     ifelse( reg_200_tidy$p.value>0.05 & reg_200_tidy$p.value<0.1, paste0(reg_200_tidy$estimate,'*'),
                                             reg_200_tidy$estimate)))  

regrobout_tidy$estimate = ifelse(regrobout_tidy$p.value<0.01,paste0(regrobout_tidy$estimate,'***') ,
                                 ifelse(regrobout_tidy$p.value>0.01 & regrobout_tidy$p.value<0.05, paste0(regrobout_tidy$estimate,'**') , 
                                        ifelse( regrobout_tidy$p.value>0.05 & regrobout_tidy$p.value<0.1, paste0(regrobout_tidy$estimate,'*'),
                                                regrobout_tidy$estimate)))  
# With controls

regmain_rob_tidy$estimate = format(round(regmain_rob_tidy$estimate,3),nsmall=3)
regmain_rob_tidy$std.error = format(round(regmain_rob_tidy$std.error,3),nsmall=3)

reg_200_rob_tidy$estimate = format(round(reg_200_rob_tidy$estimate,3),nsmall=3)
reg_200_rob_tidy$std.error = format(round(reg_200_rob_tidy$std.error,3),nsmall=3)

regrobout_rob_tidy$estimate = format(round(regrobout_rob_tidy$estimate,3),nsmall=3)
regrobout_rob_tidy$std.error = format(round(regrobout_rob_tidy$std.error,3),nsmall=3)

regmain_rob_tidy$estimate = ifelse(regmain_rob_tidy$p.value<0.01,paste0(regmain_rob_tidy$estimate,'***') ,
                                   ifelse(regmain_rob_tidy$p.value>0.01 & regmain_rob_tidy$p.value<0.05, paste0(regmain_rob_tidy$estimate,'**') , 
                                          ifelse( regmain_rob_tidy$p.value>0.05 & regmain_rob_tidy$p.value<0.1, paste0(regmain_rob_tidy$estimate,'*'),
                                                  regmain_rob_tidy$estimate)))  


reg_200_rob_tidy$estimate = ifelse(reg_200_rob_tidy$p.value<0.01,paste0(reg_200_rob_tidy$estimate,'***') ,
                                  ifelse(reg_200_rob_tidy$p.value>0.01 & reg_200_rob_tidy$p.value<0.05, paste0(reg_200_rob_tidy$estimate,'**') , 
                                         ifelse( reg_200_rob_tidy$p.value>0.05 & reg_200_rob_tidy$p.value<0.1, paste0(reg_200_rob_tidy$estimate,'*'),
                                                 reg_200_rob_tidy$estimate)))  

regrobout_rob_tidy$estimate = ifelse(regrobout_rob_tidy$p.value<0.01,paste0(regrobout_rob_tidy$estimate,'***') ,
                                     ifelse(regrobout_rob_tidy$p.value>0.01 & regrobout_rob_tidy$p.value<0.05, paste0(regrobout_rob_tidy$estimate,'**') , 
                                            ifelse( regrobout_rob_tidy$p.value>0.05 & regrobout_rob_tidy$p.value<0.1, paste0(regrobout_rob_tidy$estimate,'*'),
                                                    regrobout_rob_tidy$estimate)))  
# Do table output

reg_out1 <- data.frame(variables = c('1(Bike-Station Within 400m)','','1(Bike-Station Within 200m)','','\\#(Bike-Stations Between 1200m and 3200m)','','\\#(Bike-Share Between 1000m and 2000m)','','\\#(Bike-Stations Between 1200m and 3200m)*1(Bike-Share Within 400m))','','\\#(Bike-Stations Between 1200m and 3200m)*1(Bike-Share Within 200m))','','\\#(Bike-Share Between 1000m and 2000m)*1(Bike-Share Within 400m)','','Day FE','Station FE','Controls','N'),
                       main = c(regmain_tidy$estimate[[1]],paste0('(',regmain_tidy$std.error[1],')'),  '','', regmain_tidy$estimate[[2]],paste0('(',regmain_tidy$std.error[2],')'),'','', regmain_tidy$estimate[[3]],paste0('(',regmain_tidy$std.error[3],')'),'','','','','Yes','Yes','No','337222'),
                       main200 = c( '','', reg_200_tidy$estimate[[1]],paste0('(',reg_200_tidy$std.error[1],')'),   reg_200_tidy$estimate[[2]],paste0('(',reg_200_tidy$std.error[2],')'),'','','','', reg_200_tidy$estimate[[3]],paste0('(',reg_200_tidy$std.error[3],')'),'','','Yes','Yes','No','337222'),
                       robout = c(  regrobout_tidy$estimate[[1]],paste0('(',regrobout_tidy$std.error[1],')'),'','','','',   regrobout_tidy$estimate[[2]],paste0('(',regrobout_tidy$std.error[2],')'),'','','','', regrobout_tidy$estimate[[3]],paste0('(',regrobout_tidy$std.error[3],')'),'Yes','Yes','No','337222')
                       # maincon = c(regmain_rob_tidy$estimate[[1]],paste0('(',regmain_rob_tidy$std.error[1],')'),  '','', regmain_rob_tidy$estimate[[2]],paste0('(',regmain_rob_tidy$std.error[2],')'),'','', regmain_rob_tidy$estimate[[3]],paste0('(',regmain_rob_tidy$std.error[3],')'),'','','','','Yes','Yes','Yes',nobs(regmain_rob_clustered)),
                      #  main200con = c( '','', reg200_rob_tidy$estimate[[1]],paste0('(',reg200_rob_tidy$std.error[1],')'),   reg200_rob_tidy$estimate[[2]],paste0('(',reg200_rob_tidy$std.error[2],')'),'','','','', reg200_rob_tidy$estimate[[3]],paste0('(',reg200_rob_tidy$std.error[3],')'),'','','Yes','Yes','Yes',nobs(reg_200_rob_clustered)),
                       # roboutcon = c(  regrobout_rob_tidy$estimate[[1]],paste0('(',regrobout_rob_tidy$std.error[1],')'),'','','','',   regrobout_rob_tidy$estimate[[2]],paste0('(',regrobout_rob_tidy$std.error[2],')'),'','','','', regrobout_rob_tidy$estimate[[3]],paste0('(',regrobout_rob_tidy$std.error[3],')'),'Yes','Yes','Yes',nobs(regrobout_rob_clustered))
)                      

names(reg_out1) <- c('','(1)','(2)','(3)')                                                                   
## REPLICATION
sink(file = "Output/regbase_cdmx.tex")

tab_reg1 <- kbl(reg_out1, booktabs = T,format = 'latex',row.names = FALSE, linesep = '',escape=F , align =c('l', rep('c,6')))  %>%
  kable_styling(latex_options = c("repeat_header","scale_down")) %>%
  row_spec(c(14),hline_after = T)%>%
  #row_spec(c(30,34),hline_after = T) %>%
  kable_styling(latex_options = "hold_position") %>% 
  column_spec (1,border_left = F, border_right = T) 

substr(tab_reg1,103,nchar(tab_reg1)-13)


sink(file = NULL)

