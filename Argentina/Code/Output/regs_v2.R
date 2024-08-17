


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

setwd('G:/My Drive/Transport Mode Choice/Argentina')
# setwd('E:/My Drive/Transport Mode Choice/Argentina')

source("Code/remove_accents.R")

subway_f = readRDS('Data Argentina/Final/final_daily.rds')

#### Regressions

#### GET LIST OF STATIONS WHICH ARE NOT TREATED WITHIN 2014 AND 2019 (i.e., remove never-treated and always-treated)

# origstat =  c(subway_f$estacion_id[subway_f$fecha == min(subway_f$fecha) &subway_f$bin_0_400>0],subway_f$estacion_id[subway_f$fecha == max(subway_f$fecha) &subway_f$bin_0_400==0]) # 37 STATIONS REMOVED

origstat =  c(subway_f$estacion_id[subway_f$fecha == min(subway_f$fecha) &subway_f$bin_0_400>0]) # 30 STATIONS REMOVED

# Main

# regmain = lm(log_pases ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(month)*wkday  , data = subway_f[!(subway_f$estacion_id%in% origstat),])

regmain = lm(log_pases ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(fecha)  , data = subway_f[!(subway_f$estacion_id%in% origstat),])

regmain_clustered = coeftest(regmain,
                                vcov = vcovCL,
                                cluster = ~estacion_id)


regmain_tidy = tidy(regmain_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1200_3200','inter_1200_3200'))

saveRDS(regmain_clustered,'Data Argentina/Regs/regmain_clustered.rds')

# 200m
reg200 = lm(log_pases ~ bin_0_200 + n_1200_3200 + inter_1200_3200_2 + as.factor(estacion_id) + as.factor(fecha)  , data = subway_f[!(subway_f$estacion_id%in% origstat),])


reg_200_clustered = coeftest(reg200,
                                vcov = vcovCL,
                                cluster = ~estacion_id)

reg200_tidy = tidy(reg_200_clustered) %>% 
  filter(term %in% c('bin_0_200', 'n_1200_3200','inter_1200_3200_2'))

saveRDS(reg_200_clustered,'Data Argentina/Regs/reg_200_clustered.rds')

# Robustness for the outer-ring outside

regrobout = lm(log_pases ~ bin_0_400 + n_1000_2000  + inter_1000_2000 + as.factor(estacion_id) + as.factor(fecha) , data = subway_f[!(subway_f$estacion_id%in% origstat),])
 
regrobout_clustered = coeftest(regrobout,
                               vcov = vcovCL,
                               cluster = ~estacion_id)


regrobout_tidy =  tidy(regrobout_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1000_2000','inter_1000_2000'))

saveRDS(regrobout_clustered,'Data Argentina/Regs/regrobout_clustered.rds')

####### Now add controls (for appendix)

regmain_rob = lm(log_pases ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(fecha)  + age + male + yrschool + share_work , data = subway_f[!(subway_f$estacion_id%in% origstat),])

regmain_rob_clustered = coeftest(regmain_rob,
                                 vcov = vcovCL,
                                 cluster = ~estacion_id)

regmain_rob_tidy = tidy(regmain_rob_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1200_3200','inter_1200_3200'))

saveRDS(regmain_rob_clustered,'Data Argentina/Regs/regmain_rob_clustered.rds')

# 200m treatment

reg200_rob = lm(log_pases ~ bin_0_200 + n_1200_3200 + inter_1200_3200_2 + as.factor(estacion_id) + as.factor(fecha) + age + male + yrschool + share_work  , data = subway_f[!(subway_f$estacion_id%in% origstat),])


reg_200_rob_clustered = coeftest(reg200_rob,
                             vcov = vcovCL,
                             cluster = ~estacion_id)

reg200_rob_tidy = tidy(reg_200_rob_clustered) %>% 
  filter(term %in% c('bin_0_200', 'n_1200_3200','inter_1200_3200_2'))

saveRDS(reg_200_rob_clustered,'Data Argentina/Regs/reg_200_rob_clustered.rds')

# Robustness for the outer-ring outside

regrobout_rob = lm(log_pases ~ bin_0_400 + n_1000_2000  + inter_1000_2000 + as.factor(estacion_id) + as.factor(fecha) + age + male + yrschool + share_work , data = subway_f[!(subway_f$estacion_id%in% origstat),])

regrobout_rob_clustered = coeftest(regrobout_rob,
                               vcov = vcovCL,
                               cluster = ~estacion_id)


regrobout_rob_tidy =  tidy(regrobout_rob_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1000_2000','inter_1000_2000'))

saveRDS(regrobout_rob_clustered,'Data Argentina/Regs/regrobout_rob_tidy.rds')

regrobout_rob_clustered = readRDS('Data Argentina/Regs/regrobout_rob_tidy.rds')
#### OUT REGRESSION

# With no controls
regmain_tidy$estimate = format(round(regmain_tidy$estimate,3),nsmall=3)
regmain_tidy$std.error = format(round(regmain_tidy$std.error,3),nsmall=3)

reg200_tidy$estimate = format(round(reg200_tidy$estimate,3),nsmall=3)
reg200_tidy$std.error = format(round(reg200_tidy$std.error,3),nsmall=3)

regrobout_tidy$estimate = format(round(regrobout_tidy$estimate,3),nsmall=3)
regrobout_tidy$std.error = format(round(regrobout_tidy$std.error,3),nsmall=3)

regmain_tidy$estimate = ifelse(regmain_tidy$p.value<0.01,paste0(regmain_tidy$estimate,'***') ,
                               ifelse(regmain_tidy$p.value>0.01 & regmain_tidy$p.value<0.05, paste0(regmain_tidy$estimate,'**') , 
                                      ifelse( regmain_tidy$p.value>0.05 & regmain_tidy$p.value<0.1, paste0(regmain_tidy$estimate,'*'),
                                              regmain_tidy$estimate)))  


reg200_tidy$estimate = ifelse(reg200_tidy$p.value<0.01,paste0(reg200_tidy$estimate,'***') ,
                               ifelse(reg200_tidy$p.value>0.01 & reg200_tidy$p.value<0.05, paste0(reg200_tidy$estimate,'**') , 
                                      ifelse( reg200_tidy$p.value>0.05 & reg200_tidy$p.value<0.1, paste0(reg200_tidy$estimate,'*'),
                                              reg200_tidy$estimate)))  


reg200_tidy$estimate = ifelse(reg200_tidy$p.value<0.01,paste0(reg200_tidy$estimate,'***') ,
                               ifelse(reg200_tidy$p.value>0.01 & reg200_tidy$p.value<0.05, paste0(reg200_tidy$estimate,'**') , 
                                      ifelse( reg200_tidy$p.value>0.05 & reg200_tidy$p.value<0.1, paste0(reg200_tidy$estimate,'*'),
                                              reg200_tidy$estimate)))  

regrobout_tidy$estimate = ifelse(regrobout_tidy$p.value<0.01,paste0(regrobout_tidy$estimate,'***') ,
                              ifelse(regrobout_tidy$p.value>0.01 & regrobout_tidy$p.value<0.05, paste0(regrobout_tidy$estimate,'**') , 
                                     ifelse( regrobout_tidy$p.value>0.05 & regrobout_tidy$p.value<0.1, paste0(regrobout_tidy$estimate,'*'),
                                             regrobout_tidy$estimate)))  
# With controls

regmain_rob_tidy$estimate = format(round(regmain_rob_tidy$estimate,3),nsmall=3)
regmain_rob_tidy$std.error = format(round(regmain_rob_tidy$std.error,3),nsmall=3)

reg200_rob_tidy$estimate = format(round(reg200_rob_tidy$estimate,3),nsmall=3)
reg200_rob_tidy$std.error = format(round(reg200_rob_tidy$std.error,3),nsmall=3)

regrobout_rob_tidy$estimate = format(round(regrobout_rob_tidy$estimate,3),nsmall=3)
regrobout_rob_tidy$std.error = format(round(regrobout_rob_tidy$std.error,3),nsmall=3)

regmain_rob_tidy$estimate = ifelse(regmain_rob_tidy$p.value<0.01,paste0(regmain_rob_tidy$estimate,'***') ,
                               ifelse(regmain_rob_tidy$p.value>0.01 & regmain_rob_tidy$p.value<0.05, paste0(regmain_rob_tidy$estimate,'**') , 
                                      ifelse( regmain_rob_tidy$p.value>0.05 & regmain_rob_tidy$p.value<0.1, paste0(regmain_rob_tidy$estimate,'*'),
                                              regmain_rob_tidy$estimate)))  


reg200_rob_tidy$estimate = ifelse(reg200_rob_tidy$p.value<0.01,paste0(reg200_rob_tidy$estimate,'***') ,
                              ifelse(reg200_rob_tidy$p.value>0.01 & reg200_rob_tidy$p.value<0.05, paste0(reg200_rob_tidy$estimate,'**') , 
                                     ifelse( reg200_rob_tidy$p.value>0.05 & reg200_rob_tidy$p.value<0.1, paste0(reg200_rob_tidy$estimate,'*'),
                                             reg200_rob_tidy$estimate)))  

regrobout_rob_tidy$estimate = ifelse(regrobout_rob_tidy$p.value<0.01,paste0(regrobout_rob_tidy$estimate,'***') ,
                                 ifelse(regrobout_rob_tidy$p.value>0.01 & regrobout_rob_tidy$p.value<0.05, paste0(regrobout_rob_tidy$estimate,'**') , 
                                        ifelse( regrobout_rob_tidy$p.value>0.05 & regrobout_rob_tidy$p.value<0.1, paste0(regrobout_rob_tidy$estimate,'*'),
                                                regrobout_rob_tidy$estimate)))  
# Do table output

reg_out1 <- data.frame(variables = c('1(Bike-Station Within 400m)','','1(Bike-Station Within 200m)','','\\#(Bike-Stations Between 1200m and 3200m)','','\\#(Bike-Share Between 1000m and 2000m)','','\\#(Bike-Stations Between 1200m and 3200m)*1(Bike-Share Within 400m))','','\\#(Bike-Stations Between 1200m and 3200m)*1(Bike-Share Within 200m))','','\\#(Bike-Share Between 1000m and 2000m)*1(Bike-Share Within 400m)','','Day FE','Station FE','Controls','N'),
                            main = c(regmain_tidy$estimate[[1]],paste0('(',regmain_tidy$std.error[1],')'),  '','', regmain_tidy$estimate[[2]],paste0('(',regmain_tidy$std.error[2],')'),'','', regmain_tidy$estimate[[3]],paste0('(',regmain_tidy$std.error[3],')'),'','','','','Yes','Yes','No',nobs(regmain)),
                       main200 = c( '','', reg200_tidy$estimate[[1]],paste0('(',reg200_tidy$std.error[1],')'),   reg200_tidy$estimate[[2]],paste0('(',reg200_tidy$std.error[2],')'),'','','','', reg200_tidy$estimate[[3]],paste0('(',reg200_tidy$std.error[3],')'),'','','Yes','Yes','No',nobs(reg200)),
                       robout = c(  regrobout_tidy$estimate[[1]],paste0('(',regrobout_tidy$std.error[1],')'),'','','','',   regrobout_tidy$estimate[[2]],paste0('(',regrobout_tidy$std.error[2],')'),'','','','', regrobout_tidy$estimate[[3]],paste0('(',regrobout_tidy$std.error[3],')'),'Yes','Yes','No',nobs(regrobout)),
                       maincon = c(regmain_rob_tidy$estimate[[1]],paste0('(',regmain_rob_tidy$std.error[1],')'),  '','', regmain_rob_tidy$estimate[[2]],paste0('(',regmain_rob_tidy$std.error[2],')'),'','', regmain_rob_tidy$estimate[[3]],paste0('(',regmain_rob_tidy$std.error[3],')'),'','','','','Yes','Yes','Yes',nobs(regmain_rob)),
                       main200con = c( '','', reg200_rob_tidy$estimate[[1]],paste0('(',reg200_rob_tidy$std.error[1],')'),   reg200_rob_tidy$estimate[[2]],paste0('(',reg200_rob_tidy$std.error[2],')'),'','','','', reg200_rob_tidy$estimate[[3]],paste0('(',reg200_rob_tidy$std.error[3],')'),'','','Yes','Yes','Yes',nobs(reg200_rob)),
                       roboutcon = c(  regrobout_rob_tidy$estimate[[1]],paste0('(',regrobout_rob_tidy$std.error[1],')'),'','','','',   regrobout_rob_tidy$estimate[[2]],paste0('(',regrobout_rob_tidy$std.error[2],')'),'','','','', regrobout_rob_tidy$estimate[[3]],paste0('(',regrobout_rob_tidy$std.error[3],')'),'Yes','Yes','Yes',nobs(regrobout_rob))
)                      
                                                                      
names(reg_out1) <- c('','(1)','(2)','(3)','(4)','(5)','(6)')                                                                   
## REPLICATION
sink(file = "Output/regbase.tex")

tab_reg1 <- kbl(reg_out1, booktabs = T,format = 'latex',row.names = FALSE, linesep = '',escape=F , align =c('lcccccccc'))  %>%
  kable_styling(latex_options = c("repeat_header","scale_down")) %>%
  row_spec(c(14),hline_after = T)%>%
  #row_spec(c(30,34),hline_after = T) %>%
  kable_styling(latex_options = "hold_position") %>% 
  column_spec (1,border_left = F, border_right = T) 

substr(tab_reg1,103,nchar(tab_reg1)-13)


sink(file = NULL)


## HETEROGENEITY ANALYSIS ###########



# Main


regmain_gr = lm(log_pases ~ tg1 + tg2 + tg3 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(fecha) + age + male + yrschool + share_work , data = subway_f[!(subway_f$estacion_id%in% origstat),])


regmain_gr_clustered = coeftest(regmain_gr,
                               vcov = vcovCL,
                               cluster = ~estacion_id)


regmain_gr_tidy = tidy(regmain_gr_clustered) %>% 
  filter(term %in% c('tg1','tg2','tg3', 'n_1200_3200','inter_1200_3200'))

saveRDS(regmain_gr_clustered,'Data Argentina/Regs/regmain_gr_clustered.rds')

## DIAS HABILES

regmain_wkday = lm(log_pases ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(fecha) + age + male + yrschool + share_work , data = subway_f[ subway_f$nolab ==0 & !(subway_f$estacion_id%in% origstat),])

regmain_wkday_clustered = coeftest(regmain_wkday,
                                   vcov = vcovCL,
                                   cluster = ~estacion_id)

regmain_wkday_tidy = tidy(regmain_wkday_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1200_3200','inter_1200_3200'))

saveRDS(regmain_wkday_clustered,'Data Argentina/Regs/regmain_wkday_clustered.rds')

## FERIADOS

regmain_fer = lm(log_pases ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(fecha)  + age + male + yrschool + share_work, data = subway_f[ subway_f$nolab ==1 & !(subway_f$estacion_id%in% origstat) ,])

regmain_fer_clustered = coeftest(regmain_fer,
                                 vcov = vcovCL,
                                 cluster = ~estacion_id)

regmain_fer_tidy = tidy(regmain_fer_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1200_3200','inter_1200_3200'))

saveRDS(regmain_fer_clustered,'Data Argentina/Regs/regmain_fer_clustered.rds')

## PEAK HOURS
# 
# regmain_peak = lm(log_pases_peak ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(month) + tavg , data = subway_f[ subway_f$nolab ==0 & !(subway_f$estacion_id%in% origstat),])
# 
# regmain_peak_clustered = coeftest(regmain_peak,
#                                  vcov = vcovCL,
#                                  cluster = ~estacion_id)
# 
# regmain_peak_tidy = tidy(regmain_peak_clustered) %>% 
#   filter(term %in% c('bin_0_400', 'n_1200_3200','inter_1200_3200'))
# 
# ## NO PEAK HOURS
# 
# regmain_nopeak = lm(log_pases_nopeak ~ bin_0_400 + n_1200_3200 + inter_1200_3200 + as.factor(estacion_id) + as.factor(month) + tavg , data = subway_f[ subway_f$nolab ==0 & !(subway_f$estacion_id%in% origstat),])
# 
# regmain_nopeak_clustered = coeftest(regmain_nopeak,
#                                   vcov = vcovCL,
#                                   cluster = ~estacion_id)
# 
# regmain_nopeak_tidy = tidy(regmain_nopeak_clustered) %>% 
#   filter(term %in% c('bin_0_400', 'n_1200_3200','inter_1200_3200'))



#### OUT REGRESSION

regmain_fer_tidy$estimate = round(regmain_fer_tidy$estimate,3)
regmain_fer_tidy$std.error = round(regmain_fer_tidy$std.error,3)

regmain_wkday_tidy$estimate = round(regmain_wkday_tidy$estimate,3)
regmain_wkday_tidy$std.error = round(regmain_wkday_tidy$std.error,3)

regmain_gr_tidy$estimate = round(regmain_gr_tidy$estimate,3)
regmain_gr_tidy$std.error = round(regmain_gr_tidy$std.error,3)

## Add Stars

# No Laboral
regmain_fer_tidy$estimate = ifelse(regmain_fer_tidy$p.value<0.01,paste0(regmain_fer_tidy$estimate,'***') ,
                               ifelse(regmain_fer_tidy$p.value>0.01 & regmain_fer_tidy$p.value<0.05, paste0(regmain_fer_tidy$estimate,'**') , 
                                      ifelse( regmain_fer_tidy$p.value>0.05 & regmain_fer_tidy$p.value<0.1, paste0(regmain_fer_tidy$estimate,'*'),
                                              regmain_fer_tidy$estimate)))  

# Laboral
regmain_wkday_tidy$estimate = ifelse(regmain_wkday_tidy$p.value<0.01,paste0(regmain_wkday_tidy$estimate,'***') ,
                              ifelse(regmain_wkday_tidy$p.value>0.01 & regmain_wkday_tidy$p.value<0.05, paste0(regmain_wkday_tidy$estimate,'**') , 
                                     ifelse( regmain_wkday_tidy$p.value>0.05 & regmain_wkday_tidy$p.value<0.1, paste0(regmain_wkday_tidy$estimate,'*'),
                                             regmain_wkday_tidy$estimate)))  

# By group
regmain_gr_tidy$estimate = ifelse(regmain_gr_tidy$p.value<0.01,paste0(regmain_gr_tidy$estimate,'***') ,
                              ifelse(regmain_gr_tidy$p.value>0.01 & regmain_gr_tidy$p.value<0.05, paste0(regmain_gr_tidy$estimate,'**') , 
                                     ifelse( regmain_gr_tidy$p.value>0.05 & regmain_gr_tidy$p.value<0.1, paste0(regmain_gr_tidy$estimate,'*'),
                                             regmain_gr_tidy$estimate)))  


#### Do table output

reg_out2 <- data.frame(variables = c('1(Bike-Station Within 400m)','','1(Bike-Station Within 400m \\& Treated in 2015-2016)','','1(Bike-Station Within 400m \\& Treated in 2017-2018)','','1(Bike-Station Within 400m \\& Treated in 2019)','','Sample','Day FE','Station FE','Controls','N'),
                       main = c(regmain_fer_tidy$estimate[[1]],paste0('(',regmain_fer_tidy$std.error[1],')'),  '','','','','','','Non-Working','Yes','Yes','Yes',length(regmain_fer$residuals)),
                       main200 = c(regmain_wkday_tidy$estimate[[1]],paste0('(',regmain_wkday_tidy$std.error[1],')'),  '','','','','','','Working','Yes','Yes','Yes',length(regmain_wkday$residuals)),
                       robout = c('','',regmain_gr_tidy$estimate[[1]],paste0('(',regmain_gr_tidy$std.error[1],')')  ,regmain_gr_tidy$estimate[[2]],paste0('(',regmain_gr_tidy$std.error[2],')')  ,regmain_gr_tidy$estimate[[3]],paste0('(',regmain_gr_tidy$std.error[3],')')  ,'All','Yes','Yes','Yes',length(regmain_gr$residuals))
)                      

names(reg_out2) <- c('','(1)','(2)','(3)')                                                                   
## REPLICATION
sink(file = "Output/reghet.tex")

tab_reg2 <- kbl(reg_out2, booktabs = T,format = 'latex',row.names = FALSE, linesep = '',escape=F , align =c('lccccccc'))  %>%
  kable_styling(latex_options = c("repeat_header","scale_down")) %>%
  row_spec(c(8),hline_after = T)%>%
  #row_spec(c(30,34),hline_after = T) %>%
  kable_styling(latex_options = "hold_position") %>% 
  column_spec (1,border_left = F, border_right = T) 

substr(tab_reg2,103,nchar(tab_reg2)-13)


sink(file = NULL)









######################## APPENDIX

## ROBUST STANDARD ERRORS ##########

# Main

regmain_clustered = coeftest(regmain,
                             vcov =  vcovHC(regmain, type="HC1"),
                             cluster = ~estacion_id)


regmain_tidy = tidy(regmain_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1200_3200','inter_1200_3200'))

saveRDS(regmain_clustered,'Data Argentina/Regs/regmain_clustered_robse.rds')

# 200m

reg_200_clustered = coeftest(reg200,
                             vcov =  vcovHC(reg200, type="HC1"),
                             cluster = ~estacion_id)

reg200_tidy = tidy(reg_200_clustered) %>% 
  filter(term %in% c('bin_0_200', 'n_1200_3200','inter_1200_3200_2'))

saveRDS(reg_200_clustered,'Data Argentina/Regs/reg_200_clustered_robse.rds')

# Robustness for the outer-ring outside

regrobout_clustered = coeftest(regrobout,
                               vcov =  vcovHC(regrobout, type="HC1"),
                               cluster = ~estacion_id)


regrobout_tidy =  tidy(regrobout_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1000_2000','inter_1000_2000'))

saveRDS(regrobout_clustered,'Data Argentina/Regs/regrobout_clustered_robse.rds')

####### Now add controls (for appendix)

regmain_rob_clustered = coeftest(regmain_rob,
                                 vcov =  vcovHC(regmain_rob, type="HC1"),
                                 cluster = ~estacion_id)

regmain_rob_tidy = tidy(regmain_rob_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1200_3200','inter_1200_3200'))

saveRDS(regmain_rob_clustered,'Data Argentina/Regs/regmain_rob_clustered_robse.rds')

# 200m treatment

reg_200_rob_clustered = coeftest(reg200_rob,
                                 vcov =  vcovHC(reg200_rob, type="HC1"),
                                 cluster = ~estacion_id)

reg200_rob_tidy = tidy(reg_200_rob_clustered) %>% 
  filter(term %in% c('bin_0_200', 'n_1200_3200','inter_1200_3200_2'))

saveRDS(reg_200_rob_clustered,'Data Argentina/Regs/reg_200_rob_clustered_robse.rds')

# Robustness for the outer-ring outside

regrobout_rob_clustered = coeftest(regrobout_rob,
                                   vcov =  vcovHC(regrobout_rob, type="HC1"),
                                   cluster = ~estacion_id)


regrobout_rob_tidy =  tidy(regrobout_rob_clustered) %>% 
  filter(term %in% c('bin_0_400', 'n_1000_2000','inter_1000_2000'))


saveRDS(regrobout_rob_clustered,'Data Argentina/Regs/regrobout_rob_clustered_robse.rds')

#### OUT REGRESSION

# With no controls
regmain_tidy$estimate = format(round(regmain_tidy$estimate,3),nsmall=3)
regmain_tidy$std.error = format(round(regmain_tidy$std.error,3),nsmall=3)

reg200_tidy$estimate = format(round(reg200_tidy$estimate,3),nsmall=3)
reg200_tidy$std.error = format(round(reg200_tidy$std.error,3),nsmall=3)

regrobout_tidy$estimate = format(round(regrobout_tidy$estimate,3),nsmall=3)
regrobout_tidy$std.error = format(round(regrobout_tidy$std.error,3),nsmall=3)

regmain_tidy$estimate = ifelse(regmain_tidy$p.value<0.01,paste0(regmain_tidy$estimate,'***') ,
                               ifelse(regmain_tidy$p.value>0.01 & regmain_tidy$p.value<0.05, paste0(regmain_tidy$estimate,'**') , 
                                      ifelse( regmain_tidy$p.value>0.05 & regmain_tidy$p.value<0.1, paste0(regmain_tidy$estimate,'*'),
                                              regmain_tidy$estimate)))  


reg200_tidy$estimate = ifelse(reg200_tidy$p.value<0.01,paste0(reg200_tidy$estimate,'***') ,
                              ifelse(reg200_tidy$p.value>0.01 & reg200_tidy$p.value<0.05, paste0(reg200_tidy$estimate,'**') , 
                                     ifelse( reg200_tidy$p.value>0.05 & reg200_tidy$p.value<0.1, paste0(reg200_tidy$estimate,'*'),
                                             reg200_tidy$estimate)))  

regrobout_tidy$estimate = ifelse(regrobout_tidy$p.value<0.01,paste0(regrobout_tidy$estimate,'***') ,
                                 ifelse(regrobout_tidy$p.value>0.01 & regrobout_tidy$p.value<0.05, paste0(regrobout_tidy$estimate,'**') , 
                                        ifelse( regrobout_tidy$p.value>0.05 & regrobout_tidy$p.value<0.1, paste0(regrobout_tidy$estimate,'*'),
                                                regrobout_tidy$estimate)))  
# With controls

regmain_rob_tidy$estimate = format(round(regmain_rob_tidy$estimate,3),nsmall=3)
regmain_rob_tidy$std.error = format(round(regmain_rob_tidy$std.error,3),nsmall=3)

reg200_rob_tidy$estimate = format(round(reg200_rob_tidy$estimate,3),nsmall=3)
reg200_rob_tidy$std.error = format(round(reg200_rob_tidy$std.error,3),nsmall=3)

regrobout_rob_tidy$estimate = format(round(regrobout_rob_tidy$estimate,3),nsmall=3)
regrobout_rob_tidy$std.error = format(round(regrobout_rob_tidy$std.error,3),nsmall=3)

regmain_rob_tidy$estimate = ifelse(regmain_rob_tidy$p.value<0.01,paste0(regmain_rob_tidy$estimate,'***') ,
                                   ifelse(regmain_rob_tidy$p.value>0.01 & regmain_rob_tidy$p.value<0.05, paste0(regmain_rob_tidy$estimate,'**') , 
                                          ifelse( regmain_rob_tidy$p.value>0.05 & regmain_rob_tidy$p.value<0.1, paste0(regmain_rob_tidy$estimate,'*'),
                                                  regmain_rob_tidy$estimate)))  


reg200_rob_tidy$estimate = ifelse(reg200_rob_tidy$p.value<0.01,paste0(reg200_rob_tidy$estimate,'***') ,
                                  ifelse(reg200_rob_tidy$p.value>0.01 & reg200_rob_tidy$p.value<0.05, paste0(reg200_rob_tidy$estimate,'**') , 
                                         ifelse( reg200_rob_tidy$p.value>0.05 & reg200_rob_tidy$p.value<0.1, paste0(reg200_rob_tidy$estimate,'*'),
                                                 reg200_rob_tidy$estimate)))  

 

regrobout_rob_tidy$estimate = ifelse(regrobout_rob_tidy$p.value<0.01,paste0(regrobout_rob_tidy$estimate,'***') ,
                                     ifelse(regrobout_rob_tidy$p.value>0.01 & regrobout_rob_tidy$p.value<0.05, paste0(regrobout_rob_tidy$estimate,'**') , 
                                            ifelse( regrobout_rob_tidy$p.value>0.05 & regrobout_rob_tidy$p.value<0.1, paste0(regrobout_rob_tidy$estimate,'*'),
                                                    regrobout_rob_tidy$estimate)))  
# Do table output

reg_out1 <- data.frame(variables = c('1(Bike-Station Within 400m)','','1(Bike-Station Within 200m)','','\\#(Bike-Stations Between 1200m and 3200m)','','\\#(Bike-Share Between 1000m and 2000m)','','\\#(Bike-Stations Between 1200m and 3200m)*1(Bike-Share Within 400m))','','\\#(Bike-Stations Between 1200m and 3200m)*1(Bike-Share Within 200m))','','\\#(Bike-Share Between 1000m and 2000m)*1(Bike-Share Within 400m)','','Day FE','Station FE','Controls','N'),
                       main = c(regmain_tidy$estimate[[1]],paste0('(',regmain_tidy$std.error[1],')'),  '','', regmain_tidy$estimate[[2]],paste0('(',regmain_tidy$std.error[2],')'),'','', regmain_tidy$estimate[[3]],paste0('(',regmain_tidy$std.error[3],')'),'','','','','Yes','Yes','No',nobs(regmain)),
                       main200 = c( '','', reg200_tidy$estimate[[1]],paste0('(',reg200_tidy$std.error[1],')'),   reg200_tidy$estimate[[2]],paste0('(',reg200_tidy$std.error[2],')'),'','','','', reg200_tidy$estimate[[3]],paste0('(',reg200_tidy$std.error[3],')'),'','','Yes','Yes','No',nobs(reg200)),
                       robout = c(  regrobout_tidy$estimate[[1]],paste0('(',regrobout_tidy$std.error[1],')'),'','','','',   regrobout_tidy$estimate[[2]],paste0('(',regrobout_tidy$std.error[2],')'),'','','','', regrobout_tidy$estimate[[3]],paste0('(',regrobout_tidy$std.error[3],')'),'Yes','Yes','No',nobs(regrobout)),
                       maincon = c(regmain_rob_tidy$estimate[[1]],paste0('(',regmain_rob_tidy$std.error[1],')'),  '','', regmain_rob_tidy$estimate[[2]],paste0('(',regmain_rob_tidy$std.error[2],')'),'','', regmain_rob_tidy$estimate[[3]],paste0('(',regmain_rob_tidy$std.error[3],')'),'','','','','Yes','Yes','Yes',nobs(regmain_rob)),
                       main200con = c( '','', reg200_rob_tidy$estimate[[1]],paste0('(',reg200_rob_tidy$std.error[1],')'),   reg200_rob_tidy$estimate[[2]],paste0('(',reg200_rob_tidy$std.error[2],')'),'','','','', reg200_rob_tidy$estimate[[3]],paste0('(',reg200_rob_tidy$std.error[3],')'),'','','Yes','Yes','Yes',nobs(reg200_rob)),
                       roboutcon = c(  regrobout_rob_tidy$estimate[[1]],paste0('(',regrobout_rob_tidy$std.error[1],')'),'','','','',   regrobout_rob_tidy$estimate[[2]],paste0('(',regrobout_rob_tidy$std.error[2],')'),'','','','', regrobout_rob_tidy$estimate[[3]],paste0('(',regrobout_rob_tidy$std.error[3],')'),'Yes','Yes','Yes',nobs(regrobout_rob))
)                      

names(reg_out1) <- c('','(1)','(2)','(3)','(4)','(5)','(6)')                                                                   
## REPLICATION
sink(file = "Output/regbase_robust.tex")

tab_reg1 <- kbl(reg_out1, booktabs = T,format = 'latex',row.names = FALSE, linesep = '',escape=F , align =c('lccccccc'))  %>%
  kable_styling(latex_options = c("repeat_header","scale_down")) %>%
  row_spec(c(14),hline_after = T)%>%
  #row_spec(c(30,34),hline_after = T) %>%
  kable_styling(latex_options = "hold_position") %>% 
  column_spec (1,border_left = F, border_right = T) 

substr(tab_reg1,103,nchar(tab_reg1)-13)


sink(file = NULL)


