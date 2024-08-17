#################################################################
##                     PRE-TRENDS ANALYSIS                     ##
#################################################################

rm(list=ls())

library(sf)
library(dplyr)
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

setwd('G:/My Drive/Transport Mode Choice/CDMX')
# setwd('E:/My Drive/Transport Mode Choice/CDMX')
source("Code/remove_accents.R")

subway_f = readRDS('Data/Final/final_daily.rds')

days = seq(as.Date('2014-01-01'),as.Date('2019-12-01'),by='1 day')

subway_f = subway_f %>% filter(fecha %in% days)

###### Create database
subway_f = subway_f %>%
  group_by(month,estacion_id) %>%
  mutate(
    pases_m = sum(pases,na.rm=T),
    days = n()
  )

subway_f$log_pases_m = log(subway_f$pases_m)

subway_f$idx_m = paste0(subway_f$estacion_id,' - ',subway_f$month)

subway_m = subway_f[!duplicated(subway_f$idx_m),]


subway_m$mes = substr(subway_m$month , nchar(subway_m$month)-1,nchar(subway_m$month))




#### GET PRE AND POST PERIODS OF TREATMENT, FIRST FOR THE MONTH-FREQUENCY DATABSE

# Do two cumsums because some stations opened and then closed in the following month. Therefore, by doing a cumsum of a cumsum we keep only the first time that a station opened. THIS IS BECAUSE I WANT TO GET IN WHICH MONTH THEY WERE ORIGINALLY TREATED

subway_m = subway_m %>%
  group_by(estacion_id) %>%
  arrange(fecha) %>%
  mutate(
    csbin_0_400 = cumsum(bin_0_400)
  )

subway_m = subway_m %>%
  group_by(estacion_id) %>%
  arrange(fecha) %>%
  mutate(
    cs2bin_0_400 = cumsum(csbin_0_400)
  )

idx_treat = subway_m %>%
  filter(cs2bin_0_400==1)

# I want to check also if we consider 200 meters as a threshold how many of them were treated before

subway_m = subway_m %>%
  group_by(estacion_id) %>%
  arrange(fecha) %>%
  mutate(
    csbin_0_200 = cumsum(bin_0_200)
  )

subway_m = subway_m %>%
  group_by(estacion_id) %>%
  arrange(fecha) %>%
  mutate(
    cs2bin_0_200 = cumsum(csbin_0_200)
  )

idx_treat200 = subway_m %>%
  filter(cs2bin_0_200==1)

idx_treat200 = idx_treat200[,c('estacion_id','month')] %>% left_join(idx_treat[,c('estacion_id','month')], by = 'estacion_id') 

idx_treat200$fecha200 = as.Date(paste0(idx_treat200$month.x,'-01'))
idx_treat200$fecha400 = as.Date(paste0(idx_treat200$month.y,'-01'))

idx_treat200$treatbefore = idx_treat200$fecha400 <idx_treat200$fecha200 
sum(idx_treat200$treatbefore)


# I continue 


idx_treat = idx_treat[,c('estacion_id','month')]

names(idx_treat)[2] = 'f'

idx_treat$f_m1 = format(as.Date(paste0(idx_treat$f,'-01')) - months(1),'%Y-%m')
idx_treat$f_m2 = format(as.Date(paste0(idx_treat$f,'-01')) - months(2),'%Y-%m')
idx_treat$f_m3 = format(as.Date(paste0(idx_treat$f,'-01')) - months(3),'%Y-%m')
idx_treat$f_m4 = format(as.Date(paste0(idx_treat$f,'-01')) - months(4),'%Y-%m')
idx_treat$f_m5 = format(as.Date(paste0(idx_treat$f,'-01')) - months(5),'%Y-%m')
idx_treat$f_m6 = format(as.Date(paste0(idx_treat$f,'-01')) - months(6),'%Y-%m')
idx_treat$f_m7 = format(as.Date(paste0(idx_treat$f,'-01')) - months(7),'%Y-%m')
idx_treat$f_m8 = format(as.Date(paste0(idx_treat$f,'-01')) - months(8),'%Y-%m')
idx_treat$f_m9 = format(as.Date(paste0(idx_treat$f,'-01')) - months(9),'%Y-%m')

idx_treat$f_p1 = format(as.Date(paste0(idx_treat$f,'-01')) + months(1),'%Y-%m')
idx_treat$f_p2 = format(as.Date(paste0(idx_treat$f,'-01')) + months(2),'%Y-%m')
idx_treat$f_p3 = format(as.Date(paste0(idx_treat$f,'-01')) + months(3),'%Y-%m')
idx_treat$f_p4 = format(as.Date(paste0(idx_treat$f,'-01')) + months(4),'%Y-%m')
idx_treat$f_p5 = format(as.Date(paste0(idx_treat$f,'-01')) + months(5),'%Y-%m')
idx_treat$f_p6 = format(as.Date(paste0(idx_treat$f,'-01')) + months(6),'%Y-%m')
idx_treat$f_p7 = format(as.Date(paste0(idx_treat$f,'-01')) + months(7),'%Y-%m')
idx_treat$f_p8 = format(as.Date(paste0(idx_treat$f,'-01')) + months(8),'%Y-%m')
idx_treat$f_p9 = format(as.Date(paste0(idx_treat$f,'-01')) + months(9),'%Y-%m')



#### Repeat the same but for the daily level database 

subway_f = subway_f %>% left_join(idx_treat,by='estacion_id')

subway_f$t = ifelse(subway_f$f == subway_f$month , '1' ,'0' )

subway_f$t_m1 = ifelse(subway_f$f_m1 == subway_f$month , '1' ,'0' )
subway_f$t_m2 = ifelse(subway_f$f_m2 == subway_f$month , '1' ,'0' )
subway_f$t_m3 = ifelse(subway_f$f_m3 == subway_f$month , '1' ,'0' )
subway_f$t_m4 = ifelse(subway_f$f_m4 == subway_f$month , '1' ,'0' )
subway_f$t_m5 = ifelse(subway_f$f_m5 == subway_f$month , '1' ,'0' )
subway_f$t_m6 = ifelse(subway_f$f_m6 == subway_f$month , '1' ,'0' )
subway_f$t_m7 = ifelse(subway_f$f_m7 == subway_f$month , '1' ,'0' )
subway_f$t_m8 = ifelse(subway_f$f_m8 == subway_f$month , '1' ,'0' )
subway_f$t_m9 = ifelse(subway_f$f_m9 == subway_f$month , '1' ,'0' )

subway_f$t_p1 = ifelse(subway_f$f_p1 == subway_f$month , '1' ,'0' )
subway_f$t_p2 = ifelse(subway_f$f_p2 == subway_f$month , '1' ,'0' )
subway_f$t_p3 = ifelse(subway_f$f_p3 == subway_f$month , '1' ,'0' )
subway_f$t_p4 = ifelse(subway_f$f_p4 == subway_f$month , '1' ,'0' )
subway_f$t_p5 = ifelse(subway_f$f_p5 == subway_f$month , '1' ,'0' )
subway_f$t_p6 = ifelse(subway_f$f_p6 == subway_f$month , '1' ,'0' )
subway_f$t_p7 = ifelse(subway_f$f_p7 == subway_f$month , '1' ,'0' )
subway_f$t_p8 = ifelse(subway_f$f_p8 == subway_f$month , '1' ,'0' )
subway_f$t_p9 = ifelse(subway_f$f_p9 == subway_f$month , '1' ,'0' )

# # THOSE WITH NA REPLACE ALL WITH 0
# 
# idx_cols = which(names(subway_f) %in% c('t_m1','t_m2','t_m3','t_m4','t_m5','t_m6','t_m7','t_m8','t_m9','t_p1','t_p2','t_p3','t_p4','t_p5','t_p6','t_p7','t_p8','t_p9','t'))
#       
# columns_to_replace = c('t_m1','t_m2','t_m3','t_m4','t_m5','t_m6','t_m7','t_m8','t_m9','t_p1','t_p2','t_p3','t_p4','t_p5','t_p6','t_p7','t_p8','t_p9','t')
# 
# subway_f[,columns_to_replace] = lapply(subway_f[,columns_to_replace], function(x) {
#   x[is.na(x)] <- 0
#   return(x)
# })

      

# Add binary that goes from -Inf to -6 amd from 6 to +Inf (and 9)
# This i do just to add a value and do not receibe an error below, but in the end all observations are going to be dropped in the regression since they will have a NA in the treatment variables
subway_f$f_m6 = ifelse(is.na(subway_f$f_m6),'2000-01', subway_f$f_m6)
subway_f$f_m9 = ifelse(is.na(subway_f$f_m9),'2000-01', subway_f$f_m9)
# 
subway_f$f_p6 = ifelse(is.na(subway_f$f_p6),'2030-01', subway_f$f_p6)
subway_f$f_p9 = ifelse(is.na(subway_f$f_p9),'2030-01', subway_f$f_p9)



subway_f$t_mm6 = ifelse(as.Date(paste0(subway_f$f_m6,'-01')) > as.Date(paste0(subway_f$month,'-01')) , '1' ,'0' )
subway_f$t_mm9 = ifelse(as.Date(paste0(subway_f$f_m9,'-01')) > as.Date(paste0(subway_f$month,'-01')) , '1' ,'0' )

subway_f$t_pp6 = ifelse(as.Date(paste0(subway_f$f_p6,'-01')) < as.Date(paste0(subway_f$month,'-01')) , '1' ,'0' )
subway_f$t_pp9 = ifelse(as.Date(paste0(subway_f$f_p9,'-01')) < as.Date(paste0(subway_f$month,'-01')) , '1' ,'0' )

### As in the main regressions, I get which are the stations that are wither 'always' treated or 'never' treated

# origstat =  c(subway_f$estacion_id[subway_f$fecha == min(subway_f$fecha) &subway_f$bin_0_400>0],subway_f$estacion_id[subway_f$fecha == max(subway_f$fecha) &subway_f$bin_0_400==0]) 

origstat =  c(subway_f$estacion_id[subway_f$fecha == min(subway_f$fecha) &subway_f$bin_0_400>0]) 


######### PRE-TRENDS ANALYSIS AT THE DAILY LEVEL

# reg_pret = lm(log_pases ~    t_m2+ t_m3+ t_m4+ t_m5+ t_m6  + t_p1+ t_p2+ t_p3+ t_p4+ t_p5+ t_p6  + t_pp6 + t +t_m1 + t_mm6  + as.factor(estacion_id) + as.factor(month), data =  subway_f[  !(subway_f$estacion_id%in% origstat) ,] )

reg_pret = lm(log_pases ~  t_m1 + t_m2+ t_m3+ t_m4+ t_m5+ t_m6 + t_m7 + t_m8 + t_m9 + t  + t_p1+ t_p2+ t_p3+ t_p4+ t_p5+ t_p6 + t_p7 + t_p8 + t_p9  + t_pp9   + t_mm9 + as.factor(estacion_id) + as.factor(fecha)  + n_1200_3200 + inter_1200_3200_2, data =  subway_f[  !(subway_f$estacion_id%in% origstat) ,] )


reg_pret_clustered = coeftest(reg_pret,
                              vcov = vcovCL,
                              cluster = ~estacion_id)


saveRDS(reg_pret_clustered , 'Data/Regs/reg_pret_clustered.rds')
# reg_pret_clustered = readRDS('Data/Regs/reg_pret_clustered.rds')

reg_pret_clustered = readRDS('Data/Regs/reg_pret_clustered.rds')
# reg_pret_tidy = tidy(reg_pret_clustered) %>% 
#   filter(term %in% c( 't_m11'  , 't_m21' , 't_m31', 't_m41', 't_m51', 't_m61' , 't_p11', 't_p21', 't_p31', 't_p41', 't_p51', 't_p61'  , 't_mm61' , 't_pp61' , 't1'))


reg_pret_tidy = tidy(reg_pret_clustered) %>% 
  filter(term %in% c( 't_m11'  , 't_m21' , 't_m31', 't_m41', 't_m51', 't_m61', 't_m71', 't_m81', 't_m91' , 't_p11', 't_p21', 't_p31', 't_p41', 't_p51', 't_p61'  , 't_p71', 't_p81', 't_p91', 't_mm91' , 't_pp91' , 't1'))



tidy_pret <- tidy(reg_pret_clustered, conf.int = T)


tidy_pret = tidy_pret[2:21,]
# tidy_pret = rbind(tidy_pret[c(rev(1:9)),],c('t',0,0,0,0,0,0),tidy_pret[12,] ,tidy_pret[11:20,])
tidy_pret = rbind(c('t_mm9',0,0,0,0,0,0) ,tidy_pret[c(rev(1:9)),],tidy_pret[10:20,])


tidy_pret$term = c('T<-9','T-9','T-8','T-7','T-6','T-5','T-4','T-3','T-2','T-1','T','T+1','T+2','T+3','T+4','T+5','T+6','T+7','T+8','T+9','T>+9')
tidy_pret$idx = 1:21
tidy_pret$term = as.factor(tidy_pret$term)

tidy_pret[1,6:7]  = NA

tidy_pret$estimate = as.numeric(tidy_pret$estimate)
tidy_pret$conf.low = as.numeric(tidy_pret$conf.low)
tidy_pret$conf.high = as.numeric(tidy_pret$conf.high)

p = tidy_pret %>%
  mutate(term = fct_reorder(term, idx))  %>%
  ggplot(aes(term,estimate)) +
  geom_point() +
  scale_y_continuous(breaks = c(-0.05,0,0.05,0.1,0.15,0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  # add in a dotted line at zero
  geom_hline(yintercept = 0, lty = 2) +
  labs(
    x = "Period",
    y = NULL
  ) +
  theme_classic()+
  theme(axis.text=element_text(size=7))+
   theme( axis.title=element_text(size=14))
p

ggsave('Output/pretrend9_cdmx.png',p)




## Robustness including never-treated variables ####


subway_f2 =data.frame( subway_f)

vlabs = c( 't_m1'  , 't_m2' , 't_m3', 't_m4', 't_m5', 't_m6', 't_m7', 't_m8', 't_m9' , 't_mm9' , 't_p1', 't_p2', 't_p3', 't_p4', 't_p5', 't_p6'  , 't_p7', 't_p8', 't_p9', 't_mm9' , 't_pp9' , 't')


subway_f2 <- subway_f2 %>%
  mutate_at(vars(one_of(vlabs)), ~ replace(., is.na(.), 0))


reg_pret2 = lm(log_pases ~ t_m1 + t_m2+ t_m3+ t_m4+ t_m5+ t_m6 + t_m7 + t_m8 + t_m9  + t_p1+ t_p2+ t_p3+ t_p4+ t_p5+ t_p6 + t_p7 + t_p8 + t_p9  + t_pp9 + t    + as.factor(estacion_id) + as.factor(fecha) + nolab  + n_1200_3200 + inter_1200_3200_2, data =  subway_f2[  !(subway_f2$estacion_id%in% origstat) ,] )



reg_pret2_clustered = coeftest(reg_pret2,
                               vcov = vcovCL,
                               cluster = ~estacion_id)

saveRDS(reg_pret2_clustered,'Data/Regs/reg_pret2_clustered.rds')

reg_pret2_clustered = readRDS('Data/Regs/reg_pret2_clustered.rds')

reg_pret_tidy = tidy(reg_pret2_clustered) %>% 
  filter(term %in% c( 't_m11'  , 't_m21' , 't_m31', 't_m41', 't_m51', 't_m61', 't_m71', 't_m81', 't_m91'  , 't_p11', 't_p21', 't_p31', 't_p41', 't_p51', 't_p61'  , 't_p71', 't_p81', 't_p91', 't_mm91' , 't_pp91' , 't1'))



tidy_pret <- tidy(reg_pret2_clustered, conf.int = T)


tidy_pret = tidy_pret[2:21,]
tidy_pret = rbind(tidy_pret[c(rev(1:9)),],tidy_pret[20,], tidy_pret[10:19,])

tidy_pret$term = c('T-9','T-8','T-7','T-6','T-5','T-4','T-3','T-2','T-1','T','T+1','T+2','T+3','T+4','T+5','T+6','T+7','T+8','T+9','T>+9')
tidy_pret$idx = 1:20
tidy_pret$term = as.factor(tidy_pret$term)

# tidy_pret[11,6:7]  = NA

tidy_pret$estimate = as.numeric(tidy_pret$estimate)
tidy_pret$conf.low = as.numeric(tidy_pret$conf.low)
tidy_pret$conf.high = as.numeric(tidy_pret$conf.high)

p = tidy_pret %>%
  mutate(term = fct_reorder(term, idx))  %>%
  ggplot(aes(term,estimate)) +
  geom_point() +
  scale_y_continuous(breaks = c(-0.05,0,0.05,0.1,0.15,0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  # add in a dotted line at zero
  geom_hline(yintercept = 0, lty = 2) +
  labs(
    x = "Period",
    y = NULL
  ) +
  theme_classic()+
  theme(axis.text=element_text(size=15))

p


ggsave('Output/pretrend9_cdmx_fulls.png',p,height = 8 , width = 10 , dpi=200)





























############## WHAT'S BELOW IS WHAT WE SHOULD DO FOR HAVING PRE-POST 6 MONTHS AS TRENDS

######### PRE-TRENDS ANALYSIS AT THE DAILY LEVEL

reg_pret = lm(log_pases ~    t_m2+ t_m3+ t_m4+ t_m5+ t_m6  + t_p1+ t_p2+ t_p3+ t_p4+ t_p5+ t_p6  + t_pp6 + t +t_m1 + t_mm6  + as.factor(estacion_id) + as.factor(month), data =  subway_f[  !(subway_f$estacion_id%in% origstat) ,] )


reg_pret_clustered = coeftest(reg_pret,
                              vcov = vcovCL,
                              cluster = ~estacion_id)

reg_pret_tidy = tidy(reg_pret_clustered) %>% 
  filter(term %in% c( 't_m11'  , 't_m21' , 't_m31', 't_m41', 't_m51', 't_m61' , 't_p11', 't_p21', 't_p31', 't_p41', 't_p51', 't_p61'  , 't_mm61' , 't_pp61' , 't1'))


tidy_pret <- tidy(reg_pret_clustered, conf.int = T)


tidy_pret = tidy_pret[2:15,]
tidy_pret = rbind(c('t_mm61',0,0,0,0,0,0),tidy_pret[c(rev(1:5)),],tidy_pret[c(14,13),] ,tidy_pret[6:12,])

tidy_pret$term = c('T<-6','T-6','T-5','T-4','T-3','T-2','T-1','T','T+1','T+2','T+3','T+4','T+5','T+6','T>+6')
tidy_pret$idx = 1:15
tidy_pret$term = as.factor(tidy_pret$term)

tidy_pret[1,6:7]  = NA

tidy_pret$estimate = as.numeric(tidy_pret$estimate)
tidy_pret$conf.low = as.numeric(tidy_pret$conf.low)
tidy_pret$conf.high = as.numeric(tidy_pret$conf.high)

p = tidy_pret %>%
  mutate(term = fct_reorder(term, idx))  %>%
  ggplot(aes(term,estimate)) +
  geom_point() +
  scale_y_continuous(breaks = c(-0.05,0,0.05,0.1,0.15,0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  # add in a dotted line at zero
  geom_hline(yintercept = 0, lty = 2) +
  labs(
    x = "Period",
    y = NULL
  ) +
  theme_classic()

p

ggsave('Output/pretrend6.png',p)



