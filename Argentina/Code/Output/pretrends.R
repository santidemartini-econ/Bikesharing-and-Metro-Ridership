
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

setwd('G:/My Drive/Transport Mode Choice/Argentina')
# setwd('E:/My Drive/Transport Mode Choice/Argentina')

source("Code/remove_accents.R")

subway_f = readRDS('Data Argentina/Final/final_daily.rds')
subway_m = readRDS('Data Argentina/Final/final_monthly.rds')

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

idx_treat = idx_treat[,c('estacion_id','month')]

# Get treatment period and before and after treatment months
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

# Merge with database
subway_m = subway_m %>% left_join(idx_treat,by='estacion_id')

# Create binary variables
subway_m$t = ifelse(subway_m$f == subway_m$month , '1' ,'0' )

subway_m$t_m1 = ifelse(subway_m$f_m1 == subway_m$month , '1' ,'0' )
subway_m$t_m2 = ifelse(subway_m$f_m2 == subway_m$month , '1' ,'0' )
subway_m$t_m3 = ifelse(subway_m$f_m3 == subway_m$month , '1' ,'0' )
subway_m$t_m4 = ifelse(subway_m$f_m4 == subway_m$month , '1' ,'0' )
subway_m$t_m5 = ifelse(subway_m$f_m5 == subway_m$month , '1' ,'0' )
subway_m$t_m6 = ifelse(subway_m$f_m6 == subway_m$month , '1' ,'0' )
subway_m$t_m7 = ifelse(subway_m$f_m6 == subway_m$month , '1' ,'0' )
subway_m$t_m8 = ifelse(subway_m$f_m6 == subway_m$month , '1' ,'0' )
subway_m$t_m9 = ifelse(subway_m$f_m6 == subway_m$month , '1' ,'0' )

subway_m$t_p1 = ifelse(subway_m$f_p1 == subway_m$month , '1' ,'0' )
subway_m$t_p2 = ifelse(subway_m$f_p2 == subway_m$month , '1' ,'0' )
subway_m$t_p3 = ifelse(subway_m$f_p3 == subway_m$month , '1' ,'0' )
subway_m$t_p4 = ifelse(subway_m$f_p4 == subway_m$month , '1' ,'0' )
subway_m$t_p5 = ifelse(subway_m$f_p5 == subway_m$month , '1' ,'0' )
subway_m$t_p6 = ifelse(subway_m$f_p6 == subway_m$month , '1' ,'0' )
subway_m$t_p7 = ifelse(subway_m$f_p6 == subway_m$month , '1' ,'0' )
subway_m$t_p8 = ifelse(subway_m$f_p6 == subway_m$month , '1' ,'0' )
subway_m$t_p9 = ifelse(subway_m$f_p6 == subway_m$month , '1' ,'0' )

# Binary for treatment and pre-6 and post-6 periods (and 9)
subway_m$t_mm6 = ifelse(as.Date(paste0(subway_m$f_m6,'-01')) > as.Date(paste0(subway_m$month,'-01')) , '1' ,'0' )
subway_m$t_mm9 = ifelse(as.Date(paste0(subway_m$f_m9,'-01')) > as.Date(paste0(subway_m$month,'-01')) , '1' ,'0' )

subway_m$t_pp9 = ifelse(as.Date(paste0(subway_m$f_p9,'-01')) < as.Date(paste0(subway_m$month,'-01')) , '1' ,'0' )


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

# Add binary that goes from -Inf to -6 amd from 6 to +Inf (and 9)

subway_f$t_mm6 = ifelse(as.Date(paste0(subway_f$f_m6,'-01')) > as.Date(paste0(subway_f$month,'-01')) , '1' ,'0' )
subway_f$t_mm9 = ifelse(as.Date(paste0(subway_f$f_m9,'-01')) > as.Date(paste0(subway_f$month,'-01')) , '1' ,'0' )

subway_f$t_pp6 = ifelse(as.Date(paste0(subway_f$f_p6,'-01')) < as.Date(paste0(subway_f$month,'-01')) , '1' ,'0' )
subway_f$t_pp9 = ifelse(as.Date(paste0(subway_f$f_p9,'-01')) < as.Date(paste0(subway_f$month,'-01')) , '1' ,'0' )

### As in the main regressions, I get which are the stations that are wither 'always' treated or 'never' treated

# origstat =  c(subway_f$estacion_id[subway_f$fecha == min(subway_f$fecha) &subway_f$bin_0_400>0],subway_f$estacion_id[subway_f$fecha == max(subway_f$fecha) &subway_f$bin_0_400==0]) 

origstat =  c(subway_f$estacion_id[subway_f$fecha == min(subway_f$fecha) &subway_f$bin_0_400>0]) 




########### THIS IS AN ALTERNATIVE VERSION WHERE I INCLUDE THE <-9 COEFFICIENT AND OMIT THE T-1 ONE. I USE THIS TO CHECK IF THERE WAS A TREATMENT EFFECT

subway_f2 =data.frame( subway_f)

vlabs = c( 't_m1'  , 't_m2' , 't_m3', 't_m4', 't_m5', 't_m6', 't_m7', 't_m8', 't_m9' , 't_mm9' , 't_p1', 't_p2', 't_p3', 't_p4', 't_p5', 't_p6'  , 't_p7', 't_p8', 't_p9',  't_pp9' , 't')


subway_f2 <- subway_f2 %>%
  mutate_at(vars(one_of(vlabs)), ~ replace(., is.na(.), 0))


reg_pret3 = lm(log_pases ~ t_mm9 + t_m9  +  t_m8 +  t_m7 + t_m6+ t_m5+ t_m4+ t_m3+ t_m2  + t + t_p1+ t_p2+ t_p3+ t_p4+ t_p5+ t_p6 + t_p7 + t_p8 + t_p9  + t_pp9   + as.factor(estacion_id) + as.factor(fecha)   + n_1200_3200 + inter_1200_3200  + age + male + yrschool + share_work, data =  subway_f2[  !(subway_f2$estacion_id%in% origstat) ,] )



reg_pret3_clustered = coeftest(reg_pret3,
                               vcov = vcovCL,
                               cluster = ~estacion_id)

saveRDS(reg_pret3,'Data Argentina/Regs/reg_pret3.rds')

saveRDS(reg_pret3_clustered,'Data Argentina/Regs/reg_pret3_clustered.rds')

# reg_pret2_clustered = readRDS('Data Argentina/Regs/reg_pret2_clustered.rds')

reg_pret_tidy = tidy(reg_pret3_clustered) %>% 
  filter(term %in% c( 't_m11'  , 't_m21' , 't_m31', 't_m41', 't_m51', 't_m61', 't_m71', 't_m81', 't_m91'  , 't_p11', 't_p21', 't_p31', 't_p41', 't_p51', 't_p61'  , 't_p71', 't_p81', 't_p91', 't_mm91' , 't_pp91' , 't1'))



tidy_pret <- tidy(reg_pret3_clustered, conf.int = T)


tidy_pret = tidy_pret[2:21,]
tidy_pret  = rbind(tidy_pret[1:9,],c('t-1','0','0','0','0','0','0'), tidy_pret[10:20,])

tidy_pret[10,6:7] = NA

tidy_pret$term = c('T<-9','T-9','T-8','T-7','T-6','T-5','T-4','T-3','T-2','T-1','T','T+1','T+2','T+3','T+4','T+5','T+6','T+7','T+8','T+9','T>+9')
tidy_pret$idx = 1:21
tidy_pret$term = as.factor(tidy_pret$term)

# tidy_pret[11,6:7]  = NA

tidy_pret$estimate = as.numeric(tidy_pret$estimate)
tidy_pret$conf.low = as.numeric(tidy_pret$conf.low)
tidy_pret$conf.high = as.numeric(tidy_pret$conf.high)

p = tidy_pret %>%
  mutate(term = fct_reorder(term, idx))  %>%
  ggplot(aes(term,estimate)) +
  geom_point() +
  scale_y_continuous(breaks = c(-0.05,0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  # add in a dotted line at zero
  geom_hline(yintercept = 0, lty = 2) +
  labs(
    x = "Period",
    y = NULL
  ) +
  theme_classic()+
  theme(axis.text=element_text(size=14)) 

p


ggsave('Output/pretrend9_fulls_treat.png',p,height = 8 , width = 10 , dpi=200)





#### 0utput in table-form

tidy_pret$estimate = format(round(as.numeric(tidy_pret$estimate),3),nsmall=3)
tidy_pret$std.error = format(round(as.numeric(tidy_pret$std.error),3),nsmall=3)


tidy_pret$estimate = ifelse(tidy_pret$p.value<0.01,paste0(tidy_pret$estimate,'***') ,
                            ifelse(tidy_pret$p.value>0.01 & tidy_pret$p.value<0.05, paste0(tidy_pret$estimate,'**') , 
                                   ifelse( tidy_pret$p.value>0.05 & tidy_pret$p.value<0.1, paste0(tidy_pret$estimate,'*'),
                                           tidy_pret$estimate)))  


# Do table output


reg_out1 <- data.frame(variables = c('T<-9','','T-9','','T-8','','T-7','','T-6','','T-5','','T-4','','T-3','','T-2','','T-1','','T','','T+1','','T+2','','T+3','','T+4','','T+5','','T+6','','T+7','','T+8','','T+9','','T>+9','','Month FE','Station FE','N'),
                       main = c(  tidy_pret$estimate[[1]],paste0('(',tidy_pret$std.error[1],')'),
                                  tidy_pret$estimate[[2]],paste0('(',tidy_pret$std.error[2],')'),
                                  tidy_pret$estimate[[3]],paste0('(',tidy_pret$std.error[3],')'),
                                  tidy_pret$estimate[[4]],paste0('(',tidy_pret$std.error[4],')'),
                                  tidy_pret$estimate[[5]],paste0('(',tidy_pret$std.error[5],')'),
                                  tidy_pret$estimate[[6]],paste0('(',tidy_pret$std.error[6],')'),
                                  tidy_pret$estimate[[7]],paste0('(',tidy_pret$std.error[7],')'),
                                  tidy_pret$estimate[[8]],paste0('(',tidy_pret$std.error[8],')'),
                                  tidy_pret$estimate[[9]],paste0('(',tidy_pret$std.error[9],')'),
                                  tidy_pret$estimate[[10]],paste0('(',tidy_pret$std.error[10],')'),
                                  tidy_pret$estimate[[11]],paste0('(',tidy_pret$std.error[11],')'),
                                  tidy_pret$estimate[[12]],paste0('(',tidy_pret$std.error[12],')'),
                                  tidy_pret$estimate[[13]],paste0('(',tidy_pret$std.error[13],')'),
                                  tidy_pret$estimate[[14]],paste0('(',tidy_pret$std.error[14],')'),
                                  tidy_pret$estimate[[15]],paste0('(',tidy_pret$std.error[15],')'),
                                  tidy_pret$estimate[[16]],paste0('(',tidy_pret$std.error[16],')'),
                                  tidy_pret$estimate[[17]],paste0('(',tidy_pret$std.error[17],')'),
                                  tidy_pret$estimate[[18]],paste0('(',tidy_pret$std.error[18],')'),
                                  tidy_pret$estimate[[19]],paste0('(',tidy_pret$std.error[19],')'),
                                  tidy_pret$estimate[[20]],paste0('(',tidy_pret$std.error[20],')'),
                                  tidy_pret$estimate[[21]],paste0('(',tidy_pret$std.error[21],')'),
                                  'Yes','Yes',nobs(reg_pret3))
                       
)      



reg_out1[19:20,2] = 0
names(reg_out1) <- c('','(1)')                                                                   
## REPLICATION
sink(file = "Output/regpret9_fulls_treat.tex")

tab_reg1 <- kbl(reg_out1, booktabs = T,format = 'latex',row.names = FALSE, linesep = '',escape=F , align =c('l', rep('c,1')))  %>%
  kable_styling(latex_options = c("repeat_header","scale_down")) %>%
  row_spec(c(40),hline_after = T)%>%
  #row_spec(c(30,34),hline_after = T) %>%
  kable_styling(latex_options = "hold_position") %>% 
  column_spec (1,border_left = F, border_right = T) 

substr(tab_reg1,103,nchar(tab_reg1)-13)


sink(file = NULL)

