


#################################################################
##                        SYSTEM BREAKS                        ##
#################################################################


rm(list=ls())

library(dplyr)
library(kableExtra)

setwd('E:/My Drive/Transport Mode Choice')


loadfiles = list.files('Data Argentina/Availability Bikes')[-1]

fulldata = data.frame(Date = as.character(0) , Mean = as.numeric(0),  SD = as.numeric(0), Q1 = as.numeric(0) ,Q2 = as.numeric(0),Q3 = as.numeric(0), NFull = as.numeric(0), NEmpty = as.numeric(0), Total = as.numeric(0))


for(file in loadfiles[-1]){
  
  data = readRDS(paste0('Data Argentina/Availability Bikes/', file))
  
  data$num_bikes_available = as.numeric(data$num_bikes_available)
  data$num_bikes_disabled = as.numeric(data$num_bikes_disabled)
  data$num_docks_available = as.numeric(data$num_docks_available)
  data$num_docks_disabled = as.numeric(data$num_docks_disabled)
  
  data$availability = (data$num_bikes_available)/(data$num_bikes_available + data$num_docks_available - data$num_docks_disabled)
  
  stats = summary(data$availability)
  
  vecstat = c(substr(file,13,28), stats[4],sd(data$availability,na.rm = T) ,stats[2] ,stats[3],stats[5] , sum(data$availability==1,na.rm=T), sum(data$availability==0,na.rm=T) , sum(!is.na(data$availability)))
  
  fulldata = rbind(fulldata,vecstat)
  
}

fulldata = fulldata[8:nrow(fulldata), ]

fulldata$Date = as.numeric(substr(fulldata$Date ,12,13))+4
names(fulldata)[1] = 'Hour'

fulldata = data.frame(sapply(fulldata ,as.numeric))

fulldata$ShareFull = fulldata$NFull/fulldata$Total
fulldata$ShareEmpty = fulldata$NEmpty/fulldata$Total

fulldata$NFull= fulldata$Total = fulldata$NEmpty = NULL


fulldata = round(fulldata,2)


sink('Output/shortages.tex')

 printabla = kbl(fulldata, booktabs = T,format = 'latex',row.names = FALSE, linesep = '',escape=F)  %>%
  kable_styling(latex_options = c("repeat_header","scale_down")) %>%
  column_spec(c(1),border_right = T)%>%
  #row_spec(c(30,34),hline_after = T) %>%
  kable_styling(latex_options = "hold_position") 

 substr(printabla,103,nchar(printabla)-13)
 
 sink()
 