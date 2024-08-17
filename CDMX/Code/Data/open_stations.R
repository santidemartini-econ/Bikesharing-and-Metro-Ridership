

#################################################################
##                        OPENING BIKES                        ##
#################################################################

library(tidyr)

rm(list=ls())

setwd('G:/My Drive/Transport Mode Choice/CDMX')


months = format(seq(as.Date('2010-02-01'),as.Date('2019-12-01'),by='1 month'),'%Y-%m')

# Read location

metro = read.csv('Data/Geo/estaciones_ecobici_sist_anterior.csv')

open = data.frame(matrix(nrow=nrow(metro),ncol = length(months)))

# Loop over files on rides made to retrieve if they were open at a given date

estac = c()
mes = c()

for(m in months){
  i = which( months == m)
  
  temp = read.csv(paste0('Data/Trips/',m,'.csv'))
  stats = unique(temp$Ciclo_Estacion_Retiro)
  open[1:length(stats),i] = stats
  print(m)

  estac = c(estac,open[!is.na(open[,i]),i])
  mes = c(mes , rep(m,length(open[!is.na(open[,i]),i])))
}


# for(m in months){
#   estac = c(estac,open[!is.na(open[,i]),i])
#   mes = c(mes , rep(m,length(open[!is.na(open[,i]),i])))
# }



open_l = data.frame(cbind(estac,mes))

idx_t = which(!duplicated(open_l$estac))
open_l_cl = open_l[idx_t,]

saveRDS(open,'Data/Final/open_raw.rds')
saveRDS(open_l_cl,'Data/Final/open_stats.rds')
