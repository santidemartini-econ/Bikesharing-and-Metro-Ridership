

#################################################################
##                            CLIMA                            ##
#################################################################

rm(list=ls())

clima = read.csv("G:/My Drive/Transport Mode Choice/Argentina/Data Argentina/Clima/export.csv")

clima = clima[,c('date','tavg')]

saveRDS(clima,"G:/My Drive/Transport Mode Choice/Argentina/Data Argentina/Clima/clima.rds")

