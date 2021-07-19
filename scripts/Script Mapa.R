rm(list=ls())
setwd("~")


##################################
# Procesar Datos Mapas           #
# Aritzy Sanchez                 #
# 06/02/2019                     #
##################################

#install.packages(c("rgeos", "rgdal"))

library(rgeos)
library(rgdal)

dir1 = "/Users/Jorge/Documents/MIM/Mapas"

setwd(dir1)

mapa <- readOGR(dsn="muni_2012gw", stringsAsFactors = F)

mapa$CONCAT <- paste0(mapa$CVE_ENT, mapa$CVE_MUN)

#plot(municipios)

nl <- mapa [mapa$CVE_ENT=="19",]

#nl@data

write.csv(nl@data, file="Test.csv")
write.csv(nl, file="Test2.csv")


oax <- mapa [mapa$CVE_ENT=="20",]

datos.oax <- read.csv(paste(dir1, "OaxacaMpios.csv", sep="/"), stringsAsFactors = F)

datos.oax$CONCAT <- paste0(datos.oax$CVE_ENT, datos.oax$CVE_MUN)

write.csv(datos.oax@data, file="TestOAx.csv")

