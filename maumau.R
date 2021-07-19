setwd("~")
rm(list=ls())


##################################
# 
Mapas de calor Tamaulipas     #
# Aritzy Sanchez                 #
# 11/09/2019                     #
##################################

#install.packages("pacman")
#p_load (ggplot, tidyverse, plyr, dplyr, reshape)
library(pacman)
p_load(sp, rgdal, tidyverse,maptools)

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
options(scipen=999)

##########################################################

# abrir base agregada sesnsp
setwd("/Users/HP/Downloads")

datos <- read_csv("201401-CIFA-NuevoLaredo OCT19-13ENE20.csv")

## elshapefile de agebs viene divididio en urbana y rural
ageb_urbana <- readOGR("tamps/tamps_ageb_urbana.shp",stringsAsFactors = F)
ageb_rural <- readOGR("tamps/tamps_ageb_rural.shp",stringsAsFactors = F)



ageb_rural@data$CVE_AGEB <- paste0(ageb_rural@data$CVE_AGEB,'_rural')
ageb_urbana@data$CVE_AGEB <- paste0(ageb_urbana@data$CVE_AGEB,'_urbana')

ageb_rural@data$CVE_LOC <- NA

carto <- rbind(ageb_rural,ageb_urbana, makeUniqueIDs = TRUE)


## proyeccion del shape de agebs
proj4string(carto)


### declarar objeto de puntos spaciales

## pasar coordenadas a dos variables numericas
datos <- datos %>% separate(Coordenadas,into = c("lat","lon"), sep = ",")
datos$lat <- as.numeric(datos$lat)
datos$lon <- as.numeric(datos$lon)



puntos_espaciales <- SpatialPointsDataFrame(as.matrix(datos[,c("lon","lat")]), data = datos, proj4string = CRS(proj4string(carto)))


## visualización express

plot(carto)
plot(puntos_espaciales, add = TRUE, col = "red")





###### over
datos <- cbind(datos,over(puntos_espaciales,carto))

datos_agrupados <- datos %>% group_by(CVE_AGEB) %>% 
  summarise(total_casos = n())



#### fortify


# si no jala el fortify correr primero:
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()


cartoF <- fortify(carto,region = "CVE_AGEB")


### exportar

writeOGR(carto,"carto_tam.shp",driver = "ESRI Shapefile", layer = "CVE_AGEB")
writeOGR(puntos_espaciales,"puntos_tam.shp",driver = "ESRI Shapefile", layer = "puntos")


































  
  