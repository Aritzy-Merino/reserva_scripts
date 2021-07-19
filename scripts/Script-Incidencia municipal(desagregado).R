
rm(list=ls())


##################################
# Procesar Datos SNSP            #
#     nivel estatal              #
# Aritzy Sanchez                 #
# 05/12/2018                     #
##################################

library(pacman)
p_load(foreign, readxl, tidyverse, plyr, dplyr, tidyr, reshape,scales)

#dir1 = "/Users/HP/Documents/MIM/Incidencia delictiva"

############# Datos incidencia ########


setwd("/Users/HP/Documents/Rizika/Incidencia delictiva")


datos <- read_csv("Municipal-Delitos-2015-2021_may2021.csv", 
                       locale = locale(encoding = "latin1"))

#Transformamos en character los factores

nombres <- names(datos)[3:7]

datos[, nombres] <- apply(datos[, nombres], 2, function(x) as.character(x))

#Renombrar variables

colnames(datos) <- c("Año","Clave_ent","Entidad","Clave_mun", "Municipio", "Bien jurídico afectado","Tipo","Subtipo","Modalidad",
                     "01","02","03","04","05","06","07","08","09","10","11","12")


#Seleccionamos con las categorias que nos interesan

Nacional <- datos[datos$Tipo=="Homicidio" & datos$Subtipo=="Homicidio doloso" | datos$Tipo== "Secuestro"| 
                    datos$Tipo== "Extorsión"| datos$Tipo== "Robo" & datos$Modalidad=="Robo de coche de 4 ruedas Con violencia" | datos$Tipo== "Lesiones" & datos$Subtipo=="Lesiones dolosas"  |
                    datos$Tipo== "Robo" & datos$Modalidad == "Robo de motocicleta Con violencia" | 
                    datos$Tipo== "Abuso sexual"| 
                    datos$Tipo== "Robo" & datos$Modalidad == "Robo de embarcaciones pequeñas y grandes Con violencia"| 
                    datos$Tipo== "Feminicidio"| datos$Tipo== "Narcomenudeo"|datos$Tipo== "Tráfico de menores"|
                    datos$Tipo== "Violencia familiar"|datos$Tipo== "Trata de personas"|datos$Tipo=="Violencia de género en todas sus modalidades distinta a la violencia familiar"|datos$Subtipo== "Robo a casa habitación" | datos$Subtipo== "Robo a negocio"| datos$Subtipo== "Robo de maquinaria"| 
                    datos$Subtipo== "Robo a transeúnte en via pública" | 
                    datos$Subtipo== "Robo a transeúnte en espacio abierto al público"|
                    datos$Subtipo== "Robo a transportista",]

############# Datos estatales#######

Estatal <- Nacional 

Estatal <-  gather(Estatal, Mes , value=Incidencia, "01":"12")

Estatal$Clave_tiempo <- paste0("01","-",Estatal$Mes,"-",Estatal$Año)

#Selecciono estado de inter?s#

Estatal <- Nacional %>%
  filter(Entidad=="Coahuila de Zaragoza")

##########Homicidio########

Homicidio <- Estatal %>%
  filter(Tipo=="Homicidio")

Homicidio <- gather(Homicidio, Mes , value=Incidencia, "01":"12")

HomicidioEntidad <- aggregate(Incidencia ~ Tipo + Mes + Año , data= Homicidio, sum)

HomicidioEntidad<- spread(HomicidioEntidad, Año, Incidencia)

HomicidioMes <- aggregate(Incidencia ~ Tipo  + Municipio + Mes + Año , data= Homicidio, sum)

HomicidioMes<- spread(HomicidioMes, Año, Incidencia)

##########Secuestro########

Secuestro <- Estatal %>%
  filter(Tipo=="Secuestro")

Secuestro <- gather(Secuestro, Mes , value=Incidencia, "01":"12")

SecuestroEntidad <- aggregate(Incidencia ~ Tipo + Mes + Año , data= Secuestro, sum)

SecuestroEntidad<- spread(SecuestroEntidad, Año, Incidencia)

SecuestroMes <- aggregate(Incidencia ~ Tipo  + Municipio + Mes + Año , data= Secuestro, sum)

SecuestroMes<- spread(SecuestroMes, Año, Incidencia)

##########Extorsión########

Extorsion <- Estatal %>%
  filter(Tipo=="Extorsión")

Extorsion <- gather(Extorsion, Mes , value=Incidencia, "01":"12")

ExtorsionEntidad <- aggregate(Incidencia ~ Tipo + Mes + Año , data= Extorsion, sum)

ExtorsionEntidad<- spread(ExtorsionEntidad, Año, Incidencia)

ExtorsionMes <- aggregate(Incidencia ~ Tipo  + Municipio + Mes + Año , data= Extorsion, sum)

ExtorsionMes<- spread(ExtorsionMes, Año, Incidencia)

##########Robo de veh?culo con violencia########

RVCV <- Estatal %>%
  filter(Subtipo=="Robo de vehículo automotor")

RVCV <- gather(RVCV, Mes , value=Incidencia, "01":"12")

RVCVEntidad <- aggregate(Incidencia ~ Tipo + Mes + Año , data= RVCV, sum)

RVCVEntidad<- spread(RVCVEntidad, Año, Incidencia)

RVCVMes <- aggregate(Incidencia ~ Tipo  + Municipio + Mes + Año , data= RVCV, sum)

RVCVMes<- spread(RVCVMes, Año, Incidencia)

 ######Lesiones dolosas########
Lesiones <- Estatal %>%
  filter(Subtipo=="Lesiones dolosas")

Lesiones <- gather(Lesiones, Mes , value=Incidencia, "01":"12")

LesionesEntidad <- aggregate(Incidencia ~ Tipo + Mes +  Año , data= Lesiones, sum)

LesionesEntidad<- spread(LesionesEntidad, Año, Incidencia)

LesionesMes <- aggregate(Incidencia ~ Tipo  + Municipio + Mes + Año , data= Lesiones, sum)

LesionesMes<- spread(LesionesMes, Año, Incidencia)

############ Feminicidio ##########
Feminicidio <- Nacional %>%
  filter(Tipo=="Feminicidio")

Feminicidio <- gather(Feminicidio, Mes , value=Incidencia, "01":"12")

FeminicidioEntidad <- aggregate(Incidencia ~ Tipo +  Año , data= Feminicidio, sum)

FeminicidioEntidad<- spread(FeminicidioEntidad, Año, Incidencia)

FeminicidioMes <- aggregate(Incidencia ~ Tipo  + Municipio + Mes + Año , data= Feminicidio, sum)

FeminicidioMes<- spread(FeminicidioMes, Año, Incidencia)

############ Narcomenudeo ##########

Narco <- Estatal %>%
  filter(Tipo=="Narcomenudeo")

Narco <- gather(Narco, Mes , value=Incidencia, "01":"12")

NarcoEntidad <- aggregate(Incidencia ~ Tipo +  Mes +Año , data= Narco, sum)

NarcoEntidad<- spread(NarcoEntidad, Año, Incidencia)

NarcoMes <- aggregate(Incidencia ~ Tipo  + Municipio + Mes + Año , data= Narco, sum)

NarcoMes<- spread(NarcoMes, Año, Incidencia)

############ Violencia de g?nero ##########

Vgenero <- Nacional %>%
  filter(Tipo=="Violencia de g?nero en todas sus modalidades distinta a la violencia familiar")

Vgenero <- gather(Vgenero, Mes , value=Incidencia, "01":"12")

VgeneroEntidad <- aggregate(Incidencia ~ Tipo + Año , data= Vgenero, sum)

VgeneroEntidad<- spread(VgeneroEntidad, Año, Incidencia)

VgeneroMes <- aggregate(Incidencia ~ Tipo  + Municipio + Mes + Año , data= Vgenero, sum)

VgeneroMes<- spread(VgeneroMes, Año, Incidencia)

############ Trata de personas ##########

Trata <- Estatal %>%
  filter(Tipo=="Trata de personas")

Trata <- gather(Trata, Mes , value=Incidencia, "01":"12")

TrataAño <- aggregate(Incidencia ~ Municipio + Tipo + Año, data= Trata, sum)

TrataMEs <- aggregate(Incidencia ~ Municipio + Tipo  + Mes + Año , data= Trata, sum)

############ Robo a transportista ##########

RoboT <- Nacional %>%
  filter(Subtipo=="Robo a transportista")

RoboT <- gather(RoboT, Mes , value=Incidencia, "01":"12")

RoboTEntidad <- aggregate(Incidencia ~ Subtipo  + Mes + Año, data= RoboT, sum)

RoboTEntidad<- spread(RoboTEntidad, Año, Incidencia)

write.csv(RoboTEntidad, file = "robo_transportista.csv")

RoboTMes <- aggregate(Incidencia ~ Tipo  + Municipio + Mes + Año , data= RoboT, sum)

RoboTMes<- spread(RoboTMes, Año, Incidencia)


############ Abuso Sexual ##########

Abuso <- Estatal %>%
  filter(Tipo=="Abuso sexual")

Abuso <- gather(Abuso, Mes , value=Incidencia, "01":"12")

Abuso <- aggregate(Incidencia ~ Tipo + Mes + Año, data= Abuso, sum)

Abuso <- spread(Abuso, Año, Incidencia)

Abuso <- aggregate(Incidencia ~ Municipio + Tipo  + Mes + Año , data= Abuso, sum)


#######Delitos patrimoniales######

RoboN <- Estatal %>%
  filter(Subtipo=="Robo a negocio")

RoboN <- gather(RoboN, Mes , value=Incidencia, "01":"12")

RoboNEntidad <- aggregate(Incidencia ~ Subtipo  + Mes + Año   , data= RoboN, sum)

RoboNEntidad<- spread(RoboNEntidad, Año, Incidencia)

write.csv(RoboNEntidad, file = "robo_negocio.csv")

RoboNMes <- aggregate(Incidencia ~ Subtipo + Municipio + Mes +  Año , data= RoboN, sum)

RoboNMes<- spread(RoboNMes, Año, Incidencia)

### Robo de maquinaria

RoboMaq <- Estatal %>%
  filter(Subtipo=="Robo de maquinaria")

RoboMaq <- gather(RoboMaq, Mes , value=Incidencia, "01":"12")

RoboMaqEntidad <- aggregate(Incidencia ~ Subtipo + Mes + Año   , data= RoboMaq, sum)

RoboMaqEntidad<- spread(RoboMaqEntidad, Año, Incidencia)

RoboMaqMes <- aggregate(Incidencia ~ Subtipo + Municipio + Mes +  Año , data= RoboMaq, sum)

RoboMaqMes<- spread(RoboMaqMes, Año, Incidencia)

### Robo a transeúnte

RoboTrans <- Estatal %>%
  filter(Subtipo=="Robo a transeúnte en v?a p?blica")

RoboTrans <- gather(RoboTrans, Mes , value=Incidencia, "01":"12")

RoboTransEntidad <- aggregate(Incidencia ~ Subtipo + Mes + Año , data= RoboTrans, sum)

RoboTransEntidad<- spread(RoboTransEntidad, Año, Incidencia)

RoboTransMes <- aggregate(Incidencia ~ Subtipo + Municipio + Mes +  Año , data= RoboTrans, sum)

RoboTransMes<- spread(RoboTransMes, Año, Incidencia)

###Robo a casa habitación

RoboCasa <- Estatal %>%
  filter(Subtipo=="Robo a casa habitación")

RoboCasa <- gather(RoboCasa, Mes , value=Incidencia, "01":"12")

RoboCasaEntidad <- aggregate(Incidencia ~ Subtipo + Mes + Año   , data= RoboCasa, sum)

RoboCasaEntidad<- spread(RoboCasaEntidad, Año, Incidencia)

RoboCasaMes <- aggregate(Incidencia ~ Subtipo + Municipio + Mes +  Año , data= RoboCasa, sum)

RoboCasaMes<- spread(RoboCasaMes, Año, Incidencia)

