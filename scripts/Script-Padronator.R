rm(list=ls())
setwd("~")


##################################
# Procesar Datos Padronator      #
# Aritzy Sanchez                 #
# 06/02/2019                     #
##################################

dir1 = "/Users/HP/Documents/MIM/Padronator"

padronator <- read.csv(paste(dir1,"190130-bd-padron maestro.csv", sep="/"), stringsAsFactors = F)

#El Ine lo guarda con: APELLIDO, APELLIDO, NOMBRE, NOMBRE. No meter ignore case para que tenga m�s resultados 
#pero todo en may�scula y sin acentos.

#�D�nde tengo registros? OJO: La � s� va, los acentos no van, todo en may�scula
#registros <- grep("PE�A NIETO ENRIQUE", padronator$NOMBRE, fixed=TRUE)

#Proyectar en base de datos (sacar subconjunto)
#consulta <- padronator[registros,]

#Consulta expr�s:
  #padronator[grep("BELTRONES RIVERA MANLIO FABIO", padronator$NOMBRE, fixed=TRUE),]

#Puedes exportar a CSV tambi�n!
#write.csv(consulta)