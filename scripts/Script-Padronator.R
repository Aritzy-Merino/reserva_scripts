rm(list=ls())
setwd("~")


##################################
# Procesar Datos Padronator      #
# Aritzy Sanchez                 #
# 06/02/2019                     #
##################################

dir1 = "/Users/HP/Documents/MIM/Padronator"

padronator <- read.csv(paste(dir1,"190130-bd-padron maestro.csv", sep="/"), stringsAsFactors = F)

#El Ine lo guarda con: APELLIDO, APELLIDO, NOMBRE, NOMBRE. No meter ignore case para que tenga más resultados 
#pero todo en mayúscula y sin acentos.

#¿Dónde tengo registros? OJO: La Ñ sí va, los acentos no van, todo en mayúscula
#registros <- grep("PEÑA NIETO ENRIQUE", padronator$NOMBRE, fixed=TRUE)

#Proyectar en base de datos (sacar subconjunto)
#consulta <- padronator[registros,]

#Consulta exprés:
  #padronator[grep("BELTRONES RIVERA MANLIO FABIO", padronator$NOMBRE, fixed=TRUE),]

#Puedes exportar a CSV también!
#write.csv(consulta)