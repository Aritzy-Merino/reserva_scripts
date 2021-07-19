setwd("~")
rm(list=ls())


##################################
# Procesar Datos indicadores     #
#     nivel estatal              #
# Aritzy Sanchez                 #
# 16/07/2019                     #
##################################

#install.packages("jtools")
library(pacman)
p_load (foreign, tidyverse, plyr, dplyr, tidyr, reshape, scales, graphics, psych, car, jtools)


setwd("/Users/HP/Documents/Indicador_funcion_policial")

datos <- read_csv("210112-BD-Indicador_funcion_policial_2020.csv")

#An?lisis exploratorio con percepci?n de inseguridad

model1 <- lm(Percepcion_inseguridad ~ Control_confianza + Estado_fuerza + Percepcion_desempeno + 
               Percepcion_corrupcion + Percepcion_disposicion + Promedio_horas_lab_2017 + Indepol + 
               Porcentaje_policias_armadas_2017 + Porcentaje_FASP + IMC_normado_2017 + Vocacional_2017 + 
               Linea_ingreso + Primer_respondiente + Primeros_actos + Investigacion_criminal, data = datosv2_completo)
summary(model1)

model2 <- lm(Percepcion_desempeno ~ Control_confianza + Estado_fuerza + Tasa_victimizacion + 
               Percepcion_corrupcion + Percepcion_disposicion + Promedio_horas_lab_2017 + Indepol + 
               Porcentaje_policias_armadas_2017 + Porcentaje_FASP + IMC_normado_2017 + Vocacional_2017 + 
               Linea_ingreso + Primer_respondiente + Primeros_actos + Investigacion_criminal, data = datosv2_completo)
summary(model2)

model3 <- lm(Delitos_denunciados ~ Control_confianza + Estado_fuerza + Percepcion_desempeno + 
               Percepcion_corrupcion + Percepcion_disposicion + Promedio_horas_lab_2017 + Indepol + 
               Porcentaje_policias_armadas_2017 + Porcentaje_FASP + IMC_normado_2017 + Vocacional_2017 + 
               Linea_ingreso + Primer_respondiente + Primeros_actos + Investigacion_criminal, data = datosv2_completo)
summary(model3)


#Filtrar variables de inter?s para PCA:


#### Tras an?lisis explotarorio se descart? la tasa de victimizaci?n como variable independiente####

#Selecci?n de variables

#0) Porcentaje de delitos denunciados
#1) Control_confianza 
#2) Estado_fuerza 
#3) Percepcion_desempeno_pol 
#4) Percepcion_corrupcion 
#5) Percepcion_disposicion
#6) Promedio_horas_lab_2017 
#7) Indepol 
#8) Porcentaje_policias_armadas_2017
#9) Porcentaje_FASP
#10) IMC_normado_2017 
#11) Vocacional_2017
#12) Linea_ingreso

delitos_indicadores <- delitos_indicadores %>% 
  select((13),(1:12))

datos_pca <- select(datosv2_completo, c(9:31))
datos_pca <- select(datosv2_pca, -c(3,4,6,7,15,17,19,21,22))

#Descriptivos estad?sticos de variables

describe(datosv2_pca)
scatterplotMatrix(datosv2_pca)
matriz_correlacion_v2 <- cor(datosv2_pca)

#Estandarizaci?n de datos

datosv2_pca_estandarizado <- standardize(datosv2_pca)

scatterplotMatrix(datosv2_pca_estandarizado)


matriz_correlacion <- cor(datosv2_pca_estandarizado)

datos.pca <- prcomp(datosv2_pca_estandarizado, center = TRUE, scale. = TRUE)

summary(datos.pca)

vector_factores <- datos.pca$rotation
vector_equis <- datos.pca$x

vector_factores <- as.data.frame(vector_factores)
vector_factores <- select(vector_factores, c(1:5))

#Multiplicacion de matriz de observaciones (datosv2_pca) con vector de pesos

datosv2_completo$PC1 <- as.matrix(datosv2_pca_estandarizado) %*% as.matrix (vector_factores$PC1)
datosv2_completo$PC2 <- as.matrix(datosv2_pca_estandarizado) %*% as.matrix (vector_factores$PC2)
datosv2_completo$PC3 <- as.matrix(datosv2_pca_estandarizado) %*% as.matrix (vector_factores$PC3)
datosv2_completo$PC4 <- as.matrix(datosv2_pca_estandarizado) %*% as.matrix (vector_factores$PC4)
datosv2_completo$PC5 <- as.matrix(datosv2_pca_estandarizado) %*% as.matrix (vector_factores$PC5)

datosv2_completo$PC_completo <- (.4*PC1) + (.2*PC2) + (.2*PC3) + (.1*PC4) +(.1*PC5)


####Segunda opci?n: con respecto a percepci?n de seguridad ####
#0) Perceci?n de inseguridad
#1) Control_confianza 
#2) Estado_fuerza 
#3) Percepcion_desempeno_pol 
#4) Percepcion_confianza
#5) Percepcion_corrupcion 
#6) Percepcion_disposicion
#7) Promedio_horas_lab_2017 
#8) Indepol 
#9) Porcentaje_policias_armadas_2017
#10) IMC_normado_2017 
#11) Vocacional_2017
#12) Linea_ingreso
datosv3_pca <- select(datosv2_completo, c(10:33))
datosv3_pca <- select(datosv3_pca, -c(2,3,5,11,12,16,17,18,20,22,23))

#Descriptivos estad?sticos de variables

describe(datosv3_pca)
scatterplotMatrix(datosv3_pca)
matriz_correlacion_v3 <- cor(datosv3_pca)

#####Estandarizaci?n de datos-PCA (Segunda opci?n)####

datosv2_pca_estandarizado <- standardize(datosv2_pca)

scatterplotMatrix(datosv2_pca_estandarizado)

matriz_correlacion <- cor(datosv2_pca_estandarizado)

datos.pca <- prcomp(datosv2_pca_estandarizado, center = TRUE, scale. = TRUE)

summary(datos.pca)

vector_factores <- datos.pca$rotation
vector_equis <- datos.pca$x

vector_factores <- as.data.frame(vector_factores)
vector_factores <- select(vector_factores, c(1:5))

#Multiplicacion de matriz de observaciones (datosv2_pca) con vector de pesos

datosv2_completo$PC1 <- as.matrix(datosv2_pca_estandarizado) %*% as.matrix (vector_factores$PC1)
datosv2_completo$PC2 <- as.matrix(datosv2_pca_estandarizado) %*% as.matrix (vector_factores$PC2)
datosv2_completo$PC3 <- as.matrix(datosv2_pca_estandarizado) %*% as.matrix (vector_factores$PC3)
datosv2_completo$PC4 <- as.matrix(datosv2_pca_estandarizado) %*% as.matrix (vector_factores$PC4)
datosv2_completo$PC5 <- as.matrix(datosv2_pca_estandarizado) %*% as.matrix (vector_factores$PC5)

datosv2_completo$PC_completo <- (.4*PC1) + (.2*PC2) + (.2*PC3) + (.1*PC4) +(.1*PC5)


####Tercera opci?n: con respecto a delitos denunciados (esta es la opci?n que elegimos)####
#0) Delitos que fueron denunciados o que derivaron en una carpeta de investigaci?n
#1) Control_confianza 
#2) Estado_fuerza 
#3) Promedio_horas_lab_2017
#4) Indepol
#5) Percepcion_desempeno
#6) Percepcion_confianza
#7) Percepcion_disposicion
#8) Percepcion_corrupcion 
#9) Linea_ingreso
#10) IMC_normado_2017 
#11) Vocacional_2017
#12) Primer_respondiente
#13) Primeros_actos 
#14) Investigacion_criminal
#15) Porcentaje_policias_armadas_2017
#16) Porcentaje_FASP

datos_denuncias<- datos %>% 
  select((12:13),(16:17), (21:23), 25, 28, 30, (32:35), 37, 39)

#Descriptivos estad?sticos de variables

describe(datos_denuncias)
scatterplotMatrix(datos_denuncias)
matriz_correlacion_v4 <- cor(datos_denuncias)

#Estandarizaci?n de datos
datos_pca_estandarizado <- standardize(datos_denuncias)
scatterplotMatrix(datos_pca_estandarizado)
matriz_correlacion <- cor(datos_pca_estandarizado)

#PCA

datos.pca.4 <- prcomp(datos_pca_estandarizado, center = TRUE, scale. = TRUE)

summary(datos.pca.4)

vector_factores_v4 <- datos.pca.4$rotation
vector_equis_v4 <- datos.pca.4$x

vector_factores_v4 <- as.data.frame(vector_factores_v4)
vector_factores_v4 <- select(vector_factores_v4, c(1:4))

#Multiplicacion de matriz de observaciones (datosv2_pca) con vector de pesos

datos$PC1 <- as.matrix(datos_pca_estandarizado) %*% as.matrix (vector_factores_v4$PC1)
datos$PC2 <- as.matrix(datos_pca_estandarizado) %*% as.matrix (vector_factores_v4$PC2)
datos$PC3 <- as.matrix(datos_pca_estandarizado) %*% as.matrix (vector_factores_v4$PC3)
datos$PC4 <- as.matrix(datos_pca_estandarizado) %*% as.matrix (vector_factores_v4$PC4)

datos$PC_completo <- (.4*datos$PC1) + (.1*datos$PC2) + (.3*datos$PC3) + (.2*datos$PC4)

#Sacar categor?as
p_load(BAMMtools)
jenks <- ddply(datos, .(Year), function(x) getJenksBreaks(x$PC_completo, 5))

quintiles <- ddply(datos, .(Year), function(x) quantile(x$PC_completo, probs= c(0, .20, .40, .60, .80, 1))) 

names(quintiles)[2] <- "V1"
names(quintiles)[3] <- "V2"
names(quintiles)[4] <- "V3"
names(quintiles)[5] <- "V4"
names(quintiles)[6] <- "V5"
names(quintiles)[7] <- "V6"


#Categorizar

datos <- left_join(datos, quintiles, by = "Year")

datos <- as.data.frame(datos)

datos$categoria <- case_when(datos$PC_completo<=datos$V6 & datos$PC_completo>datos$V5 ~ "Instituci?n garante", 
                             datos$PC_completo<=datos$V5 & datos$PC_completo>datos$V4 ~ "Instituci?n debilitada", 
                             datos$PC_completo<=datos$V4 & datos$PC_completo>datos$V3 ~ "Instituci?n deficiente", 
                             datos$PC_completo<=datos$V3 & datos$PC_completo>datos$V2 ~ "Instituci?n en crisis", 
                             datos$PC_completo<=datos$V2 & datos$PC_completo>=datos$V1 ~ "Instituci?n en caos")

datos <- datos %>% 
  select ((1:2), 5, (3:4), (12:18), (21:28), (30), (32:39), (44), (51))


write.csv(datos, file = "/Users/HP/Documents/Descifra/200210-BD-Datos_funcion_policial_2017-2019.csv")
