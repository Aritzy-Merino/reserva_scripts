rm(list = ls())

#### se abren paquetes
library(pacman)
p_load(stringr, dplyr, readr, readxl, tidyverse, data.table, ggplot2, modeest, psych, lubridate, tidyr, haven)

#### Se fijan parÃÂ¡metros del ÃÂ¡rea de trabajo
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
options(scipen=999)

##########################################################

# abrir base agregada sesnsp
setwd("/Users/aritzy/Documents/Rizika/Incidencia")

incidencia <- read_csv("Municipal-Delitos-2015-2021_mar2021.csv", 
                       locale = locale(encoding = "latin1"))

pob_intercensal2020 <- read_csv("/Users/aritzy/Documents/Rizika/pob-geo/ITER_NALCSV20.csv")

pob_intercensal2020 <- pob_intercensal2020 %>% 
  filter(NOM_LOC == "Total del Municipio")

pob_intercensal2020 <- pob_intercensal2020 %>% 
  dplyr::select(ENTIDAD, NOM_ENT, MUN, NOM_MUN, POBTOT)

pob_intercensal2020$cve_ent <- str_pad(pob_intercensal2020$ENTIDAD, 2, pad = 0)
pob_intercensal2020$mun <- str_pad(pob_intercensal2020$MUN, 3, pad = 0)
pob_intercensal2020$cve_mun <- paste0(pob_intercensal2020$cve_ent, pob_intercensal2020$mun)

pob_intercensal2020 <- pob_intercensal2020 %>% 
  dplyr::select(cve_ent, NOM_ENT, cve_mun, NOM_MUN, POBTOT)

names(pob_intercensal2020)[2] <- "entidad"
names(pob_intercensal2020)[4] <- "municipio"
names(pob_intercensal2020)[5] <- "pobtot"

# filtrar Años interÃ©s
table(incidencia$Año)
class(incidencia$Año)
incidencia_objetivo <- incidencia %>% 
  filter(Año==2019 | Año==2020 | Año==2021)
names(incidencia_objetivo)

#### plataforma riesgos####
# seleccionar delitos de interÃ©s
subtipos <- c("Homicidio doloso", "Secuestro", "Extorsión", "Robo de vehículo automotor",
              "Feminicidio", "Robo a transportista", 
              "Robo a transeúnte en espacio abierto al público", 
              "Robo a transeúnte en vía pública", "Robo a negocio", 
              "Robo en transporte individual", "Robo en transporte público colectivo",
              "Robo en transporte público individual", "Robo de ganado", "Narcomenudeo", 
              "Violencia familiar", "Lesiones dolosas",
              "Violencia de género en todas sus modalidades distinta a la violencia familiar", "Robo a institución bancaria", "Robo de maquinaria")
select_delitos_todos <- incidencia_objetivo %>% 
  filter(`Subtipo de delito` %in% subtipos)

# simplificar mÃ¡s de una modalidad 
robo_v_cv <- c("Robo de coche de 4 ruedas Con violencia",
               "Robo de embarcaciones pequeñas y grandes Con violencia",
               "Robo de motocicleta Con violencia")
robo_v_sv <- c("Robo de coche de 4 ruedas Sin violencia",
               "Robo de embarcaciones pequeñas y grandes Sin violencia",
               "Robo de motocicleta Sin violencia")

delitos <- select_delitos_todos %>% 
  mutate(delito=ifelse(`Subtipo de delito`=="Robo de vehículo automotor" & Modalidad %in% robo_v_cv, "Robo de vehículo con violencia",
                       ifelse(`Subtipo de delito`=="Robo de vehículo automotor" & Modalidad %in% robo_v_sv, "Robo de vehículo sin violencia",
                              ifelse(`Subtipo de delito`=="Robo a transportista" & Modalidad=="Con violencia", "Robo a transportista con violencia",
                                     ifelse(`Subtipo de delito`=="Robo a transportista" & Modalidad=="Sin violencia", "Robo a transportista sin violencia", `Subtipo de delito`)))))

delitos_todos <- as.data.frame(unique(delitos$delito)) 

delitos <- delitos %>% 
  mutate(delito_ab = ifelse(delito=="Homicidio doloso", "homicidio_dol",
                            ifelse(delito=="Lesiones dolosas", "lesiones_dol",
                                   ifelse(delito=="Feminicidio", "feminicidio",
                                          ifelse(delito=="Secuestro", "secuestro",
                                                 ifelse(delito=="Robo de vehículo con violencia", "robo_veh_cv",
                                                        ifelse(delito=="Robo de vehículo sin violencia", "robo_veh_sv",
                                                               ifelse(delito=="Robo a transportista con violencia", "robo_transportista_cv",
                                                                      ifelse(delito=="Robo a transportista sin violencia", "robo_transportista_sv",
                                                                             ifelse(delito=="Robo a transeúnte en vía pública", "robo_transeunte_viap",
                                                                                    ifelse(delito=="Robo a transeúnte en espacio abierto al público", "robo_transeunte_ep",
                                                                                           ifelse(delito=="Robo en transporte público individual", "robo_transppub_ind",
                                                                                                  ifelse(delito=="Robo en transporte público colectivo", "robo_transppub_col",
                                                                                                         ifelse(delito=="Robo en transporte individual", "robo_transp_ind",
                                                                                                                ifelse(delito=="Robo a negocio", "robo_negocio",
                                                                                                                       ifelse(delito=="Robo de ganado", "robo_ganado",
                                                                                                                              ifelse(delito=="Extorsión", "extorsion",
                                                                                                                                     ifelse(delito=="Violencia familiar", "viol_familiar",
                                                                                                                                            ifelse(delito=="Robo de maquinaria", "robo_maquinaria",       
                                                                                                                                                   ifelse(delito=="Violencia de género en todas sus modalidades distinta a la violencia familiar", "viol_genero",
                                                                                                                                                          ifelse(delito=="Robo a institución bancaria", "robo_banco","narcomenudeo")))))))))))))))))))))

# sumar por delito
meses <- colnames(incidencia_objetivo)[10:21]
delitos_indicadores <- delitos %>% 
  group_by(Año, Clave_Ent, Entidad, `Cve. Municipio`, Municipio, `Bien jurídico afectado`, 
           `Tipo de delito`, delito, delito_ab) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)

##Suma doce meses (para años disímiles)
delitos_indicadores <- delitos_indicadores %>%
  rowwise() %>%
  replace(is.na(.), 0) %>%
  mutate(ano1 = ifelse(Año==2019, sum( Abril, Mayo, Junio, Julio, Agosto, Septiembre, Octubre, Diciembre, Diciembre),
                       ifelse(Año==2020, sum(Enero, Febrero, Marzo), 0)),
         ano2 = ifelse(Año==2020, sum(Abril, Mayo, Junio, Julio, Agosto, Septiembre, Octubre, Diciembre, Diciembre),
                       ifelse(Año==2021, sum(Enero, Febrero, Marzo), 0)),
         semestre1 = ifelse(Año==2019, sum(Junio, Julio, Agosto, Septiembre, Octubre, Diciembre, Diciembre),0),
         semestre2 = ifelse(Año==2020, sum(Junio, Julio, Agosto, Septiembre, Octubre, Diciembre, Diciembre),0),
         mes1= ifelse(Año==2020 , sum(Febrero), 0),
         mes2= ifelse(Año==2021 , sum(Marzo),0))


# delitos_indicadores <- delitos_indicadores %>%
#   rowwise() %>% 
#   replace(is.na(.), 0) %>%
#   mutate(ano1 = ifelse(Año==2019, sum(Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto, Septiembre, Octubre, Diciembre, Diciembre), 0),
#          ano2 = ifelse(Año==2020, sum(Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto, Septiembre, Octubre, Diciembre, Diciembre), 0),
#          semestre1 = ifelse(Año==2019, sum(  Junio, Julio, Agosto, Septiembre, Octubre, Diciembre, Diciembre),0),
#          semestre2 = ifelse(Año==2020, sum(  Junio, Julio, Agosto, Septiembre, Octubre, Diciembre, Diciembre),0),
#          mes1= ifelse(Año==2020 , sum(Diciembre), 0),
#          mes2= ifelse(Año==2020 , sum(Diciembre),0))

#### agregar por municipio, delito#####
names(delitos_indicadores)
delitos_indicadores <- delitos_indicadores %>%
  select((2:9), (22:27)) %>% 
  group_by(Clave_Ent, Entidad, `Cve. Municipio`, Municipio, `Bien jurídico afectado`, 
           `Tipo de delito`, delito, delito_ab) %>% 
  summarise_all(list(sum))
delitos_indicadores$cve_mun <- str_pad(delitos_indicadores$`Cve. Municipio`, 5, pad = 0)
names(delitos_indicadores)
delitos_indicadores <- delitos_indicadores %>% 
  select(15, 1:14)

names(pob_intercensal2020)
head(pob_intercensal2020)

base_pob <- pob_intercensal2020 %>% 
  select(cve_mun, pobtot)

delitos_indicadores <- left_join(delitos_indicadores, base_pob, by = "cve_mun")

names(delitos_indicadores)
delitos_indicadores <- delitos_indicadores %>% 
  mutate(ano1_t_pob = round((ano1/pobtot)*100000, 2),
         ano2_t_pob = round((ano2/pobtot)*100000,2),
         sem1_t_pob = round((semestre1/pobtot)*100000,2),
         sem2_t_pob = round((semestre2/pobtot)*100000,2),
         mes1_t_pob = round((mes1/pobtot)*100000,2),
         mes2_t_pob = round((mes2/pobtot)*100000,2))

# indicador cambio porcentual

delitos_indicadores <- delitos_indicadores %>% 
  mutate(t_crec_anual = round(ifelse(ano1_t_pob==0 & ano2_t_pob!=0, 100,
                                     ((ano2_t_pob - ano1_t_pob)/ano1_t_pob)*100),2), 
         t_crec_semestre = round(ifelse(sem1_t_pob==0 & sem2_t_pob!=0, 100,
                                        ((sem2_t_pob - sem1_t_pob)/sem1_t_pob)*100),2),
         t_crec_mensual = round(ifelse(mes1_t_pob==0 & mes2_t_pob!=0, 100,
                                       ((mes2_t_pob - mes1_t_pob)/mes1_t_pob)*100), 2))
is.na(delitos_indicadores)<-sapply(delitos_indicadores, is.infinite)
delitos_indicadores[is.na(delitos_indicadores)]<-0


write.csv(delitos_indicadores, file="datos_municipales_mar_2021.csv", fileEncoding ="UTF-8")

####Categorización por jenks####

p_load(BAMMtools)
p_load(plyr)

#Para la incidencia
jenks_ano2 <- ddply(delitos_indicadores, .(delito_ab), function(x) getJenksBreaks(x$ano2_t_pob, 6))

write.csv(jenks_ano2, file="jenks_anual_sep_2020.csv", fileEncoding ="UTF-8")

jenks_crecimiento <- ddply(delitos_indicadores, .(delito_ab), function(x) getJenksBreaks(x$t_crec_anual, 6))

jenks_sem2 <- ddply(delitos_indicadores, .(delito_ab), function(x) getJenksBreaks(x$sem2_t_pob, 6))

jenks_crec_sem <- ddply(delitos_indicadores, .(delito_ab), function(x) getJenksBreaks(x$t_crec_semestre, 6))

jenks_mes2 <- ddply(delitos_indicadores, .(delito_ab), function(x) getJenksBreaks(x$mes2_t_pob, 6))

#### Categorías Anuales: Año actual por incidencia ####

delitos_indicadores_anual <- left_join(delitos_indicadores, jenks_ano2, by = "delito_ab")

delitos_indicadores_anual <- delitos_indicadores_anual %>%
  select((1:11), (16:18), (26:31))  

delitos_indicadores_anual <- as.data.frame(delitos_indicadores_anual)

delitos_indicadores_anual$categoria <- case_when(delitos_indicadores_anual$ano2_t_pob<=delitos_indicadores_anual$V6 & delitos_indicadores_anual$ano2_t_pob>delitos_indicadores_anual$V5 ~ "Muy alto", 
                                              delitos_indicadores_anual$ano2_t_pob<=delitos_indicadores_anual$V5 & delitos_indicadores_anual$ano2_t_pob>delitos_indicadores_anual$V4 ~ "Alto", 
                                              delitos_indicadores_anual$ano2_t_pob<=delitos_indicadores_anual$V4 & delitos_indicadores_anual$ano2_t_pob>delitos_indicadores_anual$V3 ~ "Medio", 
                                              delitos_indicadores_anual$ano2_t_pob<=delitos_indicadores_anual$V3 & delitos_indicadores_anual$ano2_t_pob>delitos_indicadores_anual$V2 ~ "Bajo", 
                                              delitos_indicadores_anual$ano2_t_pob<=delitos_indicadores_anual$V2 & delitos_indicadores_anual$ano2_t_pob>delitos_indicadores_anual$V1 ~ "Muy bajo", 
                                              delitos_indicadores_anual$ano2_t_pob==delitos_indicadores_anual$V1 ~ " Sin incidencia")

# Seleccionar y spreadear la tabla

#Categorías
delitos_cat_ano2 <- delitos_indicadores_anual %>%
  select((2:3), 5, 1, 9, 12, 21)

delitos_cat_ano2 <- delitos_cat_ano2 %>%
  spread(key = delito_ab, value = categoria, fill = 0) %>% 
  group_by(cve_mun)

colnames(delitos_cat_ano2)[6:26] <- paste0("cat_a2_", colnames(delitos_cat_ano2)[6:26])

#Tasas

#año1
tasa_ano1 <- delitos_indicadores_anual %>%
  select((1), (9), (13))

tasa_ano1 <- tasa_ano1 %>%
  spread(key = delito_ab, value = ano1_t_pob, fill = 0) %>% 
  group_by(cve_mun)

colnames(tasa_ano1)[2:22] <- paste0("tasa_a1_", colnames(tasa_ano1)[2:22])

#año2
tasa_ano2 <- delitos_indicadores_anual %>%
  select((1), (9), (14))

tasa_ano2 <- tasa_ano2 %>%
  spread(key = delito_ab, value = ano2_t_pob, fill = 0) %>% 
  group_by(cve_mun)

colnames(tasa_ano2)[2:22] <- paste0("tasa_a2_", colnames(tasa_ano2)[2:22])

# Totales

#año1
totales_ano1 <- delitos_indicadores_anual %>%
  select((1), (9:10))

totales_ano1 <- totales_ano1 %>%
  spread(key = delito_ab, value = ano1, fill = 0) %>% 
  group_by(cve_mun)

colnames(totales_ano1)[2:22] <- paste0("total_a1_", colnames(totales_ano1)[2:22])

#año2
totales_ano2 <- delitos_indicadores_anual %>%
  select((1), (9), (11))

totales_ano2 <- totales_ano2 %>%
  spread(key = delito_ab, value = ano2, fill = 0) %>% 
  group_by(cve_mun)

colnames(totales_ano2)[2:22] <- paste0("total_a2_", colnames(totales_ano2)[2:22])

#unimos la base de tasas anuales con su categoría

anual_fin <- Reduce(function(x,y) merge(x=x, y=y, by = "cve_mun"), list( delitos_cat_ano2, totales_ano1, tasa_ano1, totales_ano2, tasa_ano2))

anual_fin <- anual_fin %>% 
  select(3, 2, 4, 1, (5:110))

names(anual_fin)[1] <- "estado"
names(anual_fin)[2] <- "clave_ent"
names(anual_fin)[3] <- "municipio"
names(anual_fin)[4] <- "cve_mun"
names(anual_fin)[5] <- "poblacion"

### agregamos fecha de actualización 
anual_fin$fecha_act <- "Diciembre 2020"

### datos comparativos: maximos año actual###

tasa_max_a2 <- delitos_indicadores_anual %>%
  select((2:3), (5), (1), (9), (14))

tasa_max_a2 <- tasa_max_a2 %>% 
  group_by(delito_ab) %>% 
  slice(which.max(ano2_t_pob))

tasa_max_a2 <- tasa_max_a2 %>% 
  mutate(mun_max = paste0("El municipio con incidencia máxima es ", Municipio, ", ", Entidad, " con una tasa de ", ano2_t_pob, " por cada 100mil habs.")) 

tasa_max_a2 <- tasa_max_a2 %>% 
  select(5,7) %>% 
  spread(key = delito_ab, value = mun_max, fill = 0)

colnames(tasa_max_a2)[1:21] <- paste0("mun_max_", colnames(tasa_max_a2)[1:21])

#### unimos base anual final con municipios máximos

anual_fin <- merge(anual_fin, tasa_max_a2)

#### promedio nacional de tasa anual

promedio_ano2 <- ddply(delitos_indicadores, .(delito_ab), function(x) mean(x$ano2_t_pob)) 

promedio_ano2 <- promedio_ano2 %>%
  mutate(promedio = round(V1, digits = 2)) %>% 
  select(1,3) %>% 
  spread(key = delito_ab, value = promedio, fill = 0)

colnames(promedio_ano2)[1:21] <- paste0("promedio_nac_", colnames(promedio_ano2)[1:21])

anual_fin <- merge(anual_fin, promedio_ano2)

#write.csv(anual_fin, file="datos_anuales_2020.csv", fileEncoding ="UTF-8")

#### Categorías Anuales: crecimiento anual por incidencia####

delitos_categoria_ca <- left_join(delitos_indicadores, jenks_crecimiento, by = "delito_ab")

delitos_categoria_ca <- delitos_categoria_ca %>%
  select((1:9), (16:18), (23), (26:31))  

delitos_categoria_ca <- as.data.frame(delitos_categoria_ca)

delitos_categoria_ca$categoria <- case_when(delitos_categoria_ca$t_crec_anual<=delitos_categoria_ca$V6 
                                            & delitos_categoria_ca$t_crec_anual>delitos_categoria_ca$V5 
                                            ~ "Crecimiento muy alto", 
                                            delitos_categoria_ca$t_crec_anual<=delitos_categoria_ca$V5 
                                            & delitos_categoria_ca$t_crec_anual>delitos_categoria_ca$V4
                                            ~ "Crecimiento alto", 
                                            delitos_categoria_ca$t_crec_anual<=delitos_categoria_ca$V4 
                                            & delitos_categoria_ca$t_crec_anual>delitos_categoria_ca$V3 
                                            ~ "Crecimiento medio", 
                                            delitos_categoria_ca$t_crec_anual<=delitos_categoria_ca$V3 
                                            & delitos_categoria_ca$t_crec_anual> 0 ~ "Crecimiento bajo", 
                                            delitos_categoria_ca$t_crec_anual==0 ~ "Sin cambio", 
                                            delitos_categoria_ca$t_crec_anual< 0 
                                            & delitos_categoria_ca$t_crec_anual>delitos_categoria_ca$V2 
                                            ~ "Decrecimiento bajo",
                                            delitos_categoria_ca$t_crec_anual<=delitos_categoria_ca$V2 
                                            ~ "Decrecimiento alto")

# Seleccionar y spreadear la tabla

#Categorías
delitos_cat_ca <- delitos_categoria_ca %>%
  select((1:5), (9:10), (20))

delitos_cat_ca <- delitos_cat_ca %>%
  spread(key = delito_ab, value = categoria, fill = 0) %>% 
  group_by(cve_mun)

colnames(delitos_cat_ca)[7:27] <- paste0("cat_ca_", colnames(delitos_cat_ca)[7:27])


#comparativo
tasa_ca <- delitos_categoria_ca %>%
  select((1), (9), (13))

tasa_ca <- tasa_ca %>%
  spread(key = delito_ab, value = t_crec_anual, fill = 0) %>% 
  group_by(cve_mun)

colnames(tasa_ca)[2:22] <- paste0("tasa_ca_", colnames(tasa_ca)[2:22])

#unimos la base de tasas anuales con su categor?a

crec_anual_fin <- left_join(delitos_cat_ca, tasa_ca, by = "cve_mun")

crec_anual_fin <- crec_anual_fin %>% 
  select(3, 2, 5, 1, (6:48))

names(crec_anual_fin)[1] <- "estado"
names(crec_anual_fin)[2] <- "clave_ent"
names(crec_anual_fin)[3] <- "municipio"
names(crec_anual_fin)[5] <- "poblacion"


### datos comparativos: maximos año actual###

tasa_max_ca <- delitos_indicadores %>%
  select((2:3), (5), (1), (9), (23))

tasa_max_ca <- tasa_max_ca %>% 
  group_by(delito_ab) %>% 
  slice(which.max(t_crec_anual))

tasa_max_ca <- tasa_max_ca %>% 
  mutate(mun_max = paste0("El municipio con mayor crecimiento es ", Municipio, ", ", Entidad, " con una tasa de ", t_crec_anual, " por cada 100mil habs.")) 

tasa_max_ca <- tasa_max_ca %>% 
  select(9,11) %>% 
  spread(key = delito_ab, value = mun_max, fill = 0)

colnames(tasa_max_ca)[1:21] <- paste0("mun_crec_max_", colnames(tasa_max_ca)[1:21])

#### unimos base anual final con municipios máximos

crec_anual_fin <- merge(crec_anual_fin, tasa_max_ca)

#### promedio nacional de tasa anual

promedio_crecimiento <- ddply(delitos_indicadores, .(delito_ab), function(x) mean(x$t_crec_anual))

promedio_crecimiento <- promedio_crecimiento %>%
  mutate(promedio = round(V1, digits = 2)) %>% 
  select(1,3) %>% 
  spread(key = delito_ab, value = promedio, fill = 0)

colnames(promedio_crecimiento)[1:21] <- paste0("promedio_nac_crec_", colnames(promedio_crecimiento)[1:21])

crec_anual_fin <- merge(crec_anual_fin, promedio_crecimiento)

anual_fin <- left_join(anual_fin, crec_anual_fin, by = c("estado", "clave_ent", "municipio", "cve_mun", "poblacion") )

#### segmentar bases por tipos de delitos

patrimonial <- anual_fin %>% 
  select(estado | clave_ent| municipio | cve_mun | poblacion | fecha_act | starts_with("cat_a2_robo") |  starts_with("total_a1_robo") | starts_with("total_a2_robo") | starts_with("tasa_a1_robo") | starts_with("tasa_a2_robo") | starts_with("mun_max_robo") | starts_with("promedio_nac_robo") |  starts_with("cat_ca_robo")  |starts_with("tasa_ca_robo") |starts_with("mun_crec_max_robo") |starts_with("promedio_nac_crec_robo"))

### lista de variables patrimoniales
lista <- names(patrimonial)

lista <- as.data.frame(lista)

write_csv(lista, "lista.csv")

##### mergeamos csv municipios: abrir carto .csv ####

mun_mex2018 <- read_csv("/Users/aritzy/Documents/Rizika/pob-geo/mun_mex2018.csv", locale = locale())
names(mun_mex2018)

mun_mex2018 <- mun_mex2018 %>%
  select(1:4)

names(mun_mex2018)[3] <- "cve_mun"

## datos anuales ##
carto_patrimonial <- left_join(mun_mex2018,patrimonial,
                              by ="cve_mun")

names(carto_patrimonial) <- tolower(names(carto_patrimonial))
write.csv(carto_patrimonial, file = "carto_patrimonial.csv",
          row.names = FALSE)


#####Categoría semestral: semestre 2 por incidencia #####

delitos_semestral <- left_join(delitos_indicadores, jenks_sem2, by = "delito_ab")

delitos_semestral <- delitos_semestral %>%
  select((1:9), 12, 13, 16, 19, 20, (26:31))  

delitos_semestral <- as.data.frame(delitos_semestral)

delitos_semestral$categoria <- case_when(delitos_semestral$sem2_t_pob<=delitos_semestral$V6 & delitos_semestral$sem2_t_pob>delitos_semestral$V5 ~ "Muy alto", 
                                              delitos_semestral$sem2_t_pob<=delitos_semestral$V5 & delitos_semestral$sem2_t_pob>delitos_semestral$V4 ~ "Alto", 
                                              delitos_semestral$sem2_t_pob<=delitos_semestral$V4 & delitos_semestral$sem2_t_pob>delitos_semestral$V3 ~ "Medio", 
                                              delitos_semestral$sem2_t_pob<=delitos_semestral$V3 & delitos_semestral$sem2_t_pob>delitos_semestral$V2 ~ "Bajo", 
                                              delitos_semestral$sem2_t_pob<=delitos_semestral$V2 & delitos_semestral$sem2_t_pob>delitos_semestral$V1~ "Muy bajo",
                                              delitos_semestral$sem2_t_pob==delitos_semestral$V1 ~ "Sin incidencia")

#seleccionar y spreadear

#categorización
delitos_cat_sem2 <- delitos_semestral %>%
  select((1:5), 9, 12, (21))

delitos_cat_sem2 <- delitos_cat_sem2 %>%
  spread(key = delito_ab, value = categoria, fill = 0) %>% 
  group_by(cve_mun)

colnames(delitos_cat_sem2)[7:27] <- paste0("cat_sem2_", colnames(delitos_cat_sem2)[7:27])

#Totales

#Semestre1
totales_sem1 <- delitos_semestral %>%
  select((1), (9), (10))

totales_sem1 <- totales_sem1 %>%
  spread(key = delito_ab, value = semestre1, fill = 0) %>% 
  group_by(cve_mun)

colnames(totales_sem1)[2:22] <- paste0("total_sem1_", colnames(totales_sem1)[2:22])

#Semestre2
totales_sem2 <- delitos_semestral %>%
  select((1), (9), (11))

totales_sem2 <- totales_sem2 %>%
  spread(key = delito_ab, value = semestre2, fill = 0) %>% 
  group_by(cve_mun)

colnames(totales_sem2)[2:22] <- paste0("total_sem2_", colnames(totales_sem2)[2:22])

#Tasas

#Semestre1
tasa_sem1 <- delitos_semestral %>%
  select((1), (9), (13))

tasa_sem1 <- tasa_sem1 %>%
  spread(key = delito_ab, value = sem1_t_pob, fill = 0) %>% 
  group_by(cve_mun)

colnames(tasa_sem1)[2:22] <- paste0("tasa_sem1_", colnames(tasa_sem1)[2:22])

#Semestre2
tasa_sem2 <- delitos_semestral %>%
  select((1), (9), (14))

tasa_sem2 <- tasa_sem2 %>%
  spread(key = delito_ab, value = sem2_t_pob, fill = 0) %>% 
  group_by(cve_mun)

colnames(tasa_sem2)[2:22] <- paste0("tasa_sem2_", colnames(tasa_sem2)[2:22])

#unimos bases

semestral_fin <- Reduce(function(x,y) merge(x=x, y=y, by = "cve_mun"), list( delitos_cat_sem2, totales_sem1, tasa_sem1, totales_sem2,tasa_sem2))

semestral_fin <- semestral_fin %>% 
  select(3, 2, 5, 1, (6:111))

names(semestral_fin)[1] <- "estado"
names(semestral_fin)[2] <- "clave_ent"
names(semestral_fin)[3] <- "municipio"
names(semestral_fin)[5] <- "poblacion"

### agregamos fecha de actualización 

semestral_fin$fecha_act <- "Diciembre 2020"

### datos comparativos: maximos semestre actual###

tasa_max_sem2 <- delitos_indicadores %>%
  select((2:3), (5), (1), (9), (20))

tasa_max_sem2 <- tasa_max_sem2 %>% 
  group_by(delito_ab) %>% 
  slice(which.max(sem2_t_pob))

tasa_max_sem2 <- tasa_max_sem2 %>% 
  mutate(mun_max = paste0("El municipio con incidencia máxima es ", Municipio, ", ", Entidad, " con una tasa de ", sem2_t_pob, " por cada 100mil habs.")) 

tasa_max_sem2 <- tasa_max_sem2 %>% 
  select(9,11) %>% 
  spread(key = delito_ab, value = mun_max, fill = 0)

colnames(tasa_max_sem2)[1:21] <- paste0("mun_max_", colnames(tasa_max_sem2)[1:21])

#### unimos base anual final con municipios máximos

semestral_fin <- merge(semestral_fin, tasa_max_sem2)

#### promedio nacional de tasa anual

promedio_sem2 <- ddply(delitos_indicadores, .(delito_ab), function(x) mean(x$sem2_t_pob))

promedio_sem2 <- promedio_sem2 %>%
  mutate(promedio = round(V1, digits = 2)) %>% 
  select(1,3) %>% 
  spread(key = delito_ab, value = promedio, fill = 0)

colnames(promedio_sem2)[1:21] <- paste0("promedio_nac_", colnames(promedio_sem2)[1:21])

semestral_fin <- merge(semestral_fin, promedio_sem2)


#####Categoría semestral: Categoría crecimiento semestral por incidencia #####

delitos_indicadores_crec_sem <- left_join(delitos_indicadores, jenks_crec_sem, by = "delito_ab")

delitos_indicadores_crec_sem <- delitos_indicadores_crec_sem %>%
  select((1:9),16, (19:20),24, (27:32))  

delitos_indicadores_crec_sem <- as.data.frame(delitos_indicadores_crec_sem)

delitos_indicadores_crec_sem$categoria <- case_when(delitos_indicadores_crec_sem$t_crec_semestre<=delitos_indicadores_crec_sem$V6 & delitos_indicadores_crec_sem$t_crec_semestre>delitos_indicadores_crec_sem$V5 
                                                  ~ "Crecimiento muy alto", 
                                                  delitos_indicadores_crec_sem$t_crec_semestre<=delitos_indicadores_crec_sem$V5 & delitos_indicadores_crec_sem$t_crec_semestre>delitos_indicadores_crec_sem$V4 ~ "Crecimiento alto", 
                                                  delitos_indicadores_crec_sem$t_crec_semestre<=delitos_indicadores_crec_sem$V4 & delitos_indicadores_crec_sem$t_crec_semestre>delitos_indicadores_crec_sem$V3 ~ "Crecimiento medio", 
                                                  delitos_indicadores_crec_sem$t_crec_semestre<=delitos_indicadores_crec_sem$V3 & delitos_indicadores_crec_sem$t_crec_semestre> 0 ~ "Crecimiento bajo", 
                                                  delitos_indicadores_crec_sem$t_crec_semestre==0 
                                                  ~ "Sin cambio", 
                                                  delitos_indicadores_crec_sem$t_crec_semestre< 0 & delitos_indicadores_crec_sem$t_crec_semestre>delitos_indicadores_crec_sem$V2 ~ "Decrecimiento bajo",
                                                  delitos_indicadores_crec_sem$t_crec_semestre<=delitos_indicadores_crec_sem$V2 ~ "Decrecimiento alto")

#seleccionar y spreadear
#Categorías
delitos_cat_cs <- delitos_indicadores_crec_sem %>%
  select((1:5), (9:10), (20))

delitos_cat_cs <- delitos_cat_cs %>%
  spread(key = delito_ab, value = categoria, fill = 0) %>% 
  group_by(cve_mun)

colnames(delitos_cat_cs)[7:27] <- paste0("cat_cs_", colnames(delitos_cat_cs)[7:27])

#Tasas

#Semestre1
tasa_cs_sem1 <- delitos_indicadores_crec_sem %>%
  select((1), (9), (11))

tasa_cs_sem1 <- tasa_cs_sem1 %>%
  spread(key = delito_ab, value = sem1_t_pob, fill = 0) %>% 
  group_by(cve_mun)

colnames(tasa_cs_sem1)[2:22] <- paste0("tasa_sem1_", colnames(tasa_cs_sem1)[2:22])

#Semestre2
tasa_cs_sem2 <- delitos_indicadores_crec_sem %>%
  select((1), (9), (12))

tasa_cs_sem2 <- tasa_cs_sem2 %>%
  spread(key = delito_ab, value = sem2_t_pob, fill = 0) %>% 
  group_by(cve_mun)

colnames(tasa_cs_sem2)[2:22] <- paste0("tasa_sem2_", colnames(tasa_cs_sem2)[2:22])

#comparativa
tasa_cs <- delitos_indicadores_crec_sem %>%
  select((1), (9), (13))

tasa_cs <- tasa_cs %>%
  spread(key = delito_ab, value = t_crec_semestre, fill = 0) %>% 
  group_by(cve_mun)

colnames(tasa_cs)[2:22] <- paste0("tasa_cs_", colnames(tasa_cs)[2:22])

#unimos las bases

crec_sem_fin <- Reduce(function(x,y) merge(x=x, y=y, by = "cve_mun"), list( delitos_cat_cs, tasa_cs_sem1, tasa_cs_sem2, tasa_cs))

crec_sem_fin <- crec_sem_fin %>% 
  select(3, 2, 5, 1, (6:90))

names(crec_sem_fin)[1] <- "estado"
names(crec_sem_fin)[2] <- "clave_ent"
names(crec_sem_fin)[3] <- "municipio"
names(crec_sem_fin)[5] <- "poblacion"

### agregamos fecha de actualización 
crec_sem_fin$fecha_act <- "Diciembre 2020"

### datos comparativos: maximos año actual###

tasa_max_cs <- delitos_indicadores %>%
  select((2:3), (5), (1), (9), (24))

tasa_max_cs <- tasa_max_cs %>% 
  group_by(delito_ab) %>% 
  slice(which.max(t_crec_semestre))

tasa_max_cs <- tasa_max_cs %>% 
  mutate(mun_max = paste0("El municipio con mayor crecimiento es ", Municipio, ", ", Entidad, " con una tasa de ", t_crec_semestre, " por cada 100mil habs.")) 

tasa_max_cs <- tasa_max_cs %>% 
  select(9,11) %>% 
  spread(key = delito_ab, value = mun_max, fill = 0)

colnames(tasa_max_cs)[1:21] <- paste0("mun_max_", colnames(tasa_max_cs)[1:21])

#### unimos base anual final con municipios máximos

crec_sem_fin <- merge(crec_sem_fin, tasa_max_cs)

#### promedio nacional de tasa anual

promedio_crec_sem <- ddply(delitos_indicadores, .(delito_ab), function(x) mean(x$t_crec_semestre))

promedio_crec_sem <- promedio_crec_sem %>%
  mutate(promedio = round(V1, digits = 2)) %>% 
  select(1,3) %>% 
  spread(key = delito_ab, value = promedio, fill = 0)

colnames(promedio_crec_sem)[1:21] <- paste0("promedio_nac_", colnames(promedio_crec_sem)[1:21])

crec_sem_fin <- merge(crec_sem_fin, promedio_crec_sem)


##### Categorías mensuales: Categoria tasa mensual####

delitos_indicadores_mes <- left_join(delitos_indicadores, jenks_mes2, by = "delito_ab")

delitos_indicadores_mes <- delitos_indicadores_mes %>%
  select((1:9), (14:16),(21:22), (25:31))  

delitos_indicadores_mes <- as.data.frame(delitos_indicadores_mes)

delitos_indicadores_mes$categoria <- case_when(delitos_indicadores_mes$mes2_t_pob<=delitos_indicadores_mes$V6 & delitos_indicadores_mes$mes2_t_pob>delitos_indicadores_mes$V5 ~ "Muy alto", 
                                             delitos_indicadores_mes$mes2_t_pob<=delitos_indicadores_mes$V5 & delitos_indicadores_mes$mes2_t_pob>delitos_indicadores_mes$V4 ~ "Alto", 
                                             delitos_indicadores_mes$mes2_t_pob<=delitos_indicadores_mes$V4 & delitos_indicadores_mes$mes2_t_pob>delitos_indicadores_mes$V3 ~ "Medio", 
                                             delitos_indicadores_mes$mes2_t_pob<=delitos_indicadores_mes$V3 & delitos_indicadores_mes$mes2_t_pob>delitos_indicadores_mes$V2 ~ "Bajo", 
                                             delitos_indicadores_mes$mes2_t_pob<=delitos_indicadores_mes$V2 & delitos_indicadores_mes$mes2_t_pob>delitos_indicadores_mes$V1~ "Muy bajo",
                                             delitos_indicadores_mes$mes2_t_pob==delitos_indicadores_mes$V1 ~ "Sin incidencia")

#seleccionar y spreadear
#categoria
delitos_cat_mes <- delitos_indicadores_mes %>%
  select((1:5), 9, 12, 22)

delitos_cat_mes <- delitos_cat_mes %>%
  spread(key = delito_ab, value = categoria, fill = 0) %>% 
  group_by(cve_mun)

colnames(delitos_cat_mes)[7:27] <- paste0("cat_mes_", colnames(delitos_cat_mes)[7:27])

#Totales

#Mes 1
totales_mes1 <- delitos_indicadores_mes %>%
  select((1), (9), (10))

totales_mes1 <- totales_mes1 %>%
  spread(key = delito_ab, value = mes1, fill = 0) %>% 
  group_by(cve_mun)

colnames(totales_mes1)[2:22] <- paste0("total_mes1_", colnames(totales_mes1)[2:22])

#Mes 2
totales_mes2 <- delitos_indicadores_mes %>%
  select((1), (9), (11))

totales_mes2 <- totales_mes2 %>%
  spread(key = delito_ab, value = mes2, fill = 0) %>% 
  group_by(cve_mun)

colnames(totales_mes2)[2:22] <- paste0("total_mes2_", colnames(totales_mes2)[2:22])

#Tasas
#Mes1
tasa_mes1 <- delitos_indicadores_mes %>%
  select((1), (9), (13))

tasa_mes1 <- tasa_mes1 %>%
  spread(key = delito_ab, value = mes1_t_pob, fill = 0) %>% 
  group_by(cve_mun)

colnames(tasa_mes1)[2:22] <- paste0("tasa_mes1_", colnames(tasa_mes1)[2:22])

#Mes2
tasa_mes2 <- delitos_indicadores_mes %>%
  select((1), (9), (14))

tasa_mes2 <- tasa_mes2 %>%
  spread(key = delito_ab, value = mes2_t_pob, fill = 0) %>% 
  group_by(cve_mun)

colnames(tasa_mes2)[2:22] <- paste0("tasa_mes2_", colnames(tasa_mes2)[2:22])

#Comparativo

tasa_crec_mes <- delitos_indicadores_mes %>%
  select((1), (9), (15))

tasa_crec_mes <- tasa_crec_mes %>%
  spread(key = delito_ab, value = t_crec_mensual, fill = 0) %>% 
  group_by(cve_mun)

colnames(tasa_crec_mes)[2:22] <- paste0("tasa_cm_", colnames(tasa_crec_mes)[2:22])

#unimos la base de tasas anuales con su categor?a

mensual_fin <- Reduce(function(x,y) merge(x=x, y=y, by = "cve_mun"), list( delitos_cat_mes, totales_mes1, tasa_mes1, totales_mes2, tasa_mes2, tasa_crec_mes))

mensual_fin <- mensual_fin %>% 
  select(3, 2, 5, 1, (6:132))

names(mensual_fin)[1] <- "estado"
names(mensual_fin)[2] <- "clave_ent"
names(mensual_fin)[3] <- "municipio"
names(mensual_fin)[5] <- "poblacion"

### agregamos fecha de actualización 
mensual_fin$fecha_act <- "Diciembre 2020"

### datos comparativos: maximos año actual###

tasa_max_mes <- delitos_indicadores %>%
  select((2:3), (5), (1), (9), (22))

tasa_max_mes <- tasa_max_mes %>% 
  group_by(delito_ab) %>% 
  slice(which.max(mes2_t_pob))

tasa_max_mes <- tasa_max_mes %>% 
  mutate(mun_max = paste0("El municipio con incidencia máxima es ", Municipio, ", ", Entidad, " con una tasa de ", mes2_t_pob, " por cada 100mil habs.")) 

tasa_max_mes <- tasa_max_mes %>% 
  select(9,11) %>% 
  spread(key = delito_ab, value = mun_max, fill = 0)

colnames(tasa_max_mes)[1:21] <- paste0("mun_max_", colnames(tasa_max_mes)[1:21])

#### unimos base anual final con municipios máximos

mensual_fin <- merge(mensual_fin, tasa_max_mes)

#### promedio nacional de tasa mensual

promedio_mes2 <- ddply(delitos_indicadores, .(delito_ab), function(x) mean(x$mes2_t_pob)) 

promedio_mes2 <- promedio_mes2 %>%
  mutate(promedio = round(V1, digits = 2)) %>% 
  select(1,3) %>% 
  spread(key = delito_ab, value = promedio, fill = 0)

colnames(promedio_mes2)[1:21] <- paste0("promedio_nac_", colnames(promedio_mes2)[1:21])

mensual_fin <- merge(mensual_fin, promedio_mes2)

### exportamos base

datos_mensuales <- mensual_fin %>% 
  select(1:132)

write.csv(datos_mensuales, file="datos_mensuales_sep_2020.csv", fileEncoding ="UTF-8")



## datos crecimiento anual ##
carto_JSON_crec_anual <- left_join(mun_mex2018,crec_anual_fin,
                              by ="cve_mun")

names(carto_JSON_crec_anual) <- tolower(names(carto_JSON_crec_anual))
write.csv(carto_JSON_crec_anual, file = "carto_JSON_crec_anual.csv",
          row.names = FALSE)

###### Cartografía#####
p_load(rgdal)

cartografia_municipal <- readOGR(dsn="/Users/aritzy/Documents/Rizika/Incidencia/mun_mex2018", layer= "mun_mex2018")

#Cartografía anual por incidencia
carto_anual <- merge(cartografia_municipal, anual_fin, by.x="CVEGEO", by.y="cve_mun")

carto_JSON_anual <- writeOGR(carto_anual, "carto_JSON_anual.GeoJSON", layer = "carto_anual", driver="GeoJSON")

#Cartografía crecimiento anual por incidencia 
carto_crec_anual <- merge(cartografia_municipal, crec_anual_fin, by.x="CVEGEO", by.y="cve_mun")

carto_JSON_crec_anual <- writeOGR(carto_crec_anual, "carto_JSON_crec_anual.GeoJSON", layer = "carto_crec_anual", driver="GeoJSON")

#Cartografía semestral por incidencia
carto_semestral <- merge(cartografia_municipal, semestral_fin, by.x="CVEGEO", by.y="cve_mun")

carto_JSON_semestral <- writeOGR(carto_semestral, "carto_JSON_semestral.GeoJSON", layer = "carto_semestral", driver="GeoJSON")

#Cartografía crecimiento semestral por incidencia
carto_crec_sem <- merge(cartografia_municipal, crec_sem_fin, by.x="CVEGEO", by.y="cve_mun")

carto_JSON_crec_sem <- writeOGR(carto_crec_sem, "carto_JSON_crec_sem.GeoJSON", layer = "carto_crec_sem", driver="GeoJSON")

#Cartografía mensual por incidencia
carto_mensual <- merge(cartografia_municipal, mensual_fin, by.x="CVEGEO", by.y="cve_mun")

carto_JSON_mensual <- writeOGR(carto_mensual, "carto_JSON_mensual.GeoJSON", layer = "carto_mensual", driver="GeoJSON")


#####Cartografía por bien jurídico####

#Cartografía anual 
carto_anual_bien <- merge(cartografia_municipal, bienes_anual, by.x="CVEGEO", by.y="cve_mun")

bien_JSON_anual <- writeOGR(carto_anual_bien, "bien_JSON_anual.GeoJSON", layer = "carto_anual_bien", driver="GeoJSON")

#Cartografía crecimiento anual 
carto_crec_anual_bien <- merge(cartografia_municipal, bienes_crec_anual, by.x="CVEGEO", by.y="cve_mun")

bien_JSON_crec_anual <- writeOGR(carto_crec_anual_bien, "bien_JSON_crec_anual.GeoJSON", layer = "carto_crec_anual_bien", driver="GeoJSON")

#Cartografía semestral
carto_semestral_bien <- merge(cartografia_municipal, bienes_semestral, by.x="CVEGEO", by.y="cve_mun")

bien_JSON_semestral <- writeOGR(carto_semestral_bien, "bien_JSON_semestral.GeoJSON", layer = "carto_semestral_bien", driver="GeoJSON")

#Cartografía crecimiento semestral 
carto_crec_sem_bien <- merge(cartografia_municipal, bienes_crec_sem, by.x="CVEGEO", by.y="cve_mun")

bien_JSON_crec_sem <- writeOGR(carto_crec_sem_bien, "bien_JSON_crec_sem.GeoJSON", layer = "carto_crec_sem_bien", driver="GeoJSON")


#####Comparativos para bien jurídico####

####Crecimiento anual por bien jurídico###
tasa_b_max_ca <- Juridico_final %>%
  select((1:5), (19))

tasa_b_max_ca <- tasa_b_max_ca %>% 
  group_by(bien_ab) %>% 
  slice(which.max(t_crec_anual))

write.csv(tasa_b_max_ca, file="t_crec_anual_max_bienes_mayo.csv", fileEncoding ="UTF-8")

##### Semestral por bien jurídico###
tasa_b_max_sem <- Juridico_final %>%
  select((1:5), (16))

tasa_b_max_sem <- tasa_b_max_sem %>% 
  group_by(bien_ab) %>% 
  slice(which.max(sem2_t_pob))

write.csv(tasa_b_max_sem, file="t_sem_max_bienes_mayo.csv", fileEncoding ="UTF-8")

##### crecimiento semestral por bien jur?dico###
tasa_b_max_cs <- Juridico_final %>%
  select((1:5), (20))

tasa_b_max_cs <- tasa_b_max_cs %>% 
  group_by(bien_ab) %>% 
  slice(which.max(t_crec_semestre))

write.csv(tasa_b_max_cs, file="t_crec_sem_max_bienes_mayo.csv", fileEncoding ="UTF-8")

####Promedio nacional por delito####

#Para los bienes jur?dicos

promedio_crecimiento_b <- ddply(delitos_indicadores, .(bien_ab), function(x) mean(x$t_crec_anual))
names(promedio_crecimiento_b)[2] <- "promedio_crec_anual"

promedio_sem2_b <- ddply(delitos_indicadores, .(bien_ab), function(x) mean(x$sem2_t_pob))
names(promedio_sem2_b)[2] <- "promedio_semestral"

promedio_crec_sem_b <- ddply(delitos_indicadores, .(bien_ab), function(x) mean(x$t_crec_semestre))
names(promedio_crec_sem_b)[2] <- "promedio_crec_semestral"

promedios_comparativos_b <- Reduce(function(x,y) merge(x=x, y=y, by = "bien_ab"), list( promedio_ano2_b, promedio_crecimiento_b, promedio_sem2_b, promedio_crec_sem_b ))

write.csv(promedios_comparativos_b, file="t_comp_promedio_bienes.csv", fileEncoding ="UTF-8")


##### Jenks ARO#####

jenks_ano2_aro <- ddply(delitos_indicadores, .(delito_ab), function(x) getJenksBreaks(x$ano2_t_pob, 5))

write.csv(jenks_ano2_aro, file="jenks_anual_ARO_sep_2020.csv", fileEncoding ="UTF-8")

jenks_crecimiento <- ddply(delitos_indicadores, .(delito_ab), function(x) getJenksBreaks(x$t_crec_anual, 5))

write.csv(jenks_crecimiento, file="jenks_crec_anual_ARO_sep_2020.csv", fileEncoding ="UTF-8")

jenks_sem2 <- ddply(delitos_indicadores, .(delito_ab), function(x) getJenksBreaks(x$sem2_t_pob, 5))

write.csv(jenks_sem2, file="jenks_semestral_ARO_sep_2020.csv", fileEncoding ="UTF-8")

jenks_crec_sem <- ddply(delitos_indicadores, .(delito_ab), function(x) getJenksBreaks(x$t_crec_semestre, 5))

write.csv(jenks_crec_sem, file="jenks_crec_sem_ARO_sep_2020.csv", fileEncoding ="UTF-8")

jenks_mes2 <- ddply(delitos_indicadores, .(delito_ab), function(x) getJenksBreaks(x$mes2_t_pob, 5))

write.csv(jenks_mes2, file="jenks_mensual_ARO_sep_2020.csv", fileEncoding ="UTF-8")

