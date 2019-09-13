#############################################################
## P.E. Pérez Estigarribia <pestigarribia@est.pol.una.py> #
#############################################################
# Este archivo .R tiene como objetivo limpiar y ordenar     #
# sub conjunto  de datos y combinarlos en un unico dataset. #
############################################################

##$ Estos son algunos enlaces y lecturas recomendadas $##
browseURL("https://www.tidyverse.org/") # Abre el enlace en su navegador web
browseURL("https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html")
browseURL("https://tereom.github.io/est-computacional-2018/datos-limpios.html")
browseURL("https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf")


# Se asigna un directorio de trabajo
setwd("C:/R/TopaDengue/data_transformation_R")

# A continuación en el caso de ser necesario se instalan y activan paquetes

##$ Paquetes para manipular datos $##
if (!require('tidyverse')) install.packages("tidyverse")
library(tidyverse)
if (!require('dplyr')) install.packages('dplyr')
library(dplyr)


##$ Se traen los datos desde un directorio al Global Enviroment del IDE $##

# datos en formato csv con encabezado y separadores ; para las columnas 

inspections =  read.table("inspections_summary_view_nodupl.csv",header=T, sep=";",dec=".") 

negativos   =  read.table("registro_comunitario_negativos.csv",header=T, sep=";",dec=".") 

senepa      =  read.table("relevamientos_senepa_recipiente.csv",header=T, sep=";",dec=".") 

insp_loc    =  read.table("inspections_locations.csv",header=T, sep=";",dec=".") 

relev_loc   =  read.table("locations_control_aut_ass_nodup.csv",header=T, sep=";",dec=".") 

clima       =  read.table("Climate_Asuncion_Aeropuerto.csv",header=T, sep=";",dec=".") 

criaderos   =  read.table("referencias_tipo_de_criadero.csv",header=T, sep=";",dec=".") 

##$ Algunas funciones basicas para chequear los datos 

help(read.table) # Devuelve una ventana de ayuda sobre la funcion 

class(inspections) # Devuelve la clase de objeto que se ha creado 

dim(negativos) # Devuelve la dimension del dataframe en filas y columnas

summary(inspections$status) # Entrega un resumen estadistico de una var de interes

names(insp_loc) # devuelve el nombre de las columnas del data.frame

str(clima) # muestra la estructura del dataframe

head(inspections[10:14], n = 10) # devuelve las 10 primeras filas de la columna 10 a 14

tail(negativos[1:5], n = 5) # devuelve las 10 filas ultimas de la columna 10 a 14

View(senepa) # Con esto se puede ver los datos en una tabla 

# Esto es solo un ejemplo de uso de pipes con dplyr
inspections$status %>% summary() %>% class()
# Esto hace lo mismo en la sintaxis de R 
a=inspections$status
b=summary(a)
c=class(b)
c

########################################################
# Preprocesamiento del conjunto de datos de TopaDengue
#######################################################

setequal(inspections, negativos) # Use setequal() to test whether two data sets
# contain the exact same rows (in any order).

names(negativos)

## Se unen filas de datos DengueTopa positivos y negativos

fullinspection=bind_rows(inspections, negativos)

dim(inspections)
dim(negativos)
dim(fullinspection)

str(fullinspection)

View(fullinspection)

## Se agregan datos geoespaciales a full denguetopa


fullinsploc=full_join(fullinspection,  insp_loc, by = "location_id")


dim(fullinspection)
dim(insp_loc)
dim(fullinsploc)

str(fullinsploc)

View(fullinsploc)

fullinsploc$location_city_block =as.factor(fullinsploc$location_city_block)

summary(fullinsploc$location_city_block)

# Se guarda el dataframe en el directorio 
write.csv(fullinsploc, file="full_inspection_and_location.csv")

## Se verifica a ojo y realiza correciones. En la linea siguiente se vuelve a recuperar en el Enviroment el mismo dataframe editado manualmente.
fullinsploc_v1 <-  read.table("full_inspection_and_location_v1.csv",header=T, sep=";",dec=".")

#########################################################################################
# Preprocesamiento conjunto de datos SENEPA
#######################################################################################

## Se agregan los datos geoespaciales al dataframe de senepa

names(senepa)

names(relev_loc)

senepaloc=full_join(senepa,  relev_loc, by = c("id_predio_.manz.nrocasa.apell."="address",
                                               "manzana"))

dim(senepa)
dim(relev_loc)
dim(senepaloc)

str(senepaloc)

View(senepaloc)

senepaloc$manzana.y =as.factor(senepaloc$manzana.y)

summary(senepaloc$manzana.y)


# Se guarda el dataframe en el directorio 
write.csv(senepaloc, file="relevamiento_senepa_and_location.csv")## Se verifica a ojo y realiza correciones. En la linea siguiente se vuelve a recuperar en el Enviroment el mismo dataframe editado manualmente.


relevloc_v1 <-  read.table("relevamiento_senepa_and_location_v1.csv",header=T, sep=";",dec=".")

names(relevloc_v1)

################################################
#  Union de dataset dengueTopa con SENEPA
################################################

# Para poder hacerlo se requiere establecer una biyeccion 
# entre al menos algunas columnas de datos que representen 
# las mimas variables, estas pueden tener o no el mismo nombre. 

# full_join(x, y, by = NULL, copy=FALSE, suffix=c(".x",".y"),.)
# Join data. Retain all values, all rows.


names(fullinsploc_v1)

full_data=full_join(fullinsploc_v1, relevloc_v1,
                     by = c("id_observation", 
                            "consortium",
                            "location_key"="id_predio_manz_nrocasa_apell",
                            "zona"="zona_topadengue_funcion_a_barrio",
                            "location_city_block"="manzana",
                            "location_id"="location_id_senepa",
                            "visit_date"="fecha",
                            "visit_year"="y",
                            "visit_month"="m",
                            "visit_day"="d",
                            "barrio_topadengue"="barrio_topadengue_s_manzana",
                            "predio_tipo"="location_type",
                            "lat_corregido"="lat",
                            "lon_corregido"="lon",
                            "breeding_site_code"="tipo_criadero_denguechat",
                            "breeding_site_amount"="cant_contenedores",
                            "breeding_site_description"="descripcion_cont_positivo",
                            "sub_dataset"="sub_dataset",
                            "source_file"="source_file"))

###########################################################



full_data=full_join(full_join(bind_rows(inspections, negativos),  insp_loc, by = "location_id"), 
                    full_join(senepa,  relev_loc, by = c("id_predio_manz_nrocasa_apell"="address")),
                    by = c("id_observation", 
                           "consortium",
                           "location_key"="id_predio_manz_nrocasa_apell",
                           "zona"="zona_topadengue_funcion_a_barrio",
                           "location_city_block"="manzana",
                           "location_id"="location_id_senepa",
                           "visit_date"="fecha",
                           "visit_year"="y",
                           "visit_month"="m",
                           "visit_day"="d",
                           "barrio_topadengue"="barrio_topadengue_s_manzana",
                           "predio_tipo"="location_type",
                           "lat_corregido"="lat",
                           "lon_corregido"="lon",
                           "breeding_site_code"="tipo_criadero_denguechat",
                           "breeding_site_amount"="cant_contenedores",
                           "breeding_site_description"="descripcion_cont_positivo",
                           "sub_dataset"="sub_dataset",
                           "source_file"="source_file"))

#####################################################

dim(fullinsploc_v1)
dim(relevloc_v1)
dim(full_data)


# Se guarda el dataframe en el directorio 
write.csv(full_data, file="full_TopaDengue_SENEPA.csv")

## Se verifica a ojo y realiza correciones. En la linea siguiente se vuelve a recuperar en el Enviroment el mismo dataframe editado manualmente.
full_data2 <-  read.table("full_TopaDengue_SENEPA_v1.csv",header=T, sep=";",dec=".")

##############################################################
# Por ultimo se combinan los datos senepaDengueTopa con
# datos climaticos 
# Datos reportados por la estación meteorológica: 
# 862180 (SGAS) | Registros
# Latitud: -25.26 | Longitud: -57.63 | Altitud: 101
###############################################################

clima2=rename(clima, visit_date = date)

full_dataclima=full_join(full_data2, clima2, by = "visit_date")

names(full_dataclima)

head(full_dataclima[67:70], n = 10)

dim(full_dataclima)

write.csv(full_dataclima, file="full_senepa_denguetopa_clima.csv")


full_data3 <-  read.table("full_senepa_denguetopa_clima_v2.csv",header=T, sep=";",dec=".")




