#############################################################
## Proyecto TopaDengue
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

#View(senepa) # Con esto se puede ver los datos en una tabla 

# Esto es solo un ejemplo de uso de pipes con dplyr
inspections$status %>% summary() %>% class()
# Esto hace lo mismo en la sintaxis de R 
a=inspections$status
b=summary(a)
c=class(b)
c

########################################################
# Inspección y unio de conjunto de datos de TopaDengue
#######################################################

setequal(inspections, negativos) # Use setequal() to test whether two data sets
# contain the exact same rows (in any order).

names(negativos)
names(inspections)

## Se unen filas de datos DengueTopa positivos y negativos

fullinspection=bind_rows(inspections, negativos)

dim(inspections)
dim(negativos)
dim(fullinspection)

#View(fullinspection)

names(fullinspection)

#summary(fullinspection$location_key)

#summary(fullinspection$status)

## Se agregan datos geoespaciales a full denguetopa

names(fullinspection)

names(insp_loc)

# devuelve las primeras filas de las columnas 1 al 4

head(insp_loc[,1:4])

head(fullinspection[,4:6])

# Agrega los datos espaciales al df inspecciones positivas y negativas 

fullinsploc=full_join(fullinspection,  insp_loc[,1:15], by = c("location_id","consortium"))

# devuelve las dimenciones de los df 
dim(fullinspection)
dim(insp_loc)
dim(fullinsploc)

# ver el nuevo data.frame
#View(fullinsploc)

# inspeccionar un unico campo de datos numericos tal que sean tratados como factores 
as.factor(fullinsploc$location_city_block)  %>% summary()

########################################################
# Inspección y unio de conjunto de datos de SENEPA
#######################################################

## Se agregan los datos geoespaciales al dataframe de senepa

names(senepa)

names(relev_loc)

senepaloc=full_join(senepa,  relev_loc[,1:9], by = c("id_predio_.manz.nrocasa.apell."="address",
                                               "manzana", "consortium"))
dim(senepa)
dim(relev_loc)
dim(senepaloc)
names(senepaloc)



names(senepaloc)

as.factor(senepaloc$manzana) %>% summary()

################################################
#  Union de dataset dengueTopa con SENEPA
################################################

# Para poder hacerlo se requiere establecer una biyeccion 
# entre al menos algunas columnas de datos que representen 
# las mimas variables, estas pueden tener o no el mismo nombre. 

# full_join(x, y, by = NULL, copy=FALSE, suffix=c(".x",".y"),.)
# Join data. Retain all values, all rows.

names(fullinsploc)

names(senepaloc)


# Estas columnas de datos son consideradas iguales entre TopaDengue y senepa 

col.int = c("id_observation", 
               "consortium",
               "location_key"="id_predio_.manz.nrocasa.apell.",
               "zona"="zona_topadengue_.funcion_a_barrio.",
               "location_city_block"="manzana",
               "location_id"="location_id_senepa",
               "visit_date"="fecha",
               "visit_day"= "d",
               "visit_month"= "m",
               "visit_year"= "y",
               "visity_week_of_year"="iso.semana.y",
               "barrio"="barrio_topadengue_.s._manzana.",
               "tipo"="location_type",
               "lat_corregido"="lat",
               "lon_corregido"="lon",
               "breeding_site_code"="tipo_criadero_denguechat",
               "breeding_site_amount"="cant_contenedores",
               "breading_site_positive"="contenedores_positivos",
               "breeding_site_description"="descripcion_cont_positivo",
               "sub_dataset"="sub_dataset",
               "source_file"="source_file")


full_data=full_join(fullinsploc, senepaloc,
                    by = col.int)


#summary(full_data$id_observation)

dim(fullinsploc)

dim(senepaloc)

dim(full_data)


##############################################################
# Por ultimo se combinan los datos senepaDengueTopa con
# datos climaticos 
# Datos reportados por la estación meteorológica: 
# 862180 (SGAS) | Registros
# Latitud: -25.26 | Longitud: -57.63 | Altitud: 101
###############################################################

names(clima)

names(full_data)

clima2=rename(clima, visit_date = date)

full_dataclima=full_join(full_data,clima2, by = "visit_date")

names(full_dataclima)

head(full_dataclima[67:70], n = 10)


dim(clima)
dim(full_data)
dim(full_dataclima)

View(full_dataclima)

write.csv(full_dataclima, file="full_senepa_denguetopa_clima.csv")

###########################################################

nomissfulldf = data.frame(full_dataclima$id_observation,
                          full_dataclima$id_observation,
                          full_dataclima$consortium,
                          full_dataclima$location_key,
                          full_dataclima$zona,
                          full_dataclima$location_city_block,
                          full_dataclima$location_id,
                          full_dataclima$visit_date,
                          full_dataclima$visit_year,
                          full_dataclima$visit_month,
                          full_dataclima$visit_day,
                          full_dataclima$visit_day_of_week,
                          full_dataclima$visity_week_of_year,
                          full_dataclima$barrio,
                          full_dataclima$tipo,
                          full_dataclima$lat_corregido,
                          full_dataclima$lon_corregido,
                          full_dataclima$altitude,
                          full_dataclima$breeding_site_code,
                          full_dataclima$breeding_site_amount,
                          full_dataclima$breading_site_positive,
                          full_dataclima$breeding_site_description,
                          full_dataclima[,73:92],
                          full_dataclima$weather_station,
                          ID_LOCSENEPA=full_dataclima$ï..ID_LOCSENEPA,
                          full_dataclima$sub_dataset,
                          full_dataclima$source_file)

View(nomissfulldf)

write.csv(nomissfulldf, file="full_join_senepa_denguetopa_clima.csv")


