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

if (!require('visdat')) install.packages('visdat')
library(visdat)



if (!require('Amelia')) install.packages("Amelia")
library(Amelia)

if (!require('mice')) install.packages("mice")
library(mice)

if (!require('mice')) install.packages("mice")
library(mice)

if (!require('lattice')) install.packages("lattice")
library(lattice)


##$ Se traen los datos desde un directorio al Global Enviroment del IDE $##

# datos en formato csv con encabezado y separadores ; para las columnas 

## TodaDengue data 

inspections =  read.table("inspections_summary_view_nodupl.csv",header=T, sep=";",dec=".") 

negativos   =  read.table("registro_comunitario_negativos.csv",header=T, sep=";",dec=".") 


## Senepa data 

senepa      =  read.table("relevamientos_senepa_recipiente.csv",header=T, sep=";",dec=".") 


## cleaning and tidying location data (by Carlos G.)

loc_tpd   =  read.table("Loc_tpd_cleaning_and_tidying_byCarlos.csv",header=T, sep=";",dec=".")

loc_sen   =  read.table("Loc_viviendas_senepa_cleaning_and_tidying_byCarlos.csv",header=T, sep=";",dec=".")

## Climate data Asunción Aeropuesto 

clima       =  read.table("Climate_Asuncion_Aeropuerto.csv",header=T, sep=";",dec=".") 

## Other 

criaderos   =  read.table("referencias_tipo_de_criadero.csv",header=T, sep=";",dec=".") 

SensoresRemotos = read.table("SR_spectral_data_yearweekloc.csv",header=T, sep=";",dec=".")


##$ Algunas funciones basicas para chequear los datos 

help(read.table) # Devuelve una ventana de ayuda sobre la funcion 

class(inspections) # Devuelve la clase de objeto que se ha creado 

dim(negativos) # Devuelve la dimension del dataframe en filas y columnas

summary(inspections$status) # Entrega un resumen estadistico de una var de interes


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


#clima$date=as.Date(clima$date)

clima$RA=as.factor(clima$RA)

clima$SN=as.factor(clima$SN)

clima$GR=as.factor(clima$GR)

clima$TS=as.factor(clima$TS)

clima$TR=as.factor(clima$TR)

clima$FG=as.factor(clima$FG)


########################################################
# Inspección y union de conjunto de datos de TopaDengue
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

fullinspection %>%  vis_dat()

#summary(fullinspection$location_key)

#summary(fullinspection$status)

#####################################################
## Se agregan datos geoespaciales a full denguetopa
#####################################################

names(fullinspection)

names(loc_tpd)

# devuelve las primeras filas de las columnas 1 al 4

head(loc_tpd[,1:4])

head(fullinspection[,4:6])

# Agrega los datos espaciales al df inspecciones positivas y negativas 

fullinsploc=full_join(fullinspection,  loc_tpd[,2:11], by = c("location_id",
                                                               "location_city_block"="codigo_manz_tpd"))

#View(fullinsploc)


names(fullinsploc)

fullinsploc2=fullinsploc[!is.na(fullinsploc$id_observation),]

fullinsploc %>%  vis_dat()

fullinsploc2 %>%  vis_dat()

# A nice trick that you can use is to transpose your data frame and then check for duplicates.
duplicated(t(fullinsploc2))

# devuelve las dimenciones de los df 
dim(fullinspection)
dim(loc_tpd)
dim(fullinsploc)
dim(fullinsploc2)

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('fullinsp and Loc visualisation of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
fullinsploc2 %>%  vis_dat()
dev.off()

# ver el nuevo data.frame
#View(fullinsploc)

# inspeccionar un unico campo de datos numericos tal que sean tratados como factores 
as.factor(fullinsploc2$location_city_block)  %>% summary()

########################################################
# Inspección y unio de conjunto de datos de SENEPA
#######################################################

## Se agregan los datos geoespaciales al dataframe de senepa

names(senepa)

names(loc_sen)

senepa %>%  vis_dat()

loc_sen %>%  vis_dat()

senepa$id_location_senepa = as.factor(senepa$id_location_senepa)

loc_sen$id_location_senepa = as.factor(loc_sen$id_location_senepa)


senepaloc=full_join(senepa,  loc_sen[,2:12], by = c("id_predio"))


senepaloc %>%  vis_dat()

senepaloc2=senepaloc[!is.na(senepaloc$id_observation),]

# Eliminar filas que sobran 

senepaloc2 %>%  vis_dat()

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('senepaloc2 visualisation of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
senepaloc2 %>%  vis_dat()
dev.off()



dim(senepa)
dim(loc_sen)
dim(senepaloc)
dim(senepaloc2)


as.factor(senepaloc2$manzana) %>% summary()

################################################
#  Union de dataset dengueTopa con SENEPA
################################################

# Para poder hacerlo se requiere establecer una biyeccion 
# entre al menos algunas columnas de datos que representen 
# las mimas variables, estas pueden tener o no el mismo nombre. 

# full_join(x, y, by = NULL, copy=FALSE, suffix=c(".x",".y"),.)
# Join data. Retain all values, all rows.

names(fullinsploc2)

names(senepaloc2)

fullinsploc2$location_id = as.factor(fullinsploc2$location_id)

senepaloc2$id_location_senepa.y= as.factor(senepaloc2$id_location_senepa.y)


# Estas columnas de datos son consideradas iguales entre TopaDengue y senepa 

col.int = c("id_observation", 
            "consortium",
            "Interv"="Eval",
            "location_key"="id_predio",
            "zona"="zona_topadengue_.funcion_a_barrio.",
            "location_city_block"="manzana",
            "manz_censo",
            "location_id"="id_location_senepa.y",
            "visit_date"="fecha",
            "visit_day"= "d",
            "visit_month"= "m",
            "visit_year"= "y",
            "visity_week_of_year"="iso.semana.y",
            "barrio_tpd"="barrio_topadengue_.s._manzana.",
            "barrio_sen"="barrio_senepa",
            "barrio_cen",
            "Y",
            "X",
            "breeding_site_code"="tipo_criadero_denguechat",
            "breeding_site_amount"="cant_contenedores",
            "breading_site_positive"="contenedores_positivos",
            "breeding_site_description"="descripcion_cont_positivo",
            "breeding_site_larvae"="larvas_.potenciales.",
            "breading_site_pupae"="pupas",
            "sub_dataset"="sub_dataset",
            "source_file"="source_file")

full_data=full_join(fullinsploc2, senepaloc2,
                    by = col.int)

full_data %>%  vis_dat()

full_data.df = data.frame(id_observation=full_data$id_observation, 
                          consortium=full_data$consortium,
                          period=full_data$Interv,
                          location_key=full_data$location_key,
                          zona=full_data$zona,
                          location_city_block=full_data$location_city_block,
                          city_block_censo=full_data$manz_censo,
                          location_id=full_data$location_id,
                          visit_date=full_data$visit_date,
                          visit_day=full_data$visit_day,
                          visit_month=full_data$visit_month,
                          visit_year=full_data$visit_year,
                          visity_week_of_year=full_data$visity_week_of_year,
                          barrio_tpd=full_data$barrio_tpd,
                          barrio_senepa=full_data$barrio_sen,
                          barrio_censo=full_data$barrio_cen,
                          Y=full_data$Y,
                          X=full_data$X,
                          breeding_site_code_tpd=full_data$breeding_site_code,
                          breeding_site_code_senepa=full_data$tipo_criadero_SENEPA,
                          breeding_site_amount=full_data$breeding_site_amount,
                          breading_site_positive=full_data$breading_site_positive,
                          breeding_site_description=full_data$breeding_site_description,
                          breeding_site_larvae=full_data$breeding_site_larvae,
                          status=full_data$status,
                          sub_dataset=full_data$sub_dataset,
                          source_file=full_data$source_file)

full_data.df %>%  vis_dat()

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('full_data visualisation of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
full_data.df %>%  vis_dat()
dev.off()




#summary(full_data$id_observation)

dim(fullinsploc2)

dim(senepaloc2)

dim(full_data)

dim(full_data.df)


##############################################################
# Por ultimo se combinan los datos senepaDengueTopa con
# datos climaticos 
# Datos reportados por la estación meteorológica: 
# 862180 (SGAS) | Registros
# Latitud: -25.26 | Longitud: -57.63 | Altitud: 101
###############################################################

names(clima)

names(full_data)



# Se cambian algunos nombres de columnas a nombres más operativos

clima3=clima %>%  
  rename(d=D,
         m=M,                                   
         y=Y)

names(clima3)

sapply(full_data.df, class)

clima3$visit_date=as.factor(clima3$visit_date)

sapply(clima3, class)

full_dataclima=full_join(full_data.df,clima3, by = c("visit_date")) 

full_dataclima %>%  vis_dat()

full_dataclima2=full_dataclima[!is.na(full_dataclima$id_observation),]


full_dataclima3=subset(full_dataclima2, select = -c(d, m, y))

full_dataclima3 %>%  vis_dat()

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('full_data_clima visualisation of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
full_dataclima3 %>%  vis_dat()
dev.off()


dim(clima3)
dim(full_data.df)
dim(full_dataclima3)


#write.csv(full_dataclima3, file="full_senepa_denguetopa_clima.csv")

###########################################################

full_dataclima3$breading_site_positive = as.factor(full_dataclima3$breading_site_positive)

summary(full_dataclima3$breading_site_positive)

##################################################


#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('datos sensores remotos visualisation of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
SensoresRemotos  %>%  vis_dat()
dev.off()


full_dataclima4 = as_tibble(full_dataclima3)

full_dataclima5= mutate(full_dataclima4, loc_week_y = paste(full_dataclima4$location_id,full_dataclima4$visity_week_of_year,full_dataclima4$visit_year, sep = "_"))

full_dataclima6=as.data.frame(full_dataclima5)  


full_dataclima.SensoresRemotos=full_join(SensoresRemotos,full_dataclima6, by = c("loc_week_y")) 

dim(full_dataclima6)
dim(SensoresRemotos)
dim(full_dataclima.SensoresRemotos)

full_dataclima.SensoresRemotos2=full_dataclima.SensoresRemotos[!is.na(full_dataclima.SensoresRemotos$id_observation),]

dim(full_dataclima6)
dim(SensoresRemotos)
dim(full_dataclima.SensoresRemotos2)

full_dataclima.SensoresRemotos2$FG=as.factor(full_dataclima.SensoresRemotos2$FG)
full_dataclima.SensoresRemotos2$RA=as.factor(full_dataclima.SensoresRemotos2$RA)
full_dataclima.SensoresRemotos2$SN=as.factor(full_dataclima.SensoresRemotos2$SN)
full_dataclima.SensoresRemotos2$GR=as.factor(full_dataclima.SensoresRemotos2$GR)
full_dataclima.SensoresRemotos2$TS=as.factor(full_dataclima.SensoresRemotos2$TS)
full_dataclima.SensoresRemotos2$TR=as.factor(full_dataclima.SensoresRemotos2$TR)
full_dataclima.SensoresRemotos2$loc_week_y=as.factor(full_dataclima.SensoresRemotos2$loc_week_y)
full_dataclima.SensoresRemotos2$status=as.factor(full_dataclima.SensoresRemotos2$status)

full_dataclima.SensoresRemotos2 %>%  vis_dat()

missmap(full_dataclima.SensoresRemotos2, col=c('grey', 'steelblue'), y.cex=.2, x.cex=0.8)

summary(full_dataclima.SensoresRemotos2$status)


sort(sapply(full_dataclima.SensoresRemotos2, function(x) { sum(is.na(x)) }), decreasing=TRUE)

# Porcentaje de datos perdidos 

sort(sapply(full_dataclima.SensoresRemotos2, 
            function(x) {(sum(is.na(x))/dim(full_dataclima.SensoresRemotos2)[1])*100}), 
            decreasing=TRUE)

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('datos senepa topadengue impClima sensores remotos visualisation of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
full_dataclima.SensoresRemotos2 %>%  vis_dat()
dev.off()

names(full_dataclima.SensoresRemotos2)

dd.df3=full_dataclima.SensoresRemotos2

dd.df=mutate(dd.df3, location_city_block=paste('l',dd.df3$location_city_block, sep="_"), 
                   city_block_censo=paste('l',dd.df3$city_block_censo, sep="_"),
                   location_id=paste('l',dd.df3$location_id, sep="_"))


head(dd.df$location_city_block)

head(dd.df$city_block_censo)

head(dd.df$location_id)

dim(dd.df)

full.df = data.frame(source_file=dd.df$source_file, sub_dataset=dd.df$sub_dataset,
                     id_observation=dd.df$id_observation, consortium=dd.df$consortium, period=dd.df$period, 
                     loc_week_y=dd.df$loc_week_y, location_key=dd.df$location_key, zona=dd.df$zona,  
                     location_city_block=dd.df$location_city_block, city_block_censo=dd.df$city_block_censo, 
                     location_id=dd.df$location_id,
                     barrio_tpd=dd.df$barrio_tpd, barrio_senepa=dd.df$barrio_senepa, 
                     barrio_censo=dd.df$barrio_censo,
                     Y=dd.df$Y, X=dd.df$X, visit_date=dd.df$visit_date,
                     visit_day=dd.df$visit_day, visit_month=dd.df$visit_month, 
                     visit_year=dd.df$visit_year,
                     visity_week_of_year=dd.df$visity_week_of_year, 
                     breeding_site_code_tpd=dd.df$breeding_site_code_tpd,   
                     breeding_site_code_senepa=dd.df$breeding_site_code_senepa, 
                     breeding_site_amount=dd.df$breeding_site_amount,
                     B4x1000=dd.df$B4x1000, SAVIx1000=dd.df$SAVIx1000, NDBIx1000=dd.df$NDBIx1000, 
                     B8Ax1000=dd.df$B8Ax1000, 
                     NDWIx1000=dd.df$NDWIx1000, NDVIx1000=dd.df$NDVIx1000, slopex100=dd.df$slopex100, 
                     DEM=dd.df$DEM, T=dd.df$T, TM=dd.df$TM, Tm=dd.df$Tm, SLP=dd.df$SLP, 
                     STP=dd.df$STP, H=dd.df$H, PP=dd.df$PP, VV=dd.df$VV, V=dd.df$V, VM=dd.df$VM, 
                     FG=dd.df$FG, RA=dd.df$RA, SN=dd.df$SN, GR=dd.df$GR, 
                     breeding_site_description=dd.df$breeding_site_description, 
                     breeding_site_larvae=dd.df$breeding_site_larvae, status=dd.df$status, 
                     breading_site_positive=dd.df$breading_site_positive)

names(full.df)


# Se eliminan  las inspecciones renuentes, de negocios o casas cerradas 


full.df2=full.df[!is.na(full.df$breading_site_positive),]

dim(full.df)
dim(full.df2)

names(full.df2)



full.df2[is.na(full.df2)] <- "NA"

full.df3=as.data.frame(full.df2)

class(full.df3)

full.df34 = as_tibble(full.df3)

full.df34

full.df3  %>%  vis_dat()

write.csv(full.df3, file="fullDF.csv")

df.v3   =  read.table("fullDFv3.csv",header=T, sep=";",dec=".") 

df.v3   %>%  vis_dat()
