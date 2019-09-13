#############################################################
## P.E. Pérez Estigarribia <pestigarribia@est.pol.una.py> #
#############################################################
# Este archivo .R tiene como objetivo detectar y resolver 
# problemas relacionados a datos perdidos en los conjuntos 
# de datos 
########################################################

# https://cran.r-project.org/web/packages/naniar/vignettes/getting-started-w-naniar.html
# https://www.r-bloggers.com/ggplot-your-missing-data-2/
# https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html
# https://stepupanalytics.com/missing-value-imputation-techniques-in-r/
# https://www.statmethods.net/input/missingdata.html




# Se asigna un directorio de trabajo
setwd("C:/R/TopaDengue/Missing_data_R")

# A continuación en el caso de ser necesario se instalan y activan paquetes

##$ Paquetes para manipular datos $##
if (!require('tidyverse')) install.packages("tidyverse")
library(tidyverse)
if (!require('dplyr')) install.packages('dplyr')
library(dplyr)
if (!require('finalfit')) install.packages('finalfit')
library(finalfit)
if (!require('MissMech')) install.packages('MissMech')
library(MissMech)
if (!require('visdat')) install.packages('visdat')
library(visdat)
if (!require('ggstatsplot')) utils::install.packages(pkgs = "ggstatsplot")
library("ggstatsplot", lib.loc="C:/Program Files/R/R-3.5.2/library")
if (!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)
if (!require('naniar')) install.packages('naniar')
library(naniar)
if (!require('simputation')) install.packages('simputation')
library(simputation)
if (!require('plotly')) install.packages('plotly')
library(plotly)
if (!require('RVAideMemoire')) install.packages("RVAideMemoire")
library(RVAideMemoire)

if (!require('DescTools')) install.packages("DescTools")
library(DescTools)



##$ Se traen los datos desde un directorio al Global Enviroment del IDE $##

# datos en formato csv con encabezado y separadores ; para las columnas 


clima       =  read.table("Climate_Asuncion_Aeropuerto.csv",header=T, sep=";",dec=".") 


# Verificamos si los datos estan codificados correctamente con la funcion ff_glimpse

# corregir var codificadas con enteros que son factores 

names(clima)

Clima =clima %>%  rename(id_date = ï..id_date)

names(Clima)

Clima$date=as.Date(Clima$date)

Clima$RA=as.factor(Clima$RA)

Clima$SN=as.factor(Clima$SN)

Clima$GR=as.factor(Clima$GR)

Clima$TS=as.factor(Clima$TS)

Clima$TR=as.factor(Clima$TR)

Clima$D=as.factor(Clima$D)

Clima$M=as.factor(Clima$M)

Clima$Y=as.factor(Clima$Y)

summary(Clima)


#  crear df tibble 

dfClima = tibble(Clima)

class(dfClima)

# Generaramos una lista de sumario de tipos de variables y missing data 

Climavarlist=Clima %>% ff_glimpse() 


sink("Clima_summary_varlist_missing_data.txt") #redirige la salida al fichero salida.txt
Climavarlist #guarda la salida en "salida.txt"
sink()

# Identificamos los valores faltantes de cada variable 

#senepa %>% missing_plot()

Clima %>%  vis_dat()

Clima %>%  vis_miss()


#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('Clima visualisation of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
Clima %>%  vis_dat()
dev.off()

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('Clima visualisation missing of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
Clima %>%  vis_miss()
dev.off()


names(Clima)

Clima[,10:21] %>%  missing_pairs()

Clima[,22:26] %>%  missing_pairs()

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('Clima factor visualisation missing data matrix.png', width=40, height=30, units = 'cm', res = 500)
Clima[,22:26] %>%  missing_pairs()
dev.off()

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('Clima cont visualisation missing data matrix.png', width=40, height=30, units = 'cm', res = 500)
Clima[,10:21] %>%  missing_pairs()
dev.off()
