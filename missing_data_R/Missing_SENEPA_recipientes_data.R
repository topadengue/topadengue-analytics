#############################################################
## P.E. Pérez Estigarribia <pestigarribia@est.pol.una.py> #
#############################################################
# Este archivo .R tiene como objetivo detectar y resolver 
# problemas relacionados a datos perdidos en los conjuntos 
# de datos 
########################################################

# https://cran.r-project.org/web/packages/naniar/vignettes/getting-started-w-naniar.html
# https://www.r-bloggers.com/ggplot-your-missing-data-2/·
# https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html 
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


##$ Se traen los datos desde un directorio al Global Enviroment del IDE $##

# datos en formato csv con encabezado y separadores ; para las columnas 


senepa.1      =  read.table("relevamientos_senepa_recipiente.csv",header=T, sep=";",dec=".") 

names(senepa.1)

senepa=senepa.1 %>%  
  rename(larvicida.grs=larvicida_.grs..,                                   
         barrio_topadengue.s.manzana=barrio_topadengue_.s._manzana.,
         zona_topadengue.funcion_a_barrio=zona_topadengue_.funcion_a_barrio.,       
         id_predio.manz.nrocasa.apell=id_predio_.manz.nrocasa.apell.,  
         larvas.potenciales=larvas_.potenciales.,
         larvas.laboratorio=larvas_.laboratorio.,
         id_recipiente.manz.nrocasa.apell.recipiente=id_recipiente_.manz.nrocasa.apell.recipiente.)

names(senepa)

#######################################################################################
# Encuestas Entomologicas Profesionales Simplificado.xls/recipientes
#####################################################################################

# Verificamos si los datos estan codificados correctamente con la funcion ff_glimpse

# corregir var codificadas con enteros que son factores 

senepa$manzana = as.factor(senepa$manzana)

senepa$casa = as.factor(senepa$casa)

senepa$fecha = as.Date(senepa$fecha)

senepa$d = as.factor(senepa$d)

senepa$m = as.factor(senepa$m)

senepa$y = as.factor(senepa$y)

senepa$iso.semana.y = as.factor(senepa$iso.semana.y)

senepa$contenedores_positivos = as.factor(senepa$contenedores_positivos)


#  crear df tibble 

dfSenepa = tibble(senepa)

class(dfSenepa)

# Generaramos una lista de sumario de tipos de variables y missing data 

Svarlist=senepa %>% ff_glimpse() 


sink("SENEPA_summary_varlist_missing_data.txt") #redirige la salida al fichero salida.txt
Svarlist #guarda la salida en "salida.txt"
sink()

# Identificamos los valores faltantes de cada variable 

#senepa %>% missing_plot()

senepa %>%  vis_dat()

senepa %>%  vis_miss()



#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('SENEPA recipientes visualisation of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
senepa %>%  vis_dat()
dev.off()

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('SENEPA recipientes visualisation missing of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
senepa %>%  vis_miss()
dev.off()

names(senepa)


# Explanatory or confounding variables
explanatory.cp = c("familia")

# Explanatory variable of interest
dependent.cp = "contenedores_positivos" 

gg_miss_var(senepa, facet = contenedores_positivos)


senepa %>% 
  missing_pairs(dependent.cp, explanatory.cp, position = "fill")

senepa  %>% 
  summary_factorlist(dependent.cp, explanatory.cp, 
                     na_include=TRUE, p=TRUE)

senepa %>% 
  missing_compare(dependent.cp, explanatory.cp)


names(senepa)

explanatory.ccp = c("relevamiento", "hoja_senepa_ac")

dependent.ccp = "cant_contenedores_positivos"

senepa %>% 
  missing_pairs(dependent.ccp, explanatory.ccp, position = "fill")

senepa  %>% 
  summary_factorlist(dependent.ccp, explanatory.ccp, 
                     na_include=TRUE, p=TRUE)

senepa %>% 
  missing_compare(dependent.ccp, explanatory.ccp)


explanatory.cc = c("relevamiento", "hoja_senepa_ac")

dependent.cc = "cant_contenedores"

senepa %>% 
  missing_pairs(dependent.cc, explanatory.cc, position = "fill")

senepa  %>% 
  summary_factorlist(dependent.cc, explanatory.cc, 
                     na_include=TRUE, p=TRUE)

senepa %>% 
  missing_compare(dependent.cc, explanatory.cc)



names(senepa)

explanatory.tc = c("relevamiento", "hoja_senepa_ac")

dependent.tc = "tipo_criadero_SENEPA"

senepa %>% 
  missing_pairs(dependent.tc, explanatory.tc, position = "fill")

#senepa  %>% 
#  summary_factorlist(dependent.tc, explanatory.tc, 
#                     na_include=TRUE, correct = TRUE, p=TRUE)

t=senepa %>% 
  missing_compare(dependent.tc, explanatory.tc)



#write.table(t, "mydata.csv", sep=";")

ggplot(senepa, 
       aes(x = cant_contenedores, 
           y = cant_contenedores_positivos)) + 
  geom_miss_point() + 
  facet_wrap(~relevamiento) 

gg_miss_var(data.frame(relevamiento=senepa$relevamiento,
                       manzana=senepa$manzana,
                       hoja_senepa_ac=senepa$hoja_senepa_ac,
                       tipo_criadero_SENEPA=senepa$tipo_criadero_SENEPA,
                       tipo_criadero_denguechat=senepa$tipo_criadero_denguechat,
                       manzana=senepa$manzana,
                       cant_contenedores=senepa$cant_contenedores,
                       cant_contenedores_positivos=senepa$cant_contenedores_positivos,
                       contenedores_positivos=senepa$contenedores_positivos),
            facet = relevamiento)

gg_miss_var(data.frame(relevamiento=senepa$relevamiento,
                       manzana=senepa$manzana,
                       hoja_senepa_ac=senepa$hoja_senepa_ac,
                       tipo_criadero_SENEPA=senepa$tipo_criadero_SENEPA,
                       tipo_criadero_denguechat=senepa$tipo_criadero_denguechat,
                       manzana=senepa$manzana,
                       cant_contenedores=senepa$cant_contenedores,
                       cant_contenedores_positivos=senepa$cant_contenedores_positivos,
                       contenedores_positivos=senepa$contenedores_positivos),
            facet = hoja_senepa_ac)

gg_miss_var(data.frame(manzana=senepa$manzana,
                       cant_contenedores=senepa$cant_contenedores,
                       cant_contenedores_positivos=senepa$cant_contenedores_positivos,
                       contenedores_positivos=senepa$contenedores_positivos),
            facet = manzana)

# Tidy Missing Data: The Shadow Matrix

#The shadow functions provide a way to keep track of missing values. The as_shadow function creates a dataframe with the same set of columns, but with the column names added a suffix _NA

as_shadow(senepa)

# bind_shadow attaches a shadow to the current dataframe, a format we call "nabular", a portmanteau of NA a tabular. You can also use nabular to do the same thing:

se_shadow <- bind_shadow(senepa)
se_nab <- nabular(senepa)

glimpse(se_shadow)

glimpse(se_nab)

all.equal(se_shadow, se_nab)

names(se_nab)

#Nabular data provides a useful pattern to explore missing values, grouping by the missing/complete of one variable and looking at the mean and other summary values. Below we show the mean, sd, variance, and min and max values of Solar.R for when Ozone is present, and when it is missing.

senepa %>%
  bind_shadow() %>%
  group_by(cant_contenedores_NA) %>%
  summarise_at(.vars = "cant_contenedores_positivos",
               .funs = c("mean", "sd", "var", "min", "max"),
               na.rm = TRUE)

#Below, we can plot the distribution of cant_contenedores_positivos, plotting for values of temperature when cant_contenedores is missing, and when it is not missing, by wrap relevamiento.
 
ggplot(se_shadow,
       aes(x = cant_contenedores_positivos,
           colour = cant_contenedores_NA)) + 
  geom_density() + 
  facet_wrap(~relevamiento) 

# Visualising imputed values With the easy-to-use simputation package, we impute values for cant_contenedores_positivos, then visualise the data:

se_shadow %>%
  impute_lm(cant_contenedores_positivos ~ cant_contenedores + relevamiento + barrio_senepa) %>%
  ggplot(aes(x = iso.semana.y,
             y = cant_contenedores_positivos, colour = cant_contenedores_positivos_NA)) + geom_point() +
  stat_smooth(method = "gam", formula =y ~ s(x, bs = "cs")) #+ 
 # geom_point() + facet_wrap(~zona_topadengue) 

names(se_shadow)

se_shadow %>%
  ggplot(aes(x = fecha,
             y = cant_contenedores_positivos, colour = relevamiento)) + 
  geom_point() + facet_wrap(~zona_topadengue.funcion_a_barrio) 

