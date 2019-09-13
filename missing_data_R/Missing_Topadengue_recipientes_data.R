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

inspections0 =  read.table("inspections_summary_view_nodupl.csv",header=T, sep=";",dec=".") 

negativos   =  read.table("registro_comunitario_negativos.csv",header=T, sep=";",dec=".") 

clima       =  read.table("Climate_Asuncion_Aeropuerto.csv",header=T, sep=";",dec=".") 


inspections=bind_rows(inspections0, negativos)

dim(inspections0)
dim(negativos)
dim(inspections)

names(inspections)



# Verificamos si los datos estan codificados correctamente con la funcion ff_glimpse

# corregir var codificadas con enteros que son factores 

names(inspections)

inspections$location_id = as.factor(inspections$location_id)

inspections$location_city_block = as.factor(inspections$location_city_block)

inspections$visit_year = as.factor(inspections$visit_year)

inspections$visit_month = as.factor(inspections$visit_month)

inspections$visit_day = as.factor(inspections$visit_day)

inspections$visit_date = as.Date(inspections$visit_date)

inspections$visity_week_of_year = as.factor(inspections$visity_week_of_year)

inspections$breeding_site_larvae = as.factor(inspections$breeding_site_larvae)

inspections$breading_site_pupae = as.factor(inspections$breading_site_pupae)

inspections$breading_site_positive = as.factor(inspections$breading_site_positive)

inspections$interv = as.factor(inspections$Interv)

#  crear df tibble 

dfinspections = tibble(inspections)

class(dfinspections)

# Generaramos una lista de sumario de tipos de variables y missing data 

TDvarlist=inspections %>% ff_glimpse() 


sink("inspections_summary_varlist_missing_data.txt") #redirige la salida al fichero salida.txt
TDvarlist #guarda la salida en "salida.txt"
sink()

# Identificamos los valores faltantes de cada variable 

#senepa %>% missing_plot()

inspections %>%  vis_dat()

inspections %>%  vis_miss()


#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('inspections visualisation of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
inspections %>%  vis_dat()
dev.off()

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('inspections visualisation missing of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
inspections %>%  vis_miss()
dev.off()


# Explanatory or confounding variables
explanatory.bsc = c("visit_month", "visit_year")

# Explanatory variable of interest
dependent.bsc = "breeding_site_code" 

#gg_miss_var(inspections , facet = visit_month)


inspections %>% 
  missing_pairs(dependent.bsc , explanatory.bsc, position = "fill")

inspections %>% 
  summary_factorlist(dependent.bsc , explanatory.bsc, 
                     na_include=TRUE,  p=F)

inspections %>% 
  missing_compare(dependent.bsc , explanatory.bsc)

# Visualizar algunas variables importantes con datos perdidos 

gg_miss_var(data.frame(location_id=inspections$location_id,
                       visit_day=inspections$visit_day,
                       breeding_site_code=inspections$breeding_site_code,
                       location_city_block=inspections$location_city_block,
                       breeding_site_amount=inspections$breeding_site_amount,
                       breading_site_positive=inspections$breading_site_positive,
                       breading_site_pupae=inspections$breading_site_pupae,
                       breeding_site_larvae=inspections$breeding_site_larvae),
            facet = breeding_site_code)

gg_miss_var(data.frame(location_id=inspections$location_id,
                       visit_day=inspections$visit_day,
                       visit_month=inspections$visit_month,
                       breeding_site_code=inspections$breeding_site_code,
                       location_city_block=inspections$location_city_block,
                       breeding_site_amount=inspections$breeding_site_amount,
                       breading_site_positive=inspections$breading_site_positive,
                       breading_site_pupae=inspections$breading_site_pupae,
                       breeding_site_larvae=inspections$breeding_site_larvae,
                       Interv=inspections$Interv,
                       sub_dataset=inspections$sub_dataset),
            facet = sub_dataset)

# Tidy Missing Data: The Shadow Matrix

#The shadow functions provide a way to keep track of missing values. The as_shadow function creates a dataframe with the same set of columns, but with the column names added a suffix _NA

as_shadow(inspections)

# bind_shadow attaches a shadow to the current dataframe, a format we call "nabular", a portmanteau of NA a tabular. You can also use nabular to do the same thing:

TD_shadow <- bind_shadow(inspections)
TD_nab <- nabular(inspections)

glimpse(inspections)

glimpse(TD_nab)

all.equal(TD_shadow, TD_nab)

names(TD_nab)

#Nabular data provides a useful pattern to explore missing values, grouping by the missing/complete of one variable and looking at the mean and other summary values. Below we show the mean, sd, variance, and min and max values of Solar.R for when Ozone is present, and when it is missing.

inspections %>%
  bind_shadow() %>%
  group_by(breeding_site_code_NA) %>%
  summarise_at(.vars = "breeding_site_amount",
               .funs = c("mean", "sd", "var", "min", "max"),
               na.rm = TRUE)

#Below, we can plot the distribution of cant_contenedores_positivos, plotting for values of temperature when cant_contenedores is missing, and when it is not missing, by wrap relevamiento.

ggplot(TD_shadow,
       aes(x = breeding_site_amount,
           colour = visit_date_NA)) + 
  geom_density() + 
  facet_wrap(~visit_month~visit_year) 

# Visualising imputed values With the easy-to-use simputation package, we impute values for cant_contenedores_positivos, then visualise the data:



TD_shadow %>%
  impute_lm(breeding_site_amount ~ visit_month + breading_site_positive + status) %>%
  ggplot(aes(x = visity_week_of_year,
             y = breeding_site_amount, colour = breeding_site_amount_NA)) + geom_point() +
             stat_smooth(method = "gam", formula =y ~ s(x, bs = "cs")) 

            # stat_smooth(method = "gam", formula =y ~ s(x, bs = "cs")) #+ geom_point()  #+ facet_wrap(~interv) 


TD_shadow %>%
  ggplot(aes(x = visity_week_of_year,
             y = breading_site_positive, colour = sub_dataset)) + geom_point() 

            #stat_smooth(method = "gam", formula =y ~ s(x, bs = "cs")) #+ geom_point()  #+ facet_wrap(~interv) 



TD_shadow  %>% ggplot(aes(x = visity_week_of_year,
             y = breeding_site_larvae, colour = sub_dataset)) + geom_point() #+
              #stat_smooth(method = "gam", formula =y ~ s(x, bs = "cs"))  #+ geom_point() #+ facet_wrap(~interv) 

inspections %>%
  ggplot(aes(x = visity_week_of_year,
             y = breeding_site_amount, colour = sub_dataset)) + #geom_point() +
  stat_smooth(method = "gam", formula =y ~ s(x, bs = "cs"))  #+ geom_point() #+ facet_wrap(~interv) 




TD_shadow %>%
  ggplot(aes(x = visity_week_of_year,
             y = breeding_site_amount, colour = sub_dataset)) + 
    stat_smooth(method = "gam", formula =y ~ s(x, bs = "cs")) #+ facet_wrap(~interv) 


intpositivo = table(inspections$Interv,inspections$breading_site_positive)

intpositivo

plot(intpositivo)

G.test(intpositivo)

