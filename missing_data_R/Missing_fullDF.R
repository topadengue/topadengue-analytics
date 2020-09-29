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

#if (!require('RVAideMemoire')) install.packages("RVAideMemoire")
#library(RVAideMemoire)

#if (!require('DescTools')) install.packages("DescTools")
#library(DescTools)

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


full.DFv3      =  read.table("fullDFv3.csv",header=T, sep=";",dec=".") 


full.DFv3$RA=as.factor(full.DFv3$RA)

full.DFv3$SN=as.factor(full.DFv3$SN)

full.DFv3$GR=as.factor(full.DFv3$GR)

full.DFv3$FG=as.factor(full.DFv3$FG)

full.DFv3$visit_date=as.factor(full.DFv3$visit_date)

full.DFv3 %>%  vis_dat()

full.DFv3 %>%  vis_miss()

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('full_DFv3 visualisation of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
full.DFv3 %>%  vis_dat()
dev.off()

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('full_DFv3 visualisation missing of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
full.DFv3 %>%  vis_miss()
dev.off()

# Porcentaje de datos perdidos 

sort(sapply(full.DFv3, function(x) {(sum(is.na(x))/dim(full.DFv3)[1])*100}), decreasing=TRUE)


# Imputación de datos 
#http://juliejosse.com/wp-content/uploads/2018/06/DataAnalysisMissingR.html
# https://www.kaggle.com/captcalculator/imputing-missing-data-with-the-mice-package-in-r
# https://web.maths.unsw.edu.au/~dwarton/missingDataLab.html
# http://www.di.fc.ul.pt/~jpn/r/missing/index.html
# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
#https://datascienceplus.com/graphical-presentation-of-missing-data-vim-package/

names(full.DFv3)

full.DFv4=subset(full.DFv3, select = -c(source_file,sub_dataset,id_observation,
                                        location_key,visit_date,
                                        breeding_site_description))

names(full.DFv4)

dfv5 = data.frame(status=full.DFv3$status,  
                  breading_site_positive=full.DFv3$breading_site_positive,
                  breeding_site_code_tpd=full.DFv3$breeding_site_code_tpd,
                  breeding_site_amount=full.DFv3$breeding_site_amount,
                  PP=full.DFv3$PP,
                  TM=full.DFv3$TM)

sapply(dfv5, class)

#dfv5 = data.frame(status=full.DFv3$status, 
#                  breading_site_positive=full.DFv3$breading_site_positive,
#                  breeding_site_code_tpd=full.DFv3$breeding_site_code_tpd)

dfv5$breading_site_positive=as.factor(dfv5$breading_site_positive)


imp.dfv5 <- mice(dfv5, m=10, maxit = 30, method='cart', seed=999, diag=FALSE, print=TRUE)

#imp.dfv5.rf <- mice(dfv5, m=10, maxit = 50, method='rf', seed=999, diag=FALSE, print=TRUE)

help(mice)

summary(imp.dfv5)

# Creating a Complete Dataset

imp.dfv6 <- mice::complete(imp.dfv5)

names(full.DFv3)

imp.dfv7=data.frame(full.DFv3,status_imp=imp.dfv6$status)

names(imp.dfv7)

# Let's start out by getting a high level view of what data is missing.

missmap(dfv5, col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8)

missmap(imp.dfv6, col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8)

table(imp.dfv7$status,imp.dfv7$status_imp)

missmap(full.DFv3, col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8)

imp.dfv8=subset(imp.dfv7, select = -c(status))

missmap(imp.dfv8, col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8)



write.csv(imp.dfv8, file="full_data_frame.csv")

