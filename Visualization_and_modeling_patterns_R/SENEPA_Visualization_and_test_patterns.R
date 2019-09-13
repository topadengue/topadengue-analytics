#############################################################
## P.E. Pérez Estigarribia <pestigarribia@est.pol.una.py> #
#############################################################
# Este archivo .R tiene como objetivo visualizar y evaluar 
# patrones 
########################################################

# Asigne un directorio de trabajo 

setwd("C:/R/TopaDengue/Visualization_and_test_patterns_R")

# A continuación en el caso de ser necesario se instalan y activan paquetes

##$ Paquetes para manipular datos $##
if (!require('tidyverse')) install.packages("tidyverse")
library(tidyverse)

if (!require('dplyr')) install.packages('dplyr')
library(dplyr)

if (!require('finalfit')) install.packages('finalfit')
library(finalfit)

if (!require('ggstatsplot')) utils::install.packages(pkgs = "ggstatsplot")
library(ggstatsplot)

if (!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)

if (!require('plotly')) install.packages('plotly')
library(plotly)

if (!require('RVAideMemoire')) install.packages("RVAideMemoire")
library(RVAideMemoire)

if (!require('DescTools')) install.packages("DescTools")
library(DescTools)

if (!require('vegan')) install.packages('vegan')
library("vegan", lib.loc="C:/Program Files/R/R-3.5.2/library")

if (!require('rcompanion')) install.packages('rcompanion')
library(rcompanion)

if (!require('xlsx')) install.packages('xlsx')
library(xlsx)


if (!require('pROC')) install.packages('pROC')
library(pROC)

if (!require('visdat')) install.packages('visdat')
library(visdat)

if (!require('epiDisplay')) install.packages('epiDisplay')
library(epiDisplay)

if (!require('oddsratio')) install.packages('oddsratio')
library(oddsratio)




##$ Se traen los datos desde un directorio al Global Enviroment del IDE $##

# datos en formato csv con encabezado y separadores ; para las columnas 


senepa.1      =  read.table("relevamientos_senepa_recipiente.csv",header=T, sep=";",dec=".") 

names(senepa.1)

# Se cambian algunos nombres de columnas a nombres más operativos

senepa=senepa.1 %>%  
  rename(id_observation=ï..id_observation,
         larvicida.grs=larvicida_.grs..,                                   
         barrio_topadengue.s.manzana=barrio_topadengue_.s._manzana.,
         zona_topadengue.funcion_a_barrio=zona_topadengue_.funcion_a_barrio.,       
         id_predio.manz.nrocasa.apell=id_predio_.manz.nrocasa.apell.,  
         larvas.potenciales=larvas_.potenciales.,
         larvas.laboratorio=larvas_.laboratorio.,
         id_recipiente.manz.nrocasa.apell.recipiente=id_recipiente_.manz.nrocasa.apell.recipiente.)

names(senepa)


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

# Se arreglan los datos en una tibble 


tibble.senepa = tibble(senepa)

class(tibble.senepa)

# Generaramos una lista de sumario de tipos de variables y missing data 

senepa %>% ff_glimpse() 

names(senepa)

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('SENEPA recipientes visualisation of an entire data frame.png', width=40, height=18, units = 'cm', res = 500)
senepa %>%  vis_dat()
dev.off()

#############################################################################
## Evaluacion del efecto de la intervención sobre los niveles de infestacion 
#############################################################################

rel.contpost = table(senepa$relevamiento, senepa$contenedores_positivos)

plot(rel.contpost)

G.test(rel.contpost)


zona.contpost = table(senepa$zona_topadengue.funcion_a_barrio, senepa$contenedores_positivos)

zona.contpost

plot(zona.contpost)

G.test(zona.contpost)

# Filter by factor levels
# let's leave out one of the factor levels 
senepa.abr18 <- dplyr::filter(.data = senepa, relevamiento != c("jul-18","abr-19","may-19", "NA"))

# let's change the levels of our factors, a common routine in data analysis
# pipeline, to see if this function respects the new factor levels
senepa.abr18$relevamiento <- factor(x = senepa.abr18$relevamiento, levels = c("Zona_1","Zona_2","Control_1","Control_2" ))

senepa.abr18.contpost = table(senepa.abr18$zona_topadengue.funcion_a_barrio, senepa.abr18$contenedores_positivos)

senepa.abr18.contpost

plot(senepa.abr18.contpost)

G.test(senepa.abr18.contpost)

# Filter by factor levels
# let's leave out one of the factor levels 
senepa.jul18 <- dplyr::filter(.data = senepa, relevamiento != c("abr-18","abr-19","may-19", "NA"))

# let's change the levels of our factors, a common routine in data analysis
# pipeline, to see if this function respects the new factor levels
senepa.jul18$relevamiento <- factor(x = senepa.jul18$relevamiento, levels = c("Zona_1","Zona_2","Control_1","Control_2" ))

senepa.jul18.contpost = table(senepa.jul18$zona_topadengue.funcion_a_barrio, senepa.jul18$contenedores_positivos)

senepa.jul18.contpost

plot(senepa.jul18.contpost)

G.test(senepa.jul18.contpost)


# Filter by factor levels
# let's leave out one of the factor levels 
senepa.abr19 <- dplyr::filter(.data = senepa, relevamiento != c("abr-18","jul-18","may-19", "NA"))

# let's change the levels of our factors, a common routine in data analysis
# pipeline, to see if this function respects the new factor levels
senepa.abr19 $relevamiento <- factor(x = senepa.abr19 $relevamiento, levels = c("Zona_1","Zona_2","Control_1","Control_2" ))

senepa.abr19.contpost = table(senepa.abr19$zona_topadengue.funcion_a_barrio, senepa.abr19$contenedores_positivos)

senepa.abr19.contpost

plot(senepa.abr19.contpost)

G.test(senepa.abr19.contpost)

############  comparacion Linea de base con primera evaluacion luego de la intervencion #########

# Filter by factor levels
# let's leave out one of the factor levels 
senepa.control.18 <- dplyr::filter(.data = senepa, relevamiento != c("abr-19","may-19", "NA"),
                                   zona_topadengue.funcion_a_barrio != c("Zona_1","Zona_2", "NA"))

# let's change the levels of our factors, a common routine in data analysis
# pipeline, to see if this function respects the new factor levels
senepa.control.18$relevamiento <- factor(x = senepa.control.18$relevamiento, levels = c("abr-18", "jul-18"))

t.senepa.control.18 = table(senepa.control.18$relevamiento, senepa.control.18$contenedores_positivos)

t.senepa.control.18

plot(t.senepa.control.18)

G.test(t.senepa.control.18)


# Filter by factor levels
# let's leave out one of the factor levels 
senepa.zona.18 <- dplyr::filter(.data = senepa, relevamiento != c("abr-19","may-19", "NA"),
                                zona_topadengue.funcion_a_barrio != c("Control_1","Control_2", "NA"))

# let's change the levels of our factors, a common routine in data analysis
# pipeline, to see if this function respects the new factor levels
senepa.zona.18$relevamiento <- factor(x = senepa.zona.18$relevamiento, levels = c("abr-18", "jul-18"))

t.senepa.zona.18= table(senepa.zona.18$relevamiento, senepa.zona.18$contenedores_positivos)

t.senepa.zona.18

plot(t.senepa.zona.18)

G.test(t.senepa.zona.18)

########## comparación primera evaluacion con segunda evaluacion ######

# Filter by factor levels
# let's leave out one of the factor levels 
senepa.control.18.2=dplyr::filter(.data = senepa, relevamiento != c("jul-18","may-19", "NA"),
                                   zona_topadengue.funcion_a_barrio != c("Zona_1","Zona_2", "NA"))

# let's change the levels of our factors, a common routine in data analysis
# pipeline, to see if this function respects the new factor levels
senepa.control.18.2$relevamiento=factor(x = senepa.control.18.2$relevamiento, levels = c("abr-18","abr-19"))

t.senepa.control.18.2 = table(senepa.control.18.2$relevamiento, senepa.control.18.2$contenedores_positivos)

t.senepa.control.18.2

plot(t.senepa.control.18.2)

G.test(t.senepa.control.18.2)


# Filter by factor levels
# let's leave out one of the factor levels 
senepa.zona.18.2 <- dplyr::filter(.data = senepa, relevamiento != c("jul-18","may-19", "NA"),
                                zona_topadengue.funcion_a_barrio != c("Control_1","Control_2", "NA"))

# let's change the levels of our factors, a common routine in data analysis
# pipeline, to see if this function respects the new factor levels
senepa.zona.18.2$relevamiento <- factor(x = senepa.zona.18.2$relevamiento, levels = c("abr-18","abr-19"))

t.senepa.zona.18.2= table(senepa.zona.18.2$relevamiento, senepa.zona.18.2$contenedores_positivos)

t.senepa.zona.18.2

plot(t.senepa.zona.18.2)

G.test(t.senepa.zona.18.2)

############################################################

# Filter by factor levels
# let's leave out one of the factor levels 
senepa.control.19 <- dplyr::filter(.data = senepa, relevamiento != c("abr-19", "jul-18","NA"),
                                   zona_topadengue.funcion_a_barrio != c("Zona_1","Zona_2", "NA"))

# let's change the levels of our factors, a common routine in data analysis
# pipeline, to see if this function respects the new factor levels
senepa.control.19$relevamiento <- factor(x = senepa.control.19$relevamiento, levels = c("abr-18","may-19"))

t.senepa.control.19 = table(senepa.control.19$relevamiento, senepa.control.19$contenedores_positivos)

t.senepa.control.19

plot(t.senepa.control.19)

G.test(t.senepa.control.19)


# Filter by factor levels
# let's leave out one of the factor levels 
senepa.zona.19 <- dplyr::filter(.data = senepa, relevamiento != c("abr-19", "jul-18", "NA"),
                                zona_topadengue.funcion_a_barrio != c("Control_1","Control_2", "NA"))

# let's change the levels of our factors, a common routine in data analysis
# pipeline, to see if this function respects the new factor levels
senepa.zona.19$relevamiento <- factor(x = senepa.zona.19$relevamiento, levels = c("abr-18","may-19"))

t.senepa.zona.19= table(senepa.zona.19$relevamiento, senepa.zona.19$contenedores_positivos)

t.senepa.zona.19

plot(t.senepa.zona.19)

G.test(t.senepa.zona.19)



#http://rcompanion.org/rcompanion/e_07.html
#https://rdrr.io/cran/rcompanion/man/compareGLM.html

#----------------------------------------------------------------------------------

datos = data.frame(relevamiento=senepa$relevamiento, 
                   zona_topadengue=senepa$zona_topadengue.funcion_a_barrio, 
                   contenedores_positivos=senepa$contenedores_positivos)
# Divide set en Train y Test
set.seed(13)
muestra         <- sample(nrow(datos),nrow(datos)*.3)
Train           <- datos[-muestra,]
Test            <- datos[muestra,]

### Create new data frame with all missing values removed (NA's)

datos.omit = na.omit(datos)

summary(datos.omit)

#----------------------------------------------------------------------------------
# Crea modelo predictivo 
Modelo_glm.full<-glm(contenedores_positivos~relevamiento+zona_topadengue+relevamiento:zona_topadengue, datos.omit, family = binomial(link="logit"))

# Crea modelo predictivo 
Modelo_glm.ind <- glm(contenedores_positivos~relevamiento+zona_topadengue,
                      datos.omit, family = binomial(link="logit"))

# Crea modelo predictivo 
Modelo_glm.rel <- glm(contenedores_positivos~relevamiento,
                      datos.omit, family = binomial(link="logit"))

# Crea modelo predictivo 
Modelo_glm.zon <- glm(contenedores_positivos~zona_topadengue,
                      datos.omit, family = binomial(link="logit"))

# Crea modelo predictivo 
Modelo_glm.interaccion <- glm(contenedores_positivos~relevamiento:zona_topadengue, 
                              datos.omit, family = binomial(link="logit"))

# Crea modelo predictivo nulo
Modelo_glm.null  <- glm(contenedores_positivos~1, datos.omit, family = binomial(link="logit"))


summary.glm(Modelo_glm.full)

sink("summary glm full model.txt") #redirige la salida al fichero salida.txt
summary.glm(Modelo_glm.full) #guarda la salida en "salida.txt"
sink()

anova(Modelo_glm.full, Modelo_glm.null, test = "Chisq")

sink("Contraste modelo full vs nulo por anova chisq.txt") #redirige la salida al fichero salida.txt
anova(Modelo_glm.full, Modelo_glm.null, test = "Chisq") #guarda la salida en "salida.txt"
sink()

AIC(Modelo_glm.null)
AIC(Modelo_glm.full)
AIC(Modelo_glm.rel)
AIC(Modelo_glm.zon)
AIC(Modelo_glm.ind)
AIC(Modelo_glm.interaccion)


c.GLM=compareGLM(Modelo_glm.null,
           Modelo_glm.full,
           Modelo_glm.rel,
           Modelo_glm.zon,
           Modelo_glm.ind,
           Modelo_glm.interaccion)

c.GLM

class(c.GLM)


sink("compare GLM model.txt") #redirige la salida al fichero salida.txt
c.GLM #guarda la salida en "salida.txt"
sink()

logistic.display(Modelo_glm.interaccion)

#https://www.analyticsvidhya.com/blog/2015/11/beginners-guide-on-logistic-regression-in-r/

#create training and validation data from given data
install.packages('caTools')
library(caTools)

set.seed(88)
split <- sample.split(datos.omit$contenedores_positivos, SplitRatio = 0.75)

#get training and test data
dresstrain <- subset(datos.omit, split == TRUE)
dresstest <- subset(datos.omit, split == FALSE)

names(dresstrain)


# Crea y evaluar el modelo predictivo 
Modelo_glm.interaccion<-glm(contenedores_positivos=="1"~relevamiento:zona_topadengue, dresstrain , family = binomial(link="logit"))


# Crea y evaluar el modelo predictivo 
Modelo_glm.full<-glm(contenedores_positivos=="1"~relevamiento+zona_topadengue+relevamiento:zona_topadengue, dresstrain , family = binomial(link="logit"))


# https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression
# https://pjs-web.de/post/oddsratio/
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2938757/


library(oddsratio)

# Full model 

# Calculate OR for specific increment step of continuous variable
or=or_glm(data = dresstrain , model = Modelo_glm.full, CI = .70)

or

names(or)

ors=or %>%  
  rename(conf.low="CI_low (15)", conf.high="CI_high (85)")

summary(ors)

# https://stackoverflow.com/questions/45746040/odds-ratio-plot-on-log-scale-in-r

# Elements like pointrange and position_dodge only work when the outcome
#   is mapped to y, need to go through with OR set as y then flip at the
#   end
odds.ratio.fullModel=ggplot(ors, aes(y = oddsratio, x = predictor, colour = oddsratio)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 1.2) + scale_color_gradient(low="blue", high="red") +
  geom_hline(yintercept = 1.0, linetype = "dotted", size = 1) +
  scale_y_log10(breaks = c(0.01,0.05,0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL) +
  labs(y = "Odds ratio") +
  scale_x_discrete(name ="Effect", 
                   limits=c("zona_topadengueZona_2","zona_topadengueZona_1",
                            "zona_topadengueControl_2","relevamientojul-18",
                            "relevamientoabr-19","relevamientomay-19",
                            "relevamientojul-18:zona_topadengueControl_2",
                            "relevamientoabr-19:zona_topadengueControl_2",
                            "relevamientomay-19:zona_topadengueControl_2",
                            "relevamientojul-18:zona_topadengueZona_2",
                            "relevamientojul-18:zona_topadengueZona_1",
                            "relevamientoabr-19:zona_topadengueZona_1",
                            "relevamientoabr-19:zona_topadengueZona_2",
                            "relevamientomay-19:zona_topadengueZona_2",
                            "relevamientomay-19:zona_topadengueZona_1")) +
  coord_flip(ylim = c(0.05, 15)) + theme_bw() 

odds.ratio.fullModel


#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('odds ratio glm full model.png', width=20, height=18, units = 'cm', res = 500)
odds.ratio.fullModel
dev.off()

# Modelo_glm.interaccion

# Calculate OR for specific increment step of continuous variable
or.int=or_glm(data = dresstrain , model =Modelo_glm.interaccion, CI = .70)

or.int= arrange(or.int, oddsratio)

or.int

names(or.int)

ors.int=or.int %>%  
  rename(conf.low="CI_low (15)", conf.high="CI_high (85)")

ors.int$predictor

# https://stackoverflow.com/questions/45746040/odds-ratio-plot-on-log-scale-in-r

# Elements like pointrange and position_dodge only work when the outcome
#   is mapped to y, need to go through with OR set as y then flip at the
#   end
odds.ratio.int=ggplot(ors.int, aes(y = oddsratio, x = predictor, colour = oddsratio)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 1.2) + scale_color_gradient(low="blue", high="red") +
  geom_hline(yintercept = 1.0, linetype = "dotted", size = 1) +
  scale_y_log10(breaks = c(0.01,0.05,0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL) +
  labs(y = "Odds ratio") +
  scale_x_discrete(name ="Effect",       
            limits=c("relevamientoabr-18:zona_topadengueControl_2",
                     "relevamientoabr-18:zona_topadengueZona_1", 
                     "relevamientoabr-18:zona_topadengueControl_1",
                     "relevamientoabr-18:zona_topadengueZona_2", 
                     "relevamientojul-18:zona_topadengueZona_2",
                     "relevamientojul-18:zona_topadengueZona_1",
                     "relevamientojul-18:zona_topadengueControl_1",
                     "relevamientojul-18:zona_topadengueControl_2",
                     "relevamientoabr-19:zona_topadengueControl_2",
                     "relevamientoabr-19:zona_topadengueZona_1",
                     "relevamientoabr-19:zona_topadengueControl_1",
                     "relevamientoabr-19:zona_topadengueZona_2",
                     "relevamientomay-19:zona_topadengueZona_1",
                     "relevamientomay-19:zona_topadengueControl_1",
                     "relevamientomay-19:zona_topadengueControl_2",
                     "relevamientomay-19:zona_topadengueZona_2" )) +
  coord_flip(ylim = c(0.05, 15)) + theme_bw() 

odds.ratio.int


#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('odds ratio glm interaccion model.png', width=20, height=18, units = 'cm', res = 500)
odds.ratio.int
dev.off()

# ROC modelo interaccion 

predict <- predict(Modelo_glm.interaccion, dresstrain,type = 'response')

predict

predict.test <- predict(Modelo_glm.interaccion, dresstest, type = 'response')

predict.test

#confusion matrix
table(dresstrain$contenedores_positivos, predict > 0.01)

#confusion matrix test 
table(dresstest$contenedores_positivos, predict.test > 0.1)


#ROCR Curve
library(ROCR)

ROCRpred <- prediction(predict, dresstrain$contenedores_positivos)

ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('Curva ROC train modelo logistico contenedores positivos en funcion a la interacción relevaminto zona topadengue.png', width=20, height=18, units = 'cm', res = 500)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
dev.off()


ROCRpred.test <- prediction(predict.test, dresstest$contenedores_positivos)

ROCRperf.test <- performance(ROCRpred.test, 'tpr','fpr')
plot(ROCRperf.test, colorize = TRUE, text.adj = c(-0.2,1.7))

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('Curva ROC test modelo logistico contenedores positivos en funcion a la interacción relevaminto zona topadengue.png', width=20, height=18, units = 'cm', res = 500)
ROCRperf.test <- performance(ROCRpred.test, 'tpr','fpr')
plot(ROCRperf.test, colorize = TRUE, text.adj = c(-0.2,1.7))
dev.off()



library(pROC)
roc_obj.test <- roc(dresstest$contenedores_positivos, predict.test)
auc(roc_obj.test)

roc_obj <- roc(dresstrain$contenedores_positivos, predict)
auc(roc_obj)




# ROC modelo full

predict.full <- predict(Modelo_glm.full, dresstrain,type = 'response')

predict.full

predict.test.full <- predict(Modelo_glm.full, dresstest, type = 'response')

predict.test.full

#confusion matrix
table(dresstrain$contenedores_positivos, predict.full > 0.01)

plot(table(dresstrain$contenedores_positivos, predict.full > 0.1))

#confusion matrix test 
table(dresstest$contenedores_positivos, predict.test.full > 0.1)


#ROCR Curve
library(ROCR)

ROCRpred.full <- prediction(predict.full, dresstrain$contenedores_positivos)

ROCRperf.full <- performance(ROCRpred.full, 'tpr','fpr')
plot(ROCRperf.full, colorize = TRUE, text.adj = c(-0.2,1.7))

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('Curva ROC train modelo logistico contenedores positivos en funcion a relevaminto zona topadengue y la interaccion.png', width=20, height=18, units = 'cm', res = 500)
plot(ROCRperf.full, colorize = TRUE, text.adj = c(-0.2,1.7))
dev.off()


ROCRpred.test.full <- prediction(predict.test.full, dresstest$contenedores_positivos)

ROCRperf.test.full <- performance(ROCRpred.test.full, 'tpr','fpr')
plot(ROCRperf.test.full, colorize = TRUE, text.adj = c(-0.2,1.7))

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('Curva ROC test modelo logistico contenedores positivos en funcion a  relevaminto zona topadengue y la interaccion.png', width=20, height=18, units = 'cm', res = 500)
plot(ROCRperf.test.full, colorize = TRUE, text.adj = c(-0.2,1.7))
dev.off()


library(pROC)
roc_obj.test.full <- roc(dresstest$contenedores_positivos, predict.test.full)
auc(roc_obj.test.full)

roc_obj.full <- roc(dresstrain$contenedores_positivos, predict.full)
auc(roc_obj.full)


# Crea y evaluar el modelo nulo 
Modelo_glm.null<-glm(contenedores_positivos~1, dresstrain , family = binomial(link="logit"))


predict.null <- predict(Modelo_glm.null, dresstrain,type = 'response')

logistic.display(Modelo_glm.null)


library(pROC)


roc_obj.null <- roc(dresstrain$contenedores_positivos, predict.null)
auc(roc_obj.null)



#https://www.machinelearningplus.com/machine-learning/logistic-regression-tutorial-examples-r/


#So if pred is greater than 0.1, it is present else it is 0.

y_pred_num <- ifelse(predict > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- dresstrain$contenedores_positivos


summary(y_pred)

#Let's compute the accuracy, which is nothing but the proportion of y_pred that matches with y_act.

mean(y_pred == y_act)  


# Anternative prep Training and Test data.
library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.
set.seed(100)
trainDataIndex <- createDataPartition(datos.omit$contenedores_positivos, p=0.7, list = F)
trainData <- datos.omit[trainDataIndex, ]
testData <- datos.omit[-trainDataIndex, ]

testData

# Class distribution of train data
table(trainData$contenedores_positivos)


# Down Sample
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "contenedores_positivos"],
                         y = trainData$contenedores_positivos)

table(down_train$Class)


# Up Sample (optional)
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "contenedores_positivos"],
                     y = trainData$contenedores_positivos)

table(up_train$class)

# Build Logistic Model
logitmod <- glm(Class~relevamiento:zona_topadengue, family = "binomial", data=down_train)

summary(logitmod)

pred <- predict(logitmod, newdata = testData, type = "response")
pred

# Recode factors
y_pred_num <- ifelse(pred > 0.9, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$contenedores_positivos

# Accuracy
mean(y_pred == y_act)  # 94%



###############################################################
# Visualizaciones exploratorias 
############################################################

# Variación temporal de infestacion larvaria 

# plot
ggstatsplot::ggbetweenstats(
  data = senepa,
  x = m,
  y =cant_contenedores_positivos,
  notch = TRUE, # show notched box plot
  mean.plotting = TRUE, # whether mean for each group is to be displayed
  mean.ci = TRUE, # whether to display confidence interval for means
  mean.label.size = 2.5, # size of the label for mean
  type = "parametric", # which type of test is to be run
  k = 3, # number of decimal places for statistical results
  #outlier.tagging = TRUE, # whether outliers need to be tagged
  #outlier.label = Sepal.Width, # variable to be used for the outlier tag
  #outlier.label.color = "darkgreen", # changing the color for the text label
  xlab = "Mes", # label for the x-axis variable
  ylab = "Cantidad de criaderos positivos", # label for the y-axis variable
  #title = "Dataset: Iris flower data set", # title text for the plot
  ggtheme = hrbrthemes::theme_ipsum_tw(), # choosing a different theme
  ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Darjeeling1", # choosing a different color palette
  messages = FALSE
) 

names(senepa)

# Function to produce summary statistics (mean and +/- sd)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

names(senepa)

senepa %>%
  ggplot(aes(x = zona_topadengue.funcion_a_barrio,
             y = senepa$cant_contenedores_positivos/senepa$cant_contenedores*100, colour = tipo_criadero_SENEPA)) + 
             geom_jitter(position=position_jitter(0.2)) + 
             stat_summary(fun.data=data_summary, color="blue") +
             facet_wrap(~relevamiento)

# Filter by factor levels
# let's leave out one of the factor levels 
senepa.NA = dplyr::filter(.data = senepa, zona_topadengue.funcion_a_barrio != "NA",
                          tipo_criadero_SENEPA != "NA")

# let's change the levels of our factors, a common routine in data analysis
# pipeline, to see if this function respects the new factor levels
senepa.NA$zona_topadengue.funcion_a_barrio = factor(x = senepa.NA$zona_topadengue.funcion_a_barrio,
                                      levels = c("Control_1", "Control_2","Zona_1", "Zona_2"))


senepa.NA %>%
  ggplot(aes(x = zona_topadengue.funcion_a_barrio,
             y = senepa.NA$cant_contenedores_positivos/senepa.NA$cant_contenedores*100, 
             colour = relevamiento)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data=data_summary, color="blue") +
  facet_wrap(~tipo_criadero_SENEPA) 

senepa.NA %>%
  ggplot(aes(x = zona_topadengue.funcion_a_barrio,
             y = senepa.NA$cant_contenedores_positivos/senepa.NA$cant_contenedores*100, 
             colour = Eval)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data=data_summary, color="blue") +
  facet_wrap(~tipo_criadero_SENEPA) 



senepa %>%
  ggplot(aes(x = cant_contenedores,
             y = cant_contenedores_positivos, colour = tipo_criadero_SENEPA)) + 
              geom_point()+
              facet_wrap(~zona_topadengue.funcion_a_barrio) 




