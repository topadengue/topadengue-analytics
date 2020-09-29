
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


if (!require('pROC')) install.packages('pROC')
library(pROC)

if (!require('visdat')) install.packages('visdat')
library(visdat)

if (!require('epiDisplay')) install.packages('epiDisplay')
library(epiDisplay)

if (!require('oddsratio')) install.packages('oddsratio')
library(oddsratio)

if (!require('visdat')) install.packages('visdat')
library(visdat)


full_data = read.table("full_data_frame.csv",header=T, sep=",",dec=".")  

# Contar datos perdidos por variable 

sort(sapply(full_data, function(x) {(sum(is.na(x))/dim(full_data)[1])*100}), decreasing=TRUE)

tD = t(full_data)

dim(tD)[2]

plot(sort(sapply(tD, function(x) {(sum(is.na(x))/dim(tD)[1])*100}),
          decreasing=TRUE))

# Se excluyen las filas con demasiados datos perdidos 

full_data.1=full_data[!is.na(full_data$visit_date),]

summary(full_data.1)


full_data.2=subset(full_data.1, select = -c(source_file,sub_dataset,id_observation,
                                            location_key,visit_date,
                                            breeding_site_description,GR,SN,
                                            breeding_site_larvae,status_imp))

full_data.2 %>%  vis_dat()

# Filter by factor levels
# let's leave out one of the factor levels 
senepa=dplyr::filter(.data = full_data.2, consortium =="SENEPA")

senepa %>% vis_dat()

#http://rcompanion.org/rcompanion/e_07.html
#https://rdrr.io/cran/rcompanion/man/compareGLM.html

#---------------------------------------------------------------------------------

dim(senepa)

datos=data.frame(period=senepa$period,
                 Zona_i=senepa$Zona_i,
                 Zona=senepa$Zona,
                 breading_site_positive=senepa$breading_site_positive)

dim(datos)

# Divide set en Train y Test
set.seed(13)
muestra         <- sample(nrow(datos),nrow(datos)*.25)
Train           <- datos[-muestra,]
Test            <- datos[muestra,]

dim(Train)
dim(Test)

summary(Train)

### Create new data frame with all missing values removed (NA's)

datos.omit = na.omit(Train)

summary(datos.omit)

#----------------------------------------------------------------------------------

# Crea modelo predictivo 
m.0 = glm(breading_site_positive~1, 
          datos.omit, family = binomial(link="logit"), maxit = 100)
m.1 = glm(breading_site_positive~Zona_i+period+Zona_i:period, 
          datos.omit, family = binomial(link="logit"), maxit = 100)
m.2 = glm(breading_site_positive~Zona_i+period, 
          datos.omit, family = binomial(link="logit"), maxit = 100)
m.3 = glm(breading_site_positive~Zona_i:period, 
          datos.omit, family = binomial(link="logit"), maxit = 100)
m.4 = glm(breading_site_positive~Zona_i, 
          datos.omit, family = binomial(link="logit"), maxit = 100)
m.5 = glm(breading_site_positive~period, 
          datos.omit, family = binomial(link="logit"), maxit = 100)
m.6 = glm(breading_site_positive~Zona+period+Zona:period, 
          datos.omit, family = binomial(link="logit"), maxit = 100)
m.7 = glm(breading_site_positive~Zona+period, 
          datos.omit, family = binomial(link="logit"), maxit = 100)
m.8 = glm(breading_site_positive~Zona:period, 
          datos.omit, family = binomial(link="logit"), maxit = 100)
m.9 = glm(breading_site_positive~Zona, 
          datos.omit, family = binomial(link="logit"), maxit = 100)


comp.fit.stat.model=compareGLM(m.0,m.1,m.2,
                               m.3,m.4,m.5,
                               m.6,m.7,m.8,
                               m.9)

comp.fit.stat.model

sink("Compare Fit Statistics For Glm Models.txt") #redirige la salida al fichero salida.txt
print(comp.fit.stat.model) #guarda la salida en "salida.txt"
sink()




library(pROC)

predict.train.0 = predict(m.0, Train,type = 'response')
roc_obj.train.0 = roc(Train$breading_site_positive, predict.train.0)
auc.train.0=auc(roc_obj.train.0)
auc.train.0

predict.test.0 = predict(m.0, Test,type = 'response')
roc_obj.test.0 = roc(Test$breading_site_positive, predict.test.0)
auc.test.0=auc(roc_obj.test.0)
auc.test.0

predict.train.1 = predict(m.1, Train,type = 'response')
roc_obj.train.1 = roc(Train$breading_site_positive, predict.train.1)
auc.train.1=auc(roc_obj.train.1)
auc.train.1

predict.test.1 = predict(m.1, Test,type = 'response')
roc_obj.test.1 = roc(Test$breading_site_positive, predict.test.1)
auc.test.1=auc(roc_obj.test.1)
auc.test.1

predict.train.2 = predict(m.2, Train,type = 'response')
roc_obj.train.2 = roc(Train$breading_site_positive, predict.train.2)
auc.train.2=auc(roc_obj.train.2)
auc.train.2

predict.test.2 = predict(m.2, Test,type = 'response')
roc_obj.test.2 = roc(Test$breading_site_positive, predict.test.2)
auc.test.2=auc(roc_obj.test.2)
auc.test.2

predict.train.3 = predict(m.3, Train,type = 'response')
roc_obj.train.3 = roc(Train$breading_site_positive, predict.train.3)
auc.train.3=auc(roc_obj.train.3)
auc.train.3

predict.test.3 = predict(m.3, Test,type = 'response')
roc_obj.test.3 = roc(Test$breading_site_positive, predict.test.3)
auc.test.3=auc(roc_obj.test.3)
auc.test.3

predict.train.4 = predict(m.4, Train,type = 'response')
roc_obj.train.4 = roc(Train$breading_site_positive, predict.train.4)
auc.train.4=auc(roc_obj.train.4)
auc.train.4

predict.test.4 = predict(m.4, Test,type = 'response')
roc_obj.test.4 = roc(Test$breading_site_positive, predict.test.4)
auc.test.4=auc(roc_obj.test.4)
auc.test.4

predict.train.5 = predict(m.5, Train,type = 'response')
roc_obj.train.5 = roc(Train$breading_site_positive, predict.train.5)
auc.train.5=auc(roc_obj.train.5)
auc.train.5

predict.test.5 = predict(m.5, Test,type = 'response')
roc_obj.test.5 = roc(Test$breading_site_positive, predict.test.5)
auc.test.5=auc(roc_obj.test.5)
auc.test.5

predict.train.6 = predict(m.6, Train,type = 'response')
roc_obj.train.6 = roc(Train$breading_site_positive, predict.train.6)
auc.train.6=auc(roc_obj.train.6)
auc.train.6

predict.test.6 = predict(m.6, Test,type = 'response')
roc_obj.test.6 = roc(Test$breading_site_positive, predict.test.6)
auc.test.6=auc(roc_obj.test.6)
auc.test.6

predict.train.7 = predict(m.7, Train,type = 'response')
roc_obj.train.7 = roc(Train$breading_site_positive, predict.train.7)
auc.train.7=auc(roc_obj.train.7)
auc.train.7

predict.test.7 = predict(m.7, Test,type = 'response')
roc_obj.test.7 = roc(Test$breading_site_positive, predict.test.7)
auc.test.7=auc(roc_obj.test.7)
auc.test.7

predict.train.8 = predict(m.8, Train,type = 'response')
roc_obj.train.8 = roc(Train$breading_site_positive, predict.train.8)
auc.train.8=auc(roc_obj.train.8)
auc.train.8

predict.test.8 = predict(m.8, Test,type = 'response')
roc_obj.test.8 = roc(Test$breading_site_positive, predict.test.8)
auc.test.8=auc(roc_obj.test.8)
auc.test.8

predict.train.9 = predict(m.9, Train,type = 'response')
roc_obj.train.9 = roc(Train$breading_site_positive, predict.train.9)
auc.train.9=auc(roc_obj.train.9)
auc.train.9

predict.test.9 = predict(m.9, Test,type = 'response')
roc_obj.test.9 = roc(Test$breading_site_positive, predict.test.9)
auc.test.9=auc(roc_obj.test.9)
auc.test.9


class(m.0$formula)

deparse(m.0$formula)

#The number of matrix components increases in columns
MAUC <- matrix(c(deparse(m.0$formula),deparse(m.1$formula),deparse(m.2$formula),deparse(m.3$formula),deparse(m.4$formula),deparse(m.5$formula),deparse(m.6$formula),deparse(m.7$formula),deparse(m.8$formula),deparse(m.9$formula),auc.train.0, auc.train.1, auc.train.2, auc.train.3,auc.train.4, auc.train.5, auc.train.6, auc.train.7, auc.train.8, auc.train.9,auc.test.0,auc.test.1,auc.test.2,auc.test.3,auc.test.4,auc.test.5,auc.test.6,auc.test.7,auc.test.8,auc.test.9
),nrow=10, ncol=3)

MAUC

#Combine information gain output 
#in a data.frame, rename columns 
d.auc <- data.frame(MAUC)
names(d.auc) <- c("Formula","auc.train","auc.test")

#Save an MI Table (csv) in the working directory
write.csv(d.auc, file = "AUC_model.csv")

names(d.auc)

summary(d.auc)

############################

names(m.1)


#confusion matrix
table(Train$breading_site_positive, predict.train > 0.04)

#confusion matrix test 
table(Test$breading_site_positive, predict.test > 0.04)

logistic.display(m.1)

sink("full m1 Glm Models.txt") #redirige la salida al fichero salida.txt
logistic.display(m.1) #guarda la salida en "salida.txt"
sink()



#####################################

# https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression
# https://pjs-web.de/post/oddsratio/
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2938757/

remotes::install_github("pat-s/oddsratio", build_vignettes = TRUE)


library(oddsratio)


# Calculate OR for specific increment step of continuous variable
or=or_glm(data = datos.omit , model = m.1, CI = .70)


or

#Save an MI Table (csv) in the working directory
write.csv(or, file = "Odds_Ratio_model.csv")

class(or)

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
  scale_y_log10(breaks = c(0.01,0.05,0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 17),
                minor_breaks = NULL) +
  labs(y = "Odds ratio")  +
  coord_flip(ylim = c(0.04, 18)) + theme_bw() +
  scale_x_discrete(name ="Effect", 
                   limits=c("periodE2","periodE3","periodE4",
                            "Zona_iZona_1","Zona_iZona_2","Zona_iControl_2",
                            "Zona_iZona_1:periodE2",
                            "Zona_iZona_2:periodE2",
                            "Zona_iZona_1:periodE3",
                            "Zona_iZona_2:periodE3",
                            "Zona_iZona_1:periodE4",
                            "Zona_iZona_2:periodE4",
                            "Zona_iControl_2:periodE2",
                            "Zona_iControl_2:periodE3",
                            "Zona_iControl_2:periodE4"))

odds.ratio.fullModel


#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('odds ratio glm full model.png', width=20, height=18, units = 'cm', res = 500)
odds.ratio.fullModel
dev.off()

############################################
