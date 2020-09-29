#Work directory

setwd("C:/R/TopaDengue/ML_R")

##########################$$$$$$$$$$$$###########################

#You need to install these libraries

if (!require('tidyverse')) install.packages("tidyverse")
library(tidyverse)

if (!require('visdat')) install.packages('visdat')
library(visdat)

if (!require('Rcpp')) install.packages("Rcpp")

if (!require('rJava')) install.packages("rJava")
if (!require('RWekajars')) install.packages("RWekajars", dependencies = TRUE)
if (!require('RWeka')) install.packages("RWeka", dependencies = TRUE)
if (!require('FSelector')) install.packages("FSelector", dependencies = TRUE)
if (!require('lazyBayesianRules')) install.packages('lazyBayesianRules')
if (!require('SDMTools')) install.packages("SDMTools")
if (!require('FSelector')) install.packages("FSelector")
if (!require('dplyr')) install.packages("dplyr")
if (!require('AICcmodavg')) install.packages("AICcmodavg")
if (!require('msu')) install.packages("msu")
if (!require('survMisc')) install.packages("survMisc")
if (!require('ggplot2')) install.packages('ggplot2')




# Be sure to activate the packages

library("dplyr")
library("RWeka")
library("RWeka")
library("RWekajars")
library("lazyBayesianRules")
library("SDMTools")
library("FSelector")
library("RWekajars")
library("RWeka")
library("FSelector")
library("AICcmodavg")
library("survMisc")
library("ggplot2")



######## DataSet  ########

##$ Se traen los datos desde un directorio al Global Enviroment del IDE $##

# datos en formato csv con encabezado y separadores ; para las columnas 

full_data      =  read.table("full_data_frame.csv",header=T, sep=",",dec=".") 

full_data.disc=read.table("full_data_frameDiscredtizeKononenkoBinary.csv",header=T, sep=",",dec=".") 

full_data.cond=read.table("full_data_frameClassConditionalPorbabilities.csv",header=T, sep=",",dec=".") 

#############################
# Inspeccionar y preparar datos 
##########################

names(full_data)

summary(full_data)

full_data %>%  vis_dat()


names(full_data.disc)

summary(full_data.disc)

full_data.disc %>%  vis_dat()

names(full_data.cond)

summary(full_data.cond)

full_data.cond[,1:40] %>%  vis_dat()

# Contar datos perdidos por variable 

sort(sapply(full_data, function(x) {(sum(is.na(x))/dim(full_data)[1])*100}), decreasing=TRUE)

tD = t(full_data)

dim(tD)[2]

plot(sort(sapply(tD, function(x) {(sum(is.na(x))/dim(tD)[1])*100}), decreasing=TRUE))

# Se excluyen las filas con demasiados datos perdidos 

full_data.1=full_data[!is.na(full_data$visit_date),]

full_data.1 %>%  vis_dat()

full_data.2=subset(full_data.1, select = -c(source_file,sub_dataset,id_observation,
                                        location_key,visit_date,
                                        breeding_site_description,GR,SN,breeding_site_larvae,status_imp))

full_data.2 %>%  vis_dat()

##

full_data.disc1=full_data.disc[!is.na(full_data.disc$visit_date),]

full_data.disc1 %>%  vis_dat()

full_data.disc1

##

full_data.cond1=full_data.cond[!is.na(full_data.cond$pr_visit_date.SI),]

names(full_data.cond1)

summary(full_data.cond1)

full_data.cond1[,1:40] %>%  vis_dat()

full_data.cond1[,41:83] %>%  vis_dat()


#-------------------------------------------------------------------------#
###########################################################################
#       ***** Machine Learning analysis *****                            ##
###########################################################################
#-------------------------------------------------------------------------#


# Recommended reading
browseURL("http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.0030116")
browseURL("https://cran.r-project.org/web/packages/RWeka/RWeka.pdf")
browseURL("https://en.wikipedia.org/wiki/Feature_selection#Optimality_criteria")
browseURL("https://en.wikipedia.org/wiki/Information_gain_in_decision_trees")
browseURL("http://weka.sourceforge.net/doc.dev/weka/attributeSelection/InfoGainAttributeEval.html")
browseURL("https://www.rdocumentation.org/packages/AICcmodavg/versions/2.1-1/topics/AICc")
browseURL("https://en.wikipedia.org/wiki/Receiver_operating_characteristic")
browseURL("http://weka.sourceforge.net/doc.dev/weka/classifiers/functions/Logistic.html")
browseURL("https://cran.r-project.org/web/packages/msu/msu.pdf")
browseURL("https://en.wikipedia.org/wiki/Mutual_information")
browseURL("https://arxiv.org/pdf/1709.08730.pdf")
browseURL("http://data.princeton.edu/R/glms.html")
browseURL("http://gim.unmc.edu/dxtests/roc3.htm")


###################################################################
### Mutual information between independent variables ##############
###################################################################
# It is equivalent to G-test
# In probability theory and information theory,
# the mutual information (MI) of two random variables 
# is a measure of the mutual dependence between the two variables.
# Domain of values: mutual information is nonnegative 
# (i.e. I(X;Y) ??? 0; and symmetric (i.e. I(X;Y) = I(Y;X))
# Mutual information therefore measures dependence in the 
# following sense: I(X;Y) = 0 if and only if X and Y are 
# independent random variables. 
##################################################################


#################################################################
# Se evalua la relevancia de cada atributo por information gain
################################################################

# https://rdrr.io/cran/FSelector/man/information.gain.html

#Evaluates the worth of an attribute 
#by measuring the information gain with respect to the class.
#Ranks attributes by their individual evaluations.

# InfoGain(Class,Attribute) = H(Class) - H(Class | Attribute).
# The minimum information gain is zero, 
#  when H(Class) = H(Class | Attribute). 
# The maximum is achieved when H(Class | Attribute) = 0. 

## H(Class) + H(Attribute) - H(Class, Attribute)

information_gain=information.gain(breading_site_positive~., full_data.2)

information_gain


# (H(Class) + H(Attribute) - H(Class, Attribute)) / H(Attribute)

gain_ratio=gain.ratio(breading_site_positive~., full_data.2)

gain_ratio

# 2 * (H(Class) + H(Attribute) - H(Class, Attribute)) / (H(Attribute) + H(Class))

symmetrical_uncertainty=symmetrical.uncertainty(breading_site_positive~., full_data.2)

symmetrical_uncertainty

Contribucion_de_atributos_en_base_a_Entropia=data.frame(information_gain,
                                    gain_ratio=gain_ratio$attr_importance, 
           symmetrical_uncertainty=symmetrical_uncertainty$attr_importance)


Contribucion_de_atributos_en_base_a_Entropia

Contribucion_Entropia = Contribucion_de_atributos_en_base_a_Entropia %>%  rename(information_gain = attr_importance)

Contribucion_Entropia

Contribucion_Entropia=as.matrix(Contribucion_Entropia)
names(dimnames(Contribucion_Entropia))=c("Var", "")

Contribucion_Entropia =as.data.frame(Contribucion_Entropia)


Contribucion_Entropia <- tibble::rownames_to_column(Contribucion_Entropia, "Var")

names(Contribucion_Entropia)

p <- ggplot(Contribucion_Entropia, 
            aes(x=gain_ratio,y=symmetrical_uncertainty, size=information_gain)) + 
  geom_point(aes(color=information_gain), alpha=0.5) + 
  ggrepel::geom_text_repel(aes(label = Var), color = "black", size = 2.5, segment.color = "grey") + theme_light()
p


#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('Entropy_based_filters_breading_site_positive.png', width=22, height=14, units = 'cm', res = 500)
p
dev.off()

#################################################################
## **** Inferring Models by Supervised classification  *****   ##
##          *** Supervised Learning ***                        ##
#################################################################

## Using Rweka (Machine learning) and glm (Logistic Regression)


# I will search the best model
# by additions of a single attribute (variable)
# The sequence of addition will be of the attribute with 
# greater attr_importance to that of minor according to the results of IG
# The best model will be the one 
# that is simpler and with good explanatory or predictive power
# We search the best model given the data

#(breading_site_positive~., full_data.2)

consortium+period+loc_week_y+Zona_i+                  
  Zona+Zona_i_period+Zona_period+Zona_week+               
  location_city_block+city_block_censo+location_id+barrio_tpd+               
  barrio_senepa+barrio_censo+Y+X+                        
  visit_day+visit_month+visit_year+visity_week_of_year+     
  breeding_site_code_tpd+breeding_site_code_senepa+breeding_site_amount+
  B4x1000+SAVIx1000+NDBIx1000+B8Ax1000+NDWIx1000+NDVIx1000+slopex100+
  DEM+T+TM+Tm+SLP+STP+H+PP+VV+V+VM+FG+RA

WOW(Logistic)

breading_site_positive <- factor(full_data.2$breading_site_positive, labels = c("SI","NO"))

m <- Logistic(breading_site_positive~.,data=full_data.cond1)

m

# Model Statistics for R/Weka Classifiers
## Use 10 fold cross-validation.
e <- evaluate_Weka_classifier(m,
                              cost = matrix(c(0,1,1,0), ncol = 2),
                              numFolds = 10, complexity = TRUE,
                              seed = 123, class = TRUE)

print(e)

#Predict for a data set
predict_log <- predict(m, newdata = full_data.cond1, type = c("class", "probability"))

names(full_data.cond1)

#Predict for a data 
predict(m, full_data.cond1[10,]) 

full_data.cond1[10,83]

#save prediction 
predict_Logistic = data.frame(full_data.cond1, predict_log)
head(predict_Logistic)
tail(predict_Logistic)
write.csv(predict_Logistic, file="predict_Logistic.csv")

e$confusionMatrix

e$detailsClass

summary(e)
e$details


sink("logistic_positivo.txt") #redirige la salida al fichero salida.txt
print(e) #guarda la salida en "salida.txt"
sink()


.jcache(m$classifier)
save(m, file="logistic_model.rda")

load("logistic_model.rda")

predict(m, full_data.cond1[1,]) 

###################################


## Create an interface to Weka's Naive Bayes classifier.
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
## Note that this has a very useful print method:
NB
## And we can use the Weka Option Wizard for finding out more:
WOW(NB)


breading_site_positive <- factor(full_data.2$breading_site_positive, labels = c("NO", "SI"))

help(factor)

mNB = NB(breading_site_positive~consortium+period+loc_week_y+Zona_i+                  
           Zona+Zona_i_period+Zona_period+Zona_week+               
           location_city_block+city_block_censo+location_id+barrio_tpd+               
           barrio_senepa+barrio_censo+Y+X+                        
           visit_day+visit_month+visit_year+visity_week_of_year, data=full_data.2)

mNB3 = NB(breading_site_positive~period+loc_week_y+Zona_i+                  
           Zona+Zona_i_period+Zona_period+Zona_week+
           city_block_censo+location_id+barrio_tpd+barrio_censo+                        
           visit_day+visit_month+visit_year+visity_week_of_year+     
           breeding_site_code_tpd+breeding_site_code_senepa+breeding_site_amount+
           DEM+T+TM+Tm+SLP+STP+H+PP+VV+V+VM+FG+RA, data=full_data.2)

#Predict for a data set
predict_NB <- predict(mNB3, newdata = full_data.2, type = c("class", "probability"))

predict_NB

names(full_data.2)

#Predict class for a data 
predict(mNB3, full_data.2[45,]) 
#True class
full_data.2[45,44]

#save prediction 
predict_NB.df = data.frame(full_data.2, predict_NB)
head(predict_NB.df)
tail(predict_NB.df)
write.csv(predict_NB.df, file="predict_NB.csv")


## Use 10 fold cross-validation.
eNB <- evaluate_Weka_classifier(mNB,
                                cost = matrix(c(0,1,1,0), ncol = 2),
                                numFolds = 10, complexity = TRUE,
                                seed = 123, class = TRUE)


eNB

sink("NB_positivo.txt") #redirige la salida al fichero salida.txt
print(eNB) #guarda la salida en "salida.txt"
sink()


##########################################################

#https://weka.sourceforge.io/doc.dev/weka/classifiers/bayes/BayesNet.html

BayesNet <- make_Weka_classifier("weka/classifiers/bayes/BayesNet")

mBayesNet = BayesNet(breading_site_positive~period+loc_week_y+Zona_i+                  
            Zona+Zona_i_period+Zona_period+Zona_week+
            city_block_censo+location_id+barrio_tpd+barrio_censo+                        
            visit_day+visit_month+visit_year+visity_week_of_year+     
            breeding_site_code_tpd+breeding_site_code_senepa+breeding_site_amount+
            DEM+T+TM+Tm+SLP+STP+H+PP+VV+V+VM+FG+RA, data=full_data.2)

eBayesNet  <- evaluate_Weka_classifier(mBayesNet ,
                                cost = matrix(c(0,1,1,0), ncol = 2),
                                numFolds = 10, complexity = TRUE,
                                seed = 123, class = TRUE)

eBayesNet 

sink("BayesNet.txt") #redirige la salida al fichero salida.txt
print(eBayesNet ) #guarda la salida en "salida.txt"
sink()

mBayesNet2 = BayesNet(breading_site_positive~period+loc_week_y+Zona_i+                  
                       Zona+Zona_i_period+Zona_period+Zona_week+
                       city_block_censo+location_id+barrio_tpd+barrio_censo+                        
                       visit_day+visit_month+visit_year+visity_week_of_year, data=full_data.2)

eBayesNet2  <- evaluate_Weka_classifier(mBayesNet2 ,
                                       cost = matrix(c(0,1,1,0), ncol = 2),
                                       numFolds = 10, complexity = TRUE,
                                       seed = 123, class = TRUE)

eBayesNet2 


###############################################



#####################################

breading_site_positive <- factor(full_data.2$breading_site_positive, labels = c("NO", "SI"))

## Identify a decision tree.
mj48 <- J48(breading_site_positive~.,data=full_data.cond1)

mj48


if(require("partykit", quietly = TRUE)) plot(mj48)

## Use 10 fold cross-validation.
ej48 <- evaluate_Weka_classifier(mj48,
                                 cost = matrix(c(0,2,1,0), ncol = 2),
                                 numFolds = 10, complexity = TRUE,
                                 seed = 123, class = TRUE)
ej48



sink("J48_positivo.txt") #redirige la salida al fichero salida.txt
print(ej48) #guarda la salida en "salida.txt"
sink()

## Logistic Model Tree.

m.LMT <- LMT(breading_site_positive~., data=full_data.cond1)

m.LMT

if(require("partykit", quietly = TRUE)) plot(m.LMT)

## Use 10 fold cross-validation.
eLMT <- evaluate_Weka_classifier(m.LMT,
                                 cost = matrix(c(0,1,1,0), ncol = 2),
                                 numFolds = 10, complexity = TRUE,
                                 seed = 123, class = TRUE)
eLMT

sink("Logistic Model Tree.txt") #redirige la salida al fichero salida.txt
print(eLMT) #guarda la salida en "salida.txt"
sink()


###############################################


#####################
## Multilayer Perceptron
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")

MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")

WOW(MLP)

# http://one-line-it.blogspot.com/2013/03/r-javalangoutofmemoryerror-java-heap.html

options(java.parameters = "-Xmx1024m")
library(rJava)


mMLP<- MLP(breading_site_positive~., data=full_data.cond1)

## Use 10 fold cross-validation.
eMLP <- evaluate_Weka_classifier(mMLP,
                                 cost = matrix(c(0,2,1,0), ncol = 2),
                                 numFolds = 10, complexity = TRUE,
                                 seed = 123, class = TRUE)


eMLP



sink("MLP_positivo.txt") #redirige la salida al fichero salida.txt
print(eMLP) #guarda la salida en "salida.txt"
sink()

#rf


RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest")


rfm = RF(breading_site_positive~., data=full_data.cond1)


## Use 10 fold cross-validation.
eRF <- evaluate_Weka_classifier(rfm,
                                 cost = matrix(c(0,1,1,0), ncol = 2),
                                 numFolds = 10, complexity = TRUE,
                                 seed = 123, class = TRUE)


eRF


sink("RandomForest.txt") #redirige la salida al fichero salida.txt
print(eRF) #guarda la salida en "salida.txt"
sink()












