#Directorio de trabajo local 
setwd("C:/R/TopaDengue/R_ANOVA")

# Enlaces recomendados 
browseURL("https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html")
browseURL("https://tereom.github.io/est-computacional-2018/datos-limpios.html")
browseURL("https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf")
browseURL("https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf")

# Paquetes y librerias 

if (!require('dplyr')) install.packages('dplyr')
library("dplyr", lib.loc="~/R/win-library/3.4")

if (!require('tidyverse')) install.packages("tidyverse")
library(tidyverse)

if (!require('ggstatsplot')) utils::install.packages(pkgs = "ggstatsplot")
library("ggstatsplot", lib.loc="C:/Program Files/R/R-3.5.2/library")

if (!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)

if (!require('RVAideMemoire')) install.packages("RVAideMemoire")
library(RVAideMemoire)

data1 =  read.table("Larval_surveys_index.csv",header=T, sep=",",dec=".")



browseURL("https://rdrr.io/cran/RVAideMemoire/man/perm.anova.html")

browseURL("http://www.understandingdata.net/2017/05/11/anova-tables-in-r/")

browseURL("https://garrettgman.github.io/tidying/")

#https://rpubs.com/Mentors_Ubiqum/removing_outliers

# find  and Remov outliers - quick & dirty

outliers=boxplot(data1$CI)$out

outliers

# First you need find in which rows the outliers are

out=data1[which(data1$CI %in% outliers),]


write.csv(out, file="outliers Larval_surveys_index CI.csv")

sink("outliers Larval_surveys_index CI.txt") #redirige la salida al fichero salida.txt
print(out) #guarda la salida en "salida.txt"
sink()

# Now you can remove the rows containing the outliers, one possible option is:

data1.out <- data1[-which(data1$CI %in% outliers),]

data1.out

# If you check now with boxplot, you will notice that those pesky outliers are gone
boxplot(data1.out$CI)

data2=mutate(data1.out, period.zona=paste(data1.out$period,
                            data1.out$zona_i, sep="&"))

data2$consortium


data = filter(data2, consortium=="SENEPA")

consortium=data$consortium
period = data$period
zona_i=data$zona_i
zona=data$zona
city_block=data$city_block
period.zona=data$period.zona
CI=data$CI


p.aov2=perm.anova(CI~period.zona, nperm = 999, progress = TRUE)

p.aov2

write.csv(p.aov2, file="CI~f(period.zona)perm_anova.csv")

sink("CI~f(period.zona)perm_anova.txt") #redirige la salida al fichero salida.txt
print(p.aov2) #guarda la salida en "salida.txt"
sink()

browseURL("https://rdrr.io/cran/RVAideMemoire/man/pairwise.perm.t.test.html")

period.zona

# Pairwise comparisons
pos.hoc=pairwise.perm.t.test(CI, period.zona, p.method = "fdr", paired = FALSE,
                     alternative = c("one.sided","less", "greater"), nperm = 999,
                     progress = TRUE)

sink("CI~f(period.zona) pairwise perm t test.txt") #redirige la salida al fichero salida.txt
print(pos.hoc) #guarda la salida en "salida.txt"
sink()

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

tgc <- summarySE(data, measurevar="CI", groupvars=c("period","zona_i"))

tgc 

tgc = arrange(tgc,period, zona_i, CI)

tgc

write.csv(tgc, file="CI period.zona stat.csv")

sink("CI period.zona stat.txt") #redirige la salida al fichero salida.txt
print(tgc) #guarda la salida en "salida.txt"
sink()

tgc2 <- tgc
tgc2$zona <- factor(tgc2$zona)

# Use 95% confidence intervals instead of SEM
p=ggplot(tgc2, aes(x=period, y=CI, fill=zona_i)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=CI, ymax=CI+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


p

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('CI~f(period.zona) CI bartplot.png', width=22, height=14, units = 'cm', res = 500)
p
dev.off()

#####################

#TopaDengue

data.t = filter(data2, consortium=="TopaDengue")

boxplot(data.t$CI)$out

consortium.t=data.t$consortium
period.t = data.t$period
zona_i.t=data.t$zona_i
zona.t=data.t$zona
city_block.t=data.t$city_block
period.zona.t=data.t$period.zona
CI.t=data.t$CI


p.aov.t=perm.anova(CI.t~period.zona.t, nperm = 999, progress = TRUE)

p.aov.t

write.csv(p.aov.t, file="CI~f(period.zona)perm_anova_TD.csv")

sink("CI~f(period.zona)perm_anova_TD.txt") #redirige la salida al fichero salida.txt
print(p.aov.t) #guarda la salida en "salida.txt"
sink()



tgc.t <- summarySE(data.t, measurevar="CI", groupvars=c("period","zona_i"))

#df[row,column] <- NA
tgc.t[ 2,7] <- NA

tgc.t

write.csv(tgc.t, file="CI period.zona stat TD.csv")

sink("CI period.zona stat TD.txt") #redirige la salida al fichero salida.txt
print(tgc.t) #guarda la salida en "salida.txt"
sink()

tgc2.t <- tgc.t


# Use 95% confidence intervals instead of SEM
p.t=ggplot(tgc2.t, aes(x=period, y=CI, fill=zona_i)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=CI, ymax=CI+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


p.t

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('CI bartplot TD.png', width=22, height=14, units = 'cm', res = 500)
p.t
dev.off()


####################
#  BI
#################

# find  and Remov outliers - quick & dirty

outliers.BI=boxplot(data1$BI)$out

outliers.BI

# First you need find in which rows the outliers are

out.BI=data1[which(data1$BI %in% outliers.BI),]


write.csv(out.BI, file="outliers Larval_surveys_index BI.csv")

sink("outliers Larval_surveys_index BI.txt") #redirige la salida al fichero salida.txt
print(out.BI) #guarda la salida en "salida.txt"
sink()

# Now you can remove the rows containing the outliers, one possible option is:

data1.out.BI <- data1[-which(data1$BI %in% outliers.BI),]

data1.out.BI

# If you check now with boxplot, you will notice that those pesky outliers are gone
boxplot(data1.out.BI$BI)

data2.BI=mutate(data1.out.BI, period.zona=paste(data1.out.BI$period,
                                          data1.out.BI$zona_i, sep="&"))


data.BI = filter(data2.BI, consortium=="SENEPA")

consortium=data.BI$consortium
period= data.BI$period
zona_i=data.BI$zona_i
zona=data.BI$zona
city_block=data.BI$city_block
period.zona=data.BI$period.zona
BI=data.BI$BI


p.aov.BI=perm.anova(BI~period.zona, nperm = 999, progress = TRUE)

p.aov.BI

write.csv(p.aov.BI, file="BI~f(period.zona)perm_anova.csv")

sink("BI~f(period.zona)perm_anova.txt") #redirige la salida al fichero salida.txt
print(p.aov.BI) #guarda la salida en "salida.txt"
sink()


# Pairwise comparisons
pos.hoc.BI=pairwise.perm.t.test(BI, period.zona, p.method = "fdr", paired = FALSE,
                             alternative = c("one.sided","less", "greater"), nperm = 999,
                             progress = TRUE)

pos.hoc.BI

sink("BI~f(period.zona) pairwise perm t test.txt") #redirige la salida al fichero salida.txt
print(pos.hoc.BI) #guarda la salida en "salida.txt"
sink()

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

tgc.BI <- summarySE(data.BI, measurevar="BI", groupvars=c("period","zona_i"))

tgc.BI 

tgc.BI = arrange(tgc.BI,period, zona_i, BI)

tgc.BI

write.csv(tgc.BI, file="BI period.zona stat.csv")

sink("BI period.zona stat.txt") #redirige la salida al fichero salida.txt
print(tgc.BI) #guarda la salida en "salida.txt"
sink()

tgc2.BI <- tgc.BI
tgc2.BI$zona <- factor(tgc2.BI$zona)

# Use 95% confidence intervals instead of SEM
p.BI=ggplot(tgc2.BI, aes(x=period, y=BI, fill=zona_i)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=BI, ymax=BI+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


p.BI

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('BI~f(period.zona) CI bartplot.png', width=22, height=14, units = 'cm', res = 500)
p.BI
dev.off()

#####################

#TopaDengue

data.t.BI = filter(data2.BI, consortium=="TopaDengue")

boxplot(data.t.BI$BI)$out

consortium.t=data.t.BI$consortium
period.t = data.t.BI$period
zona_i.t=data.t.BI$zona_i
zona.t=data.t.BI$zona
city_block.t=data.t.BI$city_block
period.zona.t=data.t.BI$period.zona
BI.t=data.t.BI$BI


p.aov.t.BI=perm.anova(BI.t~period.zona.t, nperm = 999, progress = TRUE)

p.aov.t.BI

write.csv(p.aov.t.BI, file="BI~f(period.zona)perm_anova_TD.csv")

sink("BI~f(period.zona)perm_anova_TD.txt") #redirige la salida al fichero salida.txt
print(p.aov.t.BI) #guarda la salida en "salida.txt"
sink()



tgc.t.BI <- summarySE(data.t.BI, measurevar="BI", groupvars=c("period","zona_i"))

tgc.t.BI

#df[row,column] <- NA
tgc.t.BI[ 2,7] <- NA

tgc.t.BI

write.csv(tgc.t.BI, file="BI period.zona stat TD.csv")

sink("BI period.zona stat TD.txt") #redirige la salida al fichero salida.txt
print(tgc.t.BI) #guarda la salida en "salida.txt"
sink()

tgc2.t.BI <- tgc.t.BI


# Use 95% confidence intervals instead of SEM
p.t.BI=ggplot(tgc2.t.BI, aes(x=period, y=BI, fill=zona_i)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=BI, ymax=BI+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


p.t.BI

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('BI bartplot TD.png', width=22, height=14, units = 'cm', res = 500)
p.t.BI
dev.off()

##################
#pos_container

data1.pos=mutate(data1, period.zona=paste(data1$period,
                                                data1$zona_i, sep="&"))

data.pos= filter(data1.pos, consortium=="SENEPA", !period.zona %in% c("E4&Zona_2", "E4&Control_2"))

consortium=data.pos$consortium
period= data.pos$period
zona_i=data.pos$zona_i
zona=data.pos$zona
city_block=data.pos$city_block
period.zona=data.pos$period.zona
pos_container=data.pos$pos_container


p.aov.pos=perm.anova(pos_container~period.zona, nperm = 999, progress = TRUE)

p.aov.pos

write.csv(p.aov.pos, file="pos_container~f(period.zona)perm_anova.csv")

sink("pos_container~f(period.zona)perm_anova.txt") #redirige la salida al fichero salida.txt
print(p.aov.pos) #guarda la salida en "salida.txt"
sink()


# Pairwise comparisons
pos.hoc.pos=pairwise.perm.t.test(pos_container, period.zona, p.method = "fdr", paired = FALSE,
                                alternative = c("one.sided","less", "greater"), nperm = 999,
                                progress = TRUE)

pos.hoc.pos

sink("pos_container~f(period.zona) pairwise perm t test.txt") #redirige la salida al fichero salida.txt
print(pos.hoc.pos) #guarda la salida en "salida.txt"
sink()

filter(data1, consortium == "SENEPA")

tgc.pos <- summarySE(filter(data1, consortium == "SENEPA"), 
                     measurevar="pos_container", groupvars=c("period","zona_i"))

tgc.pos


write.csv(tgc.pos, file="pos_container period.zona stat SENEPA.csv")

sink("pos_container period.zona stat SENEPA.txt") #redirige la salida al fichero salida.txt
print(tgc.pos) #guarda la salida en "salida.txt"
sink()

tgc2.pos <- tgc.pos


# Use 95% confidence intervals instead of SEM
p.pos=ggplot(tgc2.pos, aes(x=period, y=pos_container, fill=zona_i)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=pos_container, ymax=pos_container+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


p.pos

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('pos_container bartplot SENEPA.png', width=22, height=14, units = 'cm', res = 500)
p.pos
dev.off()

################
# pos_conteiner 
# Topadengue 

data1.pos=mutate(data1, period.zona=paste(data1$period,
                                          data1$zona_i, sep="&"))

data1.pos$consortium

data.pos.t= filter(data1.pos, consortium=="TopaDengue")

consortium=data.pos.t$consortium
period= data.pos.t$period
zona_i=data.pos.t$zona_i
zona=data.pos.t$zona
city_block=data.pos.t$city_block
period.zona=data.pos.t$period.zona
pos_container=data.pos.t$pos_container


p.aov.pos.t=perm.anova(pos_container~period.zona, nperm = 999, progress = TRUE)

p.aov.pos.t

write.csv(p.aov.pos.t, file="pos_container~f(period.zona)perm_anova_TD.csv")

sink("pos_container~f(period.zona)perm_anova_TD.txt") #redirige la salida al fichero salida.txt
print(p.aov.pos.t) #guarda la salida en "salida.txt"
sink()


filter(data1, consortium == "TopaDengue")

tgc.pos.t <- summarySE(filter(data1, consortium == "TopaDengue"), 
                     measurevar="pos_container", groupvars=c("period","zona_i"))

tgc.pos.t


write.csv(tgc.pos.t, file="pos_container period.zona stat TD.csv")

sink("pos_container period.zona stat TD.txt") #redirige la salida al fichero salida.txt
print(tgc.pos.t) #guarda la salida en "salida.txt"
sink()

tgc2.pos.t <- tgc.pos.t


# Use 95% confidence intervals instead of SEM
p.pos.t=ggplot(tgc2.pos.t, aes(x=period, y=pos_container, fill=zona_i)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=pos_container, ymax=pos_container+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


p.pos.t

#Save the figure in the working directory nominal resolution  (res) in ppi 300
png('pos_container bartplot TD.png', width=22, height=14, units = 'cm', res = 500)
p.pos.t
dev.off()





################################

#http://www.cookbook-r.com/Manipulating_data/Summarizing_data/

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

