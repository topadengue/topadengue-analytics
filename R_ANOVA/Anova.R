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

# Removing outliers - quick & dirty

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

