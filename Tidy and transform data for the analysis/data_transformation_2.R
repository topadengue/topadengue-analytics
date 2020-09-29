setwd("C:/R/TopaDengue/Ordenar y transformar datos para analisis")

if (!require('tidyverse')) install.packages("tidyverse")
library(tidyverse)
if (!require('dplyr')) install.packages('dplyr')
library(dplyr)
if (!require('lattice')) install.packages("lattice")
library(lattice)


full_data = read.table("full_df.csv",header=T, sep=",",dec=".") 

names(full_data)

dim(full_data)

summary(full_data)

## Por perido de evaluaciòn 

E1 = filter(full_data, period== "E1", breading_site_positive=="SI")

dim(E1)

E1.occ = data.frame(visity_week_of_year=E1$visity_week_of_year, 
                    visit_year=E1$visit_year, Zona=E1$Zona,
                    breading_site_positive=E1$breading_site_positive, 
                    long=E1$x1, lat=E1$y1)

summary(E1.occ) 

write.csv(E1.occ, file="occurrencias_Evluacion_1.csv")

##

E2 = filter(full_data, period== "E2", breading_site_positive=="SI")

dim(E2)

E2.occ = data.frame(visity_week_of_year=E2$visity_week_of_year, 
                    visit_year=E2$visit_year, Zona=E2$Zona,
                    breading_site_positive=E2$breading_site_positive, 
                    long=E2$x1, lat=E2$y1)

summary(E2.occ) 

write.csv(E2.occ, file="occurrencias_Evluacion_2.csv")

##


E3 = filter(full_data, period== "E3", breading_site_positive=="SI")

dim(E3)

E3.occ = data.frame(visity_week_of_year=E3$visity_week_of_year, 
                    visit_year=E3$visit_year, Zona=E3$Zona,
                    breading_site_positive=E3$breading_site_positive, 
                    long=E3$x1, lat=E3$y1)

summary(E3.occ) 

write.csv(E3.occ, file="occurrencias_Evluacion_3.csv")

##

E4 = filter(full_data, period== "E4", breading_site_positive=="SI")

dim(E4)

E4.occ = data.frame(visity_week_of_year=E4$visity_week_of_year, 
                    visit_year=E4$visit_year, Zona=E4$Zona,
                    breading_site_positive=E4$breading_site_positive, 
                    long=E4$x1, lat=E4$y1)

summary(E4.occ) 

write.csv(E4.occ, file="occurrencias_Evluacion_4.csv")

######################

Int2E2 = filter(full_data, period %in% c("Int2", "E2"), breading_site_positive=="SI")

dim(Int2E2)

Int2E2.occ = data.frame(visity_week_of_year=Int2E2$visity_week_of_year, 
                    visit_year=Int2E2$visit_year, Zona=Int2E2$Zona,
                    breading_site_positive=Int2E2$breading_site_positive, 
                    long=Int2E2$x1, lat=Int2E2$y1)

summary(Int2E2.occ) 

write.csv(Int2E2.occ, file="occurrencias_Intervencion_2_Evluacion_2.csv")

###

Int3E3 = filter(full_data, period %in% c("Int3", "E3"), breading_site_positive=="SI")

dim(Int3E3)

Int3E3.occ = data.frame(visity_week_of_year=Int3E3$visity_week_of_year, 
                        visit_year=Int3E3$visit_year, Zona=Int3E3$Zona,
                        breading_site_positive=Int3E3$breading_site_positive, 
                        long=Int3E3$x1, lat=Int3E3$y1)

summary(Int3E3.occ) 

write.csv(Int3E3.occ, file="occurrencias_Intervencion_3_Evluacion_3.csv")

###

Int4E4 = filter(full_data, period %in% c("Int4", "E4"), breading_site_positive=="SI")

dim(Int4E4)

Int4E4.occ = data.frame(visity_week_of_year=Int4E4$visity_week_of_year, 
                        visit_year=Int4E4$visit_year, Zona=Int4E4$Zona,
                        breading_site_positive=Int4E4$breading_site_positive, 
                        long=Int4E4$x1, lat=Int4E4$y1)

summary(Int4E4.occ) 

write.csv(Int4E4.occ, file="occurrencias_Intervencion_4_Evluacion_4.csv")



## Por trimestre 

tri1 <- filter(full_data, visit_month %in% c(1, 2, 3), visit_year == 2018, 
               breading_site_positive=="SI")
dim(tri1)

tri1.occ = data.frame(visit_month=tri1$visit_month, visit_year=tri1$visit_year,
                      breading_site_positive=tri1$breading_site_positive, long=tri1$x1, lat=tri1$y1)



tri2 <- filter(full_data, visit_month %in% c(4, 5, 6), visit_year == 2018, 
               breading_site_positive=="SI")

dim(tri2)

tri2.occ = data.frame(visit_month=tri2$visit_month, visit_year=tri2$visit_year,
                      breading_site_positive=tri2$breading_site_positive, long=tri2$x1, lat=tri2$y1)

summary(tri2.occ) 

write.csv(tri2.occ, file="occurrencias_trimestre_2.csv")

tri3 <- filter(full_data, visit_month %in% c(7, 8, 9), visit_year == 2018, 
               breading_site_positive=="SI")

dim(tri3)


tri3.occ = data.frame(visit_month=tri3$visit_month, visit_year=tri3$visit_year,
                      breading_site_positive=tri3$breading_site_positive, long=tri3$x1, lat=tri3$y1)

summary(tri3.occ) 

write.csv(tri3.occ, file="occurrencias_trimestre_3.csv")

tri4 <- filter(full_data, visit_month %in% c(10, 11, 12), visit_year == 2018, 
               breading_site_positive=="SI")

dim(tri4)

tri4.occ = data.frame(visit_month=tri4$visit_month, visit_year=tri4$visit_year,
                      breading_site_positive=tri4$breading_site_positive, tri4$Zona
                      ,long=tri4$x1, lat=tri4$y1)

summary(tri4.occ) 

write.csv(tri4.occ, file="occurrencias_trimestre_4.csv")

########################################

# Preparar datos para analisis de tipo ANOVA 
# Unidad muestral definida como:
# Consortium&period&Zona_i&breeding_site_code_tpd&breeding_site_code_senepa&Zona&location_city_block

names(full_data)

##########################
## Container index x UM
#########################

full_data.container.index = filter(full_data, !breeding_site_code_tpd=="N")

df.ci =full_data.container.index 

UM.df=mutate(df.ci, UM=paste(df.ci$consortium,df.ci$period,
                             df.ci$Zona_i,df.ci$Zona,
                             df.ci$location_city_block, sep="&"))

# Ordenar datos para visualizar abundancia de especies por cobertura segun Naido y Hill 2004

conteiner.df=table(UM.df$UM, UM.df$breading_site_positive)

dim(conteiner.df)


cidf=spread(data.frame(conteiner.df), Var2, Freq)

class(cidf)

cidf

CI.df=mutate(cidf, CI=cidf$SI/(cidf$SI+cidf$NO)*100)

CI.df

summary(CI.df)

CI.df=CI.df %>%  rename(UM=Var1)

CI.df=separate(CI.df, UM, into = c("consortium", "period", "zona_i",
                                   "zona", "city_block"), sep = "&")

write.csv(CI.df, file="CI.csv")

#######################################

############################
## Infestaion index x UM
###########################


UM.h1=mutate(full_data, UM=paste(full_data$consortium,full_data$period,
                                full_data$Zona_i,full_data$Zona,
                                full_data$location_city_block,  sep="&"))

UM.h2=mutate(full_data, UM.id=paste(UM.h1$UM, UM.h1$id_observation, UM.h1$location_id, sep="/"))

dim(UM.h2)

UM.h = filter(UM.h2, !breeding_site_code_tpd=="N")#filter(UM.h2, period== "E1")


house=table(UM.h$UM.id,UM.h$breading_site_positive)


dim(house)

house.df=data.frame(house)

house.df

summary(house.df)

hindx=spread(house.df, Var2, Freq)

summary(hindx)



hindx1=separate(hindx, Var1, into = c("block", "container", "house"), sep = "/")

head(hindx1)

class(hindx1)


# https://stackoverflow.com/questions/55914124/compute-the-breteau-index-with-r

#com = commune, house_pros = number of house prospected, c_found = the number of container found in #each commune, pos_container = the number of positive container (container where at least one either #aegypti or albopictus was found) per commune and for each species (aegypti or albopictus) (taking #into account that we may have more than one container in a house), and BI = Breteau index computed #((number of positive container / number of house prospected)*100.

#Container index (CI): percentage of water-holding containers positive with larvae or pupae.
#Breteau index (BI): number of positive containers per 100 houses inspected.


In.index=hindx1 %>% group_by(block) %>% 
  summarise(house_pros=n_distinct(house),   #eqi length(unique(x))  #See dplyr::n_distinct
            c_found=n(),  #n()                       # Size of each group
            pos_container=sum(SI!=0)) %>%  #Num of aegypti !=0
  bind_rows(.,tibble(block='Total',house_pros=sum(.$house_pros), c_found=sum(.$c_found),
                     pos_container=sum(.$pos_container))) %>% 
  mutate(neg_container=(c_found-pos_container),BI=(pos_container/house_pros)*100, 
         CI=(pos_container/c_found)*100,NCI=(neg_container/c_found)*100)

head(In.index)

tail(In.index)

plot(In.index$BI,In.index$CI)

In.index.df=data.frame(In.index)

In.index.df = filter(In.index.df, !block=="Total")

In.index.df=separate(In.index.df, block, into = c("consortium", "period", "zona_i", 
                                                  "zona", "city_block"), sep = "&")

tail(In.index.df)

write.csv(In.index.df, file="Larval_surveys_index.csv")
