###########
#Hack & Health
#Author: David Conrado Cabanillas Barbacil
#Team: ghostandgoblins
##########

##############################
#Read data
##############################
#setwd("C:/Users/david_2/Desktop/hack&health/hack/hackandhealth/teams/ghostandgoblins/src")
rca_popullation<-read.csv("../etc/data/CST/RCA_popullation.csv",sep=";")

deaths<-read.csv("../etc/data/CST/defuncions.csv",sep=",")

library(dplyr)
deaths %>% filter(EDAT>=18,SEXE!="Altres") %>% group_by(ANNY,SEXE) %>% summarize(M_EDAT=mean(EDAT),count=n()) %>% as.data.frame() ->tx

###NEXT STEPS
#població molt estable a l'estiu similar que a l'hivern

#Forecasting 2030
#Similar population but older one (OPEN DATA)
#CST_dades nens vs vells