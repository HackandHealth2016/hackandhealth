###########
#Hack & Health
#Author: David Conrado Cabanillas Barbacil
#Team: ghostandgoblins
##########

##############################
#Read data
##############################
#setwd("C:/Users/david_2/Desktop/hack&health/hack/hackandhealth/teams/ghostandgoblins/src")
activity<-read.csv("../etc/data/HealthApp/Activity.csv")
centmed<-read.csv("../etc/data/HealthApp/Centmed.csv")
#centmed$CODE<-paste("T",centmed$CODE,sep="")
emotion<-read.csv("../etc/data/HealthApp/Emotion.csv")
episodes<-read.csv("../etc/data/HealthApp/Episodes.csv")
meal<-read.csv("../etc/data/HealthApp/Meal.csv")
patient<-read.csv("../etc/data/HealthApp/Patient.csv")
therapist<-read.csv("../etc/data/HealthApp/Therapist.csv")

info_patient<-merge(activity,emotion,by.x=c("PATIENT","DAY","HOUR"),by.y=c("PATIENT","DAY","HOUR"))
info_patient<-merge(info_patient,episodes,by.x=c("PATIENT","DAY","HOUR"),by.y=c("PATIENT","DAY","HOUR"))
info_patient<-merge(info_patient,meal,by.x=c("PATIENT","DAY","HOUR"),by.y=c("PATIENT","DAY","HOUR"))
info_patient<-merge(info_patient,patient,by.x=c("PATIENT"),by.y=c("NAME"))
info_patient<-merge(info_patient,therapist,by.x=c("THERAPIST"),by.y=c("NAME"))
info_patient<-merge(info_patient,centmed,by.x=c("CENTMED"),by.y=c("CODE"))
info_patient$X<-NULL
summary(info_patient)
#########################################################################
#PATIENT
#install.packages("dplyr")
library(dplyr)

#info_patient<-(info_patient[(info_patient$EPISODE)!="",])
info_patient<-(info_patient[(info_patient$ACTIVITY)!="",])
info_patient$EPISODE<-as.character(info_patient$EPISODE)
info_patient$EPISODE[(info_patient$EPISODE)==""]<-"NOT_EPISODE"
info_patient$EPISODE<-as.factor(info_patient$EPISODE)
info_patient %>% group_by(PATIENT,ACTIVITY,EMOTION,MEAL,EPISODE) %>% summarize(count=n()) %>% as.data.frame() ->t_PATIENT

#with pattern P14,P11,P4,P9
P14<-(info_patient[(info_patient$PATIENT=="P14"),])

#without pattern P9,P8,P7,
P9<-(info_patient[(info_patient$PATIENT=="P9"),])

#Find out the best and worst therapist

#########################################################################

#########################################################################
#PATIENT Gender
#sum(patient$GENDER=="F")
#26
#sum(patient$GENDER=="M")
#4

info_patient %>% group_by(GENDER) %>% summarize(count=n()) %>% as.data.frame() ->t_PATIENT
#GENDER count
#F      7170 events
#M      1101  events

F_events<-info_patient[info_patient$GENDER=="F",]
F_events<-F_events[c(1:1101),]
M_events<-info_patient[info_patient$GENDER=="M",]

events<-rbind(F_events,M_events)  
events %>% group_by(GENDER,ACTIVITY) %>% summarize(count=n()) %>% as.data.frame() ->t_PATIENT

#GENDER ACTIVITY COUNT
#1 F ALONE 39
#2 F FRIENDS 15
#3 F SPORTS 2
#4 F STUDYING/WORKING 26
#5 M ALONE 54
#6 M FRIENDS 12
#7 M STUDYING/WORKING 16

#library(ggplot2)
#ggplot(t_PATIENT, aes(x = EMOTION, y = GENDER, fill = GENDER)) + geom_boxplot() +
#facet_wrap(~ EMOTION, ncol = 5)
#GENDER EMOTION COUNT
#1 F DISTRESSED 24
#2 F FUSTRATED 20
#3 F GUILTY 18
#4 F SAD 20
#5 M DISTRESSED 25
#6 M FUSTRATED 21
#7 M GUILTY 21
#8 M SAD 15

events %>% group_by(GENDER,MEAL) %>% summarize(count=n()) %>% as.data.frame() ->t_PATIENT
#GENDER MEAL COUNT
#1 F + 3 THIRD 16
#2 F 1 THIRD 13
#3 F 2 THIRD 29
#4 F NOTHING 24
#5 M + 3 THIRD 27
#6 M 1 THIRD 22 
#7 M 2 THIRD 12
#8 M NOTHING 21

events %>% group_by(GENDER,EPISODE) %>% summarize(count=n()) %>% as.data.frame() ->t_PATIENT
#GENDER EPISODE COUNT
#1 F BINGE 17 
#2 F RESTRICTION 47 
#3 F VOMITED 18
#4 M BINGE 27
#5 M RESTRICTION 42
#6 M VOMITED 13
#########################################################################
#tables
# table(info_patient$THERAPIST)
#T1 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19  T2 T20  T3  T4  T5  T6  T7  T8  T9 
#552 276 552 276 552 276 552 276 552 276 552 276 276 552 276 552 276 552 276 552 

#table(info_patient$MEAL)
#+  3 THIRD    1 THIRD    2 THIRD    3 THIRD    NOTHING 
#       403       2651       2658       2171        388

#table(info_patient$ACTIVITY)
#ALONE           FAMILY          FRIENDS           SPORTS STUDYING/WORKING 
#  872             1330             1617             2634             1818 

#table(info_patient$EMOTION)
#DISTRESSED  FUSTRATED     GUILTY      HAPPY    RELAXED        SAD 
#       380        418        388       1688       3336       2061 

#table(info_patient$EPISODE)
#BINGE RESTRICTION     VOMITED 
#405         562         502            and  6811 NOTHING  SPECIAL
#########################################################################

# Boxplot of MPG by Car Cylinders 
#boxplot(THERAPIST~PATIENT,data=info_patient, main="Car Milage Data", 
#        xlab="Number of Cylinders", ylab="Miles Per Gallon")
#########################################################################


#DPLYR
#GROUP_BY INFORMATION
library(dplyr)
#Per hour we want #ACTIVITY, EMOTION, EPISODE, MEAL
#info_patient$EPISODE<-as.character(info_patient$EPISODE)
#info_patient$EPISODE[(info_patient$EPISODE)==""]<-"NOT_EPISODE"
info_patient %>% group_by(HOUR,ACTIVITY,EMOTION,MEAL,EPISODE) %>% summarize(count=n()) %>% as.data.frame() ->t_HOUR

t_HOUR<-(t_HOUR[(t_HOUR$EPISODE)!="",])
t_HOUR<-(t_HOUR[(t_HOUR$ACTIVITY)!="",])
t_HOUR$EPISODE<-as.factor(t_HOUR$EPISODE)
t_HOUR.morning<-t_HOUR[t_HOUR$HOUR=="00:00 - 10:00",]
t_HOUR.afternoon<-t_HOUR[t_HOUR$HOUR=="10:00 - 16:00",]
t_HOUR.evening<-t_HOUR[t_HOUR$HOUR=="16:00 - 24:00",]

########################
#DECISION TREE
#HOUR,ACTIVITY,EMOTION,MEAL=>EPISODE  ???
#http://www.rdatamining.com/examples/association-rules
########################
t_HOUR.morning[t_HOUR.morning$EPISODE!="NOT_EPISODE",]
t_HOUR.morning_rules<-t_HOUR.morning[,c(2:5)]
library(arules)
# find association rules with default settings
#rules <- apriori(t_HOUR.morning_rules)
#inspect(rules)
# rules with rhs containing "Survived" only
rules <- apriori(t_HOUR.morning_rules,parameter = list(minlen=2, supp=0.005, conf=0.8),appearance = list(rhs=c("EPISODE=BINGE", "EPISODE=RESTRICTION", "EPISODE=VOMITED"),default="lhs"),control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#Visualizing Association Rules
#install.packages("dplyr")
#install.packages("arulesViz")
library(arulesViz)
plot(rules.pruned)

plot(rules.pruned, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))
#plot(rules, method="graph", control=list(type="items"))
#plot(rules, method="paracoord", control=list(reorder=TRUE))

t_HOUR.afternoon[t_HOUR.afternoon$EPISODE!="NOT_EPISODE",]
t_HOUR.afternoon_rules<-t_HOUR.afternoon[,c(2:5)]
library(arules)
# find association rules with default settings
#rules <- apriori(t_HOUR.morning_rules)
#inspect(rules)
# rules with rhs containing "Survived" only
rules <- apriori(t_HOUR.afternoon_rules,parameter = list(minlen=2, supp=0.005, conf=0.8),appearance = list(rhs=c("EPISODE=BINGE", "EPISODE=RESTRICTION", "EPISODE=VOMITED"),default="lhs"),control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
plot(rules.pruned, method="graph", control=list(type="items"))
plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))


t_HOUR.evening_rules<-t_HOUR.evening[,c(2:5)]
t_HOUR.evening[t_HOUR.evening$EPISODE!="NOT_EPISODE",]

library(arules)
# find association rules with default settings
#rules <- apriori(t_HOUR.morning_rules)
#inspect(rules)
# rules with rhs containing "Survived" only
rules <- apriori(t_HOUR.evening_rules,parameter = list(minlen=2, supp=0.005, conf=0.8),appearance = list(rhs=c("EPISODE=NOT_EPISODE","EPISODE=BINGE", "EPISODE=RESTRICTION", "EPISODE=VOMITED"),default="lhs"),control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
plot(rules.pruned, method="graph", control=list(type="items"))
plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))

###ALL
t_HOUR.all_rules<-t_HOUR[,c(2:5)]
library(arules)
# find association rules with default settings
#rules <- apriori(t_HOUR.morning_rules)
#inspect(rules)
# rules with rhs containing "Survived" only
rules <- apriori(t_HOUR.all_rules,parameter = list(minlen=2, supp=0.005, conf=0.8),appearance = list(rhs=c("EPISODE=NOT_EPISODE","EPISODE=BINGE", "EPISODE=RESTRICTION", "EPISODE=VOMITED"),default="lhs"),control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))
########################
#DECISION TREE
#HOUR,ACTIVITY,EMOTION,MEAL=>EPISODE
########################



info_patient<-(info_patient[(info_patient$EPISODE)!="",])
info_patient<-(info_patient[(info_patient$ACTIVITY)!="",])
info_patient$EPISODE<-as.factor(info_patient$EPISODE)
info_patient.morning<-info_patient[info_patient$HOUR=="00:00 - 10:00",]
info_patient.morning_rules<-info_patient.morning[,c(5:8)]

rules <- apriori(info_patient.morning_rules,parameter = list(minlen=2, supp=0.005, conf=0.8),appearance = list(rhs=c("EPISODE=BINGE", "EPISODE=RESTRICTION", "EPISODE=VOMITED"),default="lhs"),control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
plot(rules.pruned, method="graph", control=list(type="items"))
plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))


################################
################################
################################
#Clasification
#https://www.datacamp.com/community/tutorials/machine-learning-in-r#gs.QkpJr_8
set.seed(1234)
info_patient<-(info_patient[(info_patient$EPISODE)!="",])
info_patient<-(info_patient[(info_patient$ACTIVITY)!="",])
ind <- sample(2, nrow(info_patient), replace=TRUE, prob=c(0.67, 0.33))


#indx <- sapply(info_patient, is.factor)
indx<-c(2,5,6,7,9,11,12)
info_patient[indx] <- lapply(info_patient[indx], function(x) as.numeric(x))
info_patient.training <- info_patient[ind==1, c(2,5,6,7,9,11,12)]
info_patient.test <- info_patient[ind==2,c(2,5,6,7,9,11,12)]


info_patient.trainLabels <- info_patient[ind==1, 8]
info_patient.testLabels <- info_patient[ind==2, 8]

library(class)
info_patient_pred <- knn(train = info_patient.training, test = info_patient.test, cl = info_patient.trainLabels, k=3)

#install.packages("gmodels")
library(gmodels)
CrossTable(x = info_patient.testLabels, y = info_patient_pred, prop.chisq=FALSE)



