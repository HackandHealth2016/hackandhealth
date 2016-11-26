## 0: SETUP
## ========

# Set seed
set.seed(42)

setwd("Z:/DATA/Dropbox/METRO/HandH/data/HealthApp")

# Dependencies
library(data.table)
library(Matrix)
library(caret)
library(xgboost)
library(matrixStats)

# Define file paths
activity_csv <- "h_activity.csv"
patients_csv <- "h_patients.csv"
emotion_csv <- "h_emotion.csv"
meals_csv <- "h_meal.csv"
episodes_csv <- "h_episodes.csv"

dtp <- fread(patients_csv)
dt1 <- fread(activity_csv)
dt2 <- fread(emotion_csv)
dt3 <- fread(meals_csv)
dt4 <- fread(episodes_csv)

setnames(dtp, "NAME", "PATIENT")
 
dtc11 <-  merge(x = dtp, y = dt1, by = c("PATIENT"), all = TRUE)
dtc11 <-  merge(x = dtc11, y = dt2, by = c("PATIENT", "DAY", "HOUR"), all = TRUE)
dtc11 <-  merge(x = dtc11, y = dt3, by = c("PATIENT", "DAY", "HOUR"), all = TRUE)
dtc11 <-  merge(x = dtc11, y = dt4, by = c("PATIENT", "DAY", "HOUR"), all = TRUE)
dtc11
summary(dtc11)

unique(dtc11[,EMOTION])

dtc11[,E1:=ifelse(EMOTION=="RELAXED", 1,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,E2:=ifelse(EMOTION=="SAD", 1,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,E3:=ifelse(EMOTION=="HAPPY", 1,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,E4:=ifelse(EMOTION=="DISTRESSED", 1,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,E5:=ifelse(EMOTION=="GUILTY", 1,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,E6:=ifelse(EMOTION=="FUSTRATED", 1,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,E7:=ifelse(EMOTION=="", 1,0),  by=list(PATIENT, DAY, HOUR)]

dtc11[,E1:=sum(E1),  by=list(PATIENT, DAY)]
dtc11[,E2:=sum(E2),  by=list(PATIENT, DAY)]
dtc11[,E3:=sum(E3),  by=list(PATIENT, DAY)]
dtc11[,E4:=sum(E4),  by=list(PATIENT, DAY)]
dtc11[,E5:=sum(E5),  by=list(PATIENT, DAY)]
dtc11[,E6:=sum(E6),  by=list(PATIENT, DAY)]
dtc11[,E7:=sum(E7),  by=list(PATIENT, DAY)]

dtc11
unique(dtc11[,ACTIVITY])

dtc11[,A1:=ifelse(ACTIVITY=="FAMILY", 1,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,A2:=ifelse(ACTIVITY=="STUDYING/WORKING", 1,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,A3:=ifelse(ACTIVITY=="SPORTS", 1,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,A4:=ifelse(ACTIVITY=="FRIENDS", 1,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,A5:=ifelse(ACTIVITY=="ALONE", 1,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,A6:=ifelse(ACTIVITY=="", 1,0),  by=list(PATIENT, DAY, HOUR)]

dtc11[,A1:=sum(A1),  by=list(PATIENT, DAY)]
dtc11[,A2:=sum(A2),  by=list(PATIENT, DAY)]
dtc11[,A3:=sum(A3),  by=list(PATIENT, DAY)]
dtc11[,A4:=sum(A4),  by=list(PATIENT, DAY)]
dtc11[,A5:=sum(A5),  by=list(PATIENT, DAY)]
dtc11[,A6:=sum(A6),  by=list(PATIENT, DAY)]


unique(dtc11[,MEAL])

dtc11[,DM1:= ifelse(MEAL=="1 THIRD", 1,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,DM2:= ifelse(MEAL=="2 THIRD", 2,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,DM3:= ifelse(MEAL=="3 THIRD", 3,0),  by=list(PATIENT, DAY, HOUR)]
dtc11[,DM4:= ifelse(MEAL=="+  3 THIRD", 4,0),  by=list(PATIENT, DAY, HOUR)]

dtc11[,M:=DM1+DM2+DM3+DM4,  by=list(PATIENT, DAY,HOUR)]
dtc11[,M:=sum(M),  by=list(PATIENT, DAY)]

unique(dtc11[,EPISODE])

dtc11[,EP:=0,  by=list(PATIENT, DAY)]
dtc11[,EP:=ifelse(EPISODE=="RESTRICTION", 1,EP),  by=list(PATIENT, DAY, HOUR)]
dtc11[,EP:=ifelse(EPISODE=="VOMITED", 1,EP),  by=list(PATIENT, DAY, HOUR)]
dtc11[,EP:=ifelse(EPISODE=="BINGE", 1,EP),  by=list(PATIENT, DAY, HOUR)]

dtc11[,EP:=sum(EP),  by=list(PATIENT, DAY)]
dtc11

dtc22 <- dtc11[dtc11$HOUR == "10:00 - 16:00", ]
names(dtc22)

dtc22[,ACTIVITY:=NULL]
dtc22[,EMOTION:=NULL]
dtc22[,MEAL:=NULL]
dtc22[,CITY:=NULL]
dtc22[,THERAPIST:=NULL]
dtc22[,HOUR:=NULL]
dtc22[,EPISODE:=NULL]
dtc22[,DM1:=NULL]
dtc22[,DM2:=NULL]
dtc22[,DM3:=NULL]
dtc22[,EP1:=NULL]
dtc22[,EP2:=NULL]
dtc22[,EP3:=NULL]
dtc22[,GENDER:=NULL]
dtc22[,ENVIRONMENT:=NULL]


dtc22[,DAY:=as.Date(DAY,"%d/%m/%Y")]


dtc22<-dtc2[order(PATIENT,DAY)]

names(dtc22)

library(zoo)

dtdt<-data.table()
patients <- seq(from = 1, to = 30, by = 1)
for (p in patients){

#pat<-eval(sub("([[:space:]])","",paste("P",p)))
  pat<-paste("P",p,sep="")
  dp<-dtc22[PATIENT==pat, ]

dpts<-as.ts(dp, DAY)

dpts7<-rollmean(dpts,7,fill=NA, align = c("right"))
dpts15<-rollmean(dpts,15,fill=NA, align = c("right"))

dptst<-as.data.table(dpts)
dptst[,PATIENT:=NULL]
dptst[,PATIENT:=pat]
dptst[,DAY:=NULL]
dptst$DAY<-seq.int(nrow(dptst))

dpts7dt<-as.data.table(dpts7)
dpts7dt[,PATIENT:=NULL]
dpts7dt[,PATIENT:=pat]

dpts7dt[,DAY:=NULL]
dpts7dt$DAY<-seq.int(nrow(dpts7dt))

dpts15dt<-as.data.table(dpts15)
dpts15dt[,PATIENT:=NULL]
dpts15dt[,PATIENT:=pat]

dpts15dt[,DAY:=NULL]
dpts15dt$DAY<-seq.int(nrow(dpts15dt))

names(dpts7dt)<-c(    "E1m7",      "E2m7" ,     "E3m7"  ,    "E4m7",      "E5m7",
                  "E6m7"  ,    "E7m7" ,     "A1m7"  ,   
                  "A2m7"  ,    "A3m7" ,     "A4m7"    ,  "A5m7" ,     "A6m7"  ,    "Mm7"  ,     "EPm7" ,
                  "PATIENT", "DAY")

names(dpts15dt)<-c(     "E1m15",      "E2m15" ,     "E3m15"  ,    "E4m15",      "E5m15",
                  "E6m15"  ,    "E7m15" ,     "A1m15"  ,   
                  "A2m15"  ,    "A3m15" ,     "A4m15"    ,  "A5m15" ,     "A6m15"  ,    "Mm15"  ,     "EPm15" 
                  ,"PATIENT", "DAY")

dp <-  merge(x = dptst, y = dpts7dt, by = c("PATIENT", "DAY"), all = TRUE)
dp <-  merge(x = dp, y = dpts15dt, by = c("PATIENT", "DAY"), all = TRUE)
dtdt<-rbind(dtdt,dp)

}

write.csv(dtdt, "dtdt.csv", row.names = F)
summary(dtc2)
summary(dtc22)
