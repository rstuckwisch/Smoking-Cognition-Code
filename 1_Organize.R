#### 1. Libraries required #####
library(haven)
library(readxl)
library(data.table)
library(tidyverse)

#### 2. Creation of the data set for the analysis #######

# generic names for variables and data
# data = dataframe with the sample
#
# ID = Identifier of the participant - should be numeric to avoid any problems with packages
# age0C = age at baseline minus 65
# male = indicator for male
# EL = binary education level with low (less than hs/hs graduate) = 0 and high (some college/college +) = 1
# time = delay in the study in decades (current time - time of entry of the participant)
# varY = outcome
# prevalent = indicator of prevalent dementia at inclusion
#             SIS less than or equal to 4 = 1
#              SIS greater than 4 = 0
# timeEvent = time to first event between death, dropout or observed at the end of the study
#             indicated in decades
#             a person who dies more than 3 years after the last visit is considered to have dropped out at the last visit
#             (dementia is considered as dropout)
# indEvent = status indicator at the end of the study, which equals 2 if death; 1 if dropout; 0 if observed 
# visit = indicator of visits (1 2 3 ...)
# PE = practice effect ie indicator that the visit is the first one
# smoke_past = indicator for past smoker with 0 = curren + never smoke and 1 = past smoker
# smoke_current = indicator for current smoker with 0 =past + never smoke and 1 = current smoker
# alc_past = indicator for psat alcohol use with 0 = never and current and 1 = past use
# alc_current = indicator for current alcohol use with 0 = never and past and 1 = current use


# timeDeath = visit corresponding to time to death (first visit after death)

# a. loading of the dataset (longitudinal format) without any selection


data <-read_excel("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/composite_overall.xlxs.xlsx")
data <-  subset(data, select = -c(Smoke, Alc_Use))

# specify the name of the dataset for outputs: 
dataset <- "REGARDS"
# specify the name of the outcome for outputs:  
outcome <- "overall"


#### 3. Selection of the population #######


# with varY observed at baseline
# not demented at baseline
# no missing values for gender, education and age at baseline


# To do so I select the data at baseline in data0 and select the ID to include in ID
data0 <- data[data$time==0,]

ID <- (data0$ID[which((!is.na(data0$comp_overall2))&(!(data0$prevalent==1))&
                       (!is.na(data0$prevalent))&(!is.na(data0$EL))&
                       (!is.na(data0$male))&(!is.na(data0$white))&
                       (!is.na(data0$smoke_past))&(!is.na(data0$smoke_current))&
                       (!is.na(data0$alc_past))&(!is.na(data0$alc_current)))])

dataL <-  data[which(data$ID%in%ID),]

# verification that there is no problem: the next computation should give 0
sum(unique(ID) - unique(dataL$ID))

summary(dataL$prevalent)
uniqueL <- unique(dataL[,c("ID")])
# selection of the subjects who were not immediately censored 
# and the non missing observations for varY
#dataJM <- dataL[!(dataL$timeEvent==0)&!is.na(dataL$varY)&(dataL$time < dataL$timeEvent),]

temp1 <- dataL[!(dataL$timeEvent==0),]
temp2 <- dataL[!(dataL$timeEvent==0)&(!is.na(dataL$comp_overall2)),]
temp3 <- dataL[!(dataL$timeEvent==0)&(!is.na(dataL$comp_overall2))&(dataL$time <= dataL$timeEvent),] 
temp4 <- temp3[!is.na(temp3$time)&!is.na(temp3$white),] 
temp5 <- temp4[!is.na(temp4$smoke_past)&!is.na(temp4$smoke_current),] 
temp6 <- temp5[!is.na(temp5$alc_past)&!is.na(temp5$alc_current)&!is.na(temp5$EL),] 
dataJM <- temp6
dataJM <- subset(dataJM, select = c(ID, age0C, smoke_past, smoke_current, alc_past, alc_current,
                        male, white, EL, time, comp_memory2, comp_fluency, comp_overall2, prevalent, timeEvent, indEvent,
                        visit, PE))
dataJM <- data.frame(dataJM)
dataS <- unique(dataJM[,c("ID")])




write.csv(dataJM, "S:/Regards/analysis/Stuckwisch/THESIS/Code/Differences/Overall/RStudio/Output/dataJM_overall.csv")