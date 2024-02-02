#### 1. Libraries required #####
library(nlme)
library(JM)
library(lcmm)
library(weightQuant)
library(splines)

# specify the name of the dataset for outputs: 
dataset <- "REGARDS"
# specify the name of the outcome for outputs:  
outcome <- "overall"

dataJM <- read.csv("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/RStudio/Output/dataJM_overall.csv", header = TRUE, sep = ",")
dataJM$varY <- dataJM$comp_overall2

## create a  dataset with a unique line per subject
dataS <- unique(dataJM[,c("ID","smoke_past", "smoke_current","timeEvent","indEvent",
                          "age0C", "male", "white", "EL","alc_past", "alc_current")])

### The sample size is: 
length(dataS$ID)
length(dataJM$ID)
### The number of repeated measures by subject: 
quantile(table(dataJM$ID[!is.na(dataJM$varY)]))


### Description : histogram at all visits and at baseline
hist(dataJM$varY,xlab="Composite Score",main="All Overall Composite Scores in REGARDS")
hist(dataJM$varY[dataJM$time==0],xlab="Composite Score",main="Baseline Overall Composite Scores in REGARDS")

### Description: range of the data, mean
summary(dataJM$varY)
summary(dataJM$visit)

# description of smoking status 
table(dataS$smoke_past)
table(dataS$smoke_current)

############### MIXED MODEL WITH HLME #################
dataJM$ID2 <- as.numeric(dataJM$ID)
summary(dataJM$ID2)

#### Linear mixed model with HLME with quadratic shape of trajectory
DquadHLME <- hlme( varY ~ time + I(time^2) + PE + age0C 
                   + smoke_past + smoke_current + alc_past + alc_current + male + EL + white
                   + smoke_past*(time + I(time^2))
                   + smoke_current*(time + I(time^2))
                   + alc_past*(time + I(time^2))
                   + alc_current*(time + I(time^2))
                   + male*(time + I(time^2))
                   + EL*(time + I(time^2))
                   + white*(time + I(time^2)),
                   random=~ time + I(time^2) , subject= "ID2", ng=1, 
                   data = dataJM)
#hlme(..., idiag =  )
  # optional logical arguement for the strugcutre of the variance covariance matrix of the random effects
  # FALSE (default) a non structured matrix of variance/covariance is considered
  #TRUE a diagonal matric of the variance-covariance in sonsidered

sumTabHLME <- summary(DquadHLME)

WaldMult(DquadHLME,pos=c(13,14,15,16))
#The procedure tests whether Q=Q_0, (where Q_0 is a k-vector of values under the null hypothesis) (typically all zeros)
  #https://www.utstat.toronto.edu/~brunner/oldclass/2201s11/handouts/2201s11Wald.pdf
#Wald test 12.95007 p-value 0.01152
  #Reject the Null Hypothesis


save(DquadHLME, dataJM, dataS,
     file=paste("models_LMM_",dataset,"_",outcome,".RData",sep=""))

# later, you can load those objects (and so not have to rerun them)
# by using for instance:
load("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/RStudio/Output/models_LMM_REGARDS_overall.RData")

# load("models_LMM_REGARDS_Overall Composite")

