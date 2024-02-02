#### 1. Libraries required #####
library(nlme)
library(JM)
library(lcmm)
library(weightQuant)
library(splines)
library(readxl)
library(nlme)

dataJM <- read.csv("S:/Regards/analysis/Stuckwisch/THESIS/Code/Differences/Overall/RStudio/Output/dataJM_overall.csv", header = TRUE, sep = ",")
dataJM$varY <- dataJM$comp_overall2

dataS <- unique(dataJM[,c("ID","smoke_past", "smoke_current","timeEvent","indEvent",
                          "age0C", "male", "white", "EL","alc_past", "alc_current")])


# specify the name of the dataset for outputs: 
dataset <- "REGARDS"
# specify the name of the outcome for outputs:  
outcome <- "overall"


### The sample size is: 
length(dataS$ID)
length(dataJM$ID)
### The number of repeated measures by subject: 
quantile(table(dataJM$ID[!is.na(dataJM$varY)]))


### Description : histogram at all visits and at baseline
hist(dataJM$varY,xlab="outcome",main="All Overall Composite Scores in REGARDS")
hist(dataJM$varY[dataJM$time==0],xlab="outcome",main="Baseline Overall Composite Scores in REGARDS")

### Description: range of the data, mean
summary(dataJM$varY)


# description of smoking status 
table(dataS$smoke_past)
table(dataS$smoke_current)

########## JOINT MODEL WITH A SINGLE TYPE OF EVENT #######

#### Summarize the types of event in a single one
dataS$indSingleEvent <- (!(dataS$indEvent==0))

DquadLME <- lme( varY ~ time + I(time^2) 
                 + PE + age0C 
                 + smoke_past + smoke_current + alc_past + alc_current 
                 + male + EL + white
                 + smoke_past*(time + I(time^2))
                 + smoke_current*(time + I(time^2))
                 + alc_past*(time + I(time^2))
                 + alc_current*(time + I(time^2))
                 + male*(time + I(time^2))
                 + EL*(time + I(time^2))
                 + white*(time + I(time^2)),  # put the adjustment after that and
                                              # if you add some interactions with time, 
                                              # remember to put them also in 
                                              # the Dform of the joint model (survival part)
                 random=~ time + I(time^2) | ID, 
                 data = dataJM,method='ML',
                 control=lmeControl(maxIter = 15000,   ## maximum number of iterations for the lme optimization algorithm
                                    msMaxIter = 15000, ## maximum number of iterations for the nlm optimization step inside the lme optimization
                                    tolerance = 0.001, ## tolerance for the convergence criterion in the lme algorithm
                                    niterEM=15000))   ## niterEM: number of iterations for the EM algorithm used to refine the initial estimates of the random effects variance-covariance coefficients
                                    
summary(DquadLME)

save(DquadLME,
     file=paste("models_LMM_for_JM_",dataset,"_",outcome,".RData",sep=""))

#### Survival model with CoxPH

coxFit <- 
  coxph(formula = Surv(timeEvent, indSingleEvent) ~ smoke_past + smoke_current + alc_past + alc_current + male + age0C + EL + white,
        data = dataS, 
        x = TRUE)   # x : outputs the design matrix associated with covariates
summary(coxFit)


#### Joint Model  with current level

JM_D_level <- 
  jointModel(lmeObject = DquadLME,
             survObject = coxFit,
             timeVar = "time", # time variable in the mixed model
             parameterization = "value", # dependence structure 
             method = "spline-PH-aGH", # baseline risk model
             verbose = TRUE, # trace output
             control = list(GHk = 5))
summary(JM_D_level)

#### jointmodel()
  #lmeObject an object inheriting from class lme (see also Note).
  #survObject an object inheriting from class coxph or class survreg. In the call to coxph() or survreg(), 
      #you need to specify the argument x = TRUE such that the design matrix is contained in the object fit. See Examples.
  #timeVar a character string indicating the time variable in the linear mixed effects model.
  #parameterization - a character string indicating the type of parameterization. See Details
  #method - a character string specifying the type of joint model to fit. See Details
    #GH stands for Gauss-Hermite ; using aGH invokes the psuedo-adaptive Gauss-Hermite rule
  #GHk the number of Gauss-Hermite quadrature points used to approximate the integrals over the random effects. 
      #The default is 15 for one- or two-dimensional integration and for N < 2000, and 9 otherwise for the simple 
      #GaussHermite rule, and 5 for one-, two-dimensional or three-dimensional integration and for N < 2000, 
      #and 3 otherwise for the pseudo adaptive GaussHermite rule, where N denotes the total number of longitudinal measurements

###https://cran.r-project.org/web/packages/JM/JM.pdf



############### SAVE THE MODELS #######################
save(JM_D_level, DquadLME, dataJM, dataS,
     file=paste("models_JM_",dataset,"_",outcome,".RData",sep=""))

# later, you can load those objects (and so not have to rerun them)
# by using for instance:
#load("S:/Regards/analysis/Stuckwisch/THESIS/R Code/models_JM_REGARDS_Memory.RData")
