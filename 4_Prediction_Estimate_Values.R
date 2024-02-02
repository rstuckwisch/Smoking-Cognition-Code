library(nlme)
library(JM)
library(lcmm)
library(weightQuant)
library(splines)


load("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/RStudio/Output/models_LMM_REGARDS_Overall.RData")
load("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/RStudio/Output/models_JM_REGARDS_Overall.RData")

# specify the name of the dataset for outputs: 
dataset <- "THESIS"
# specify the name of the outcome for outputs:  
outcome <- "Overall Composite"

############### PREDICTIONS ###########################
summary(dataJM$varY)


#### dataset for new data
datanew <- data.frame(time=seq(0,14,by=2),age0=65,EL=0,male=0,PE=0,white=0,smoke_past=0,smoke_current=0,alc_past=0,alc_current=0)
datanew$age0C <- (datanew$age0 - 65)

#### predictions in JM (with association through level)
datanew$smoke_past=0
datanew$smoke_current=0
JM_pred_never <- predict(JM_D_level,newdata=datanew, se.fit=TRUE, interval='confidence') %>% as.data.frame()

datanew$smoke_past=1
datanew$smoke_current=0
JM_pred_past <- predict(JM_D_level,newdata=datanew, se.fit=TRUE, interval='confidence') %>% as.data.frame()

datanew$smoke_past=0
datanew$smoke_current=1
JM_pred_current <- predict(JM_D_level,newdata=datanew, se.fit=TRUE, interval='confidence') %>% as.data.frame()

### predictions from LMM

datanew$smoke_past=0
datanew$smoke_current=0
Pnever <- predictY(DquadHLME, newdata = datanew, draws=TRUE)
LMM_pred_never <- as.data.frame(Pnever[["pred"]])

datanew$smoke_past=1
datanew$smoke_current=0
Ppast <- predictY(DquadHLME, newdata = datanew, draws=TRUE)
LMM_pred_past <- as.data.frame(Ppast[["pred"]])

datanew$smoke_past=0
datanew$smoke_current=1
Pcurrent <- predictY(DquadHLME, newdata = datanew, draws=TRUE)
LMM_pred_current <- as.data.frame(Pcurrent[["pred"]])

save(JM_pred_never, JM_pred_past, JM_pred_current, LMM_pred_never, LMM_pred_past, LMM_pred_current,
     file=paste("Prediction_Values_",dataset,"_",outcome,".RData",sep=""))





