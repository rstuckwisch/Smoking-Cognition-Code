library(nlme)
library(JM)
library(lcmm)
library(weightQuant)
library(splines)

library(ggplot2)
library(gridExtra)
library(mstate)
library(dplyr)
library(patchwork)
library(reshape2)

load("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/RStudio/Output/models_LMM_REGARDS_Overall.RData")
load("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/RStudio/Output/models_JM_REGARDS_Overall.RData")

# specify the name of the dataset for outputs: 
dataset <- "REGARDS"
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
JMPnever <- predict(JM_D_level,newdata=datanew)
datanew$smoke_past=1
datanew$smoke_current=0
JMPpast <- predict(JM_D_level,newdata=datanew)
datanew$smoke_past=0
datanew$smoke_current=1
JMPcurrent <- predict(JM_D_level,newdata=datanew)

### predictions from LMM

datanew$smoke_past=0
datanew$smoke_current=0
Pnever <- predictY(DquadHLME ,newdata = datanew,draws=T)
datanew$smoke_past=1
datanew$smoke_current=0
Ppast <- predictY(DquadHLME ,newdata = datanew,draws=T)
datanew$smoke_past=0
datanew$smoke_current=1
Pcurrent <- predictY(DquadHLME ,newdata = datanew,draws=T)


###############################################################################
#########################GENERAL PLOT FOR PREDICTIONS##########################
###############################################################################
summary(JMPnever)
summary(dataJM$varY)

#title(sub ="\n \n (for white, female participants, who were 65 years of age at baseline, \n have an education level of less than highschool or high school degree only and have never been alcohol drinkers", 
#      line=4.5,cex.sub=0.75, font.sub=3, col.sub="darkgreen")

# Change the name of the file
pdf(file=paste("Pred_",outcome,".pdf",sep=""),height = 5,width=7)

# limits for Y by default
ylim1 <- c(min(dataJM$varY),max(dataJM$varY))
# change the limits for Y axis for better contrast of the curves
ylim1 <- c(-1.5,0)
# change the limits for Y axis for better contrast of the curves
xlim <- c(min(dataJM$time,na.rm=T),max(dataJM$time,na.rm=T))

### Plot of the predictions for the mixed model (plain) and JM (dashed)

plot(Pnever,shades = T ,xlab="Time (years)",ylab=outcome,col="#1b9e77",lwd=2,
     ylim=ylim1,xlim=xlim,main=paste(outcome," by Smoking Status",sep=""),bty="n",legend=NULL)

plot(Ppast,shades=T,col="#d95f02",add=T,lwd=2)
plot(Pcurrent,shades=T,col="#7570b3",add=T,lwd=2)

lines(JMPnever  ~ datanew$time, type="l",col="#1b9e77",lty=2,lwd=2)
lines(JMPpast  ~ datanew$time, col="#d95f02",lty=2,lwd=2)
lines(JMPcurrent  ~ datanew$time, col="#7570b3",lty=2,lwd=2)


legend(legend = c("never","past","current"),x="topright",bty="n",lty=1,
       col=c("#1b9e77","#d95f02", "#7570b3"),lwd=2)

legend(legend = c("LMM","JM"),x="bottomleft",bty="n",lty=c(1,2),
       col=1,lwd=2)


dev.off()

###############################################################################
##########Prediction Difference between Current Smoker - Never Smoker##########
###############################################################################

# Change the name of the file
pdf(file=paste("Pred_Diff_Current_Never_",outcome,".pdf",sep=""),height = 5,width=7)

ylim1 <- c(-0.5,0)
# change the limits for Y axis for better contrast of the curves
#ylim1 <- c(2,8)


### Plot of the predictions for the mixed model (plain) and JM (dashed)
plot(Pcurrent$pred[,1]-Pnever$pred[,1] ~ datanew$time, type='l',xlim=xlim,
     xlab="Time (years)",ylab=paste("Difference in ",outcome),
     col="#1f78b4",lwd=2,ylim=ylim1,bty="n",legend=NULL)

title(main=paste("Difference in ",outcome,"\n (Current Smoker - Never Smoker)",sep=""))

lines(JMPcurrent- JMPnever ~ datanew$time, type="l",col="#1f78b4",lty=2,lwd=2)

abline(a=0,b=0,col="gray",lwd=2,lty=3)


legend(legend = c("LMM","JM"),x="bottomleft",bty="n",lty=c(1,2),
       col="#1f78b4",lwd=2)


dev.off()


###############################################################################
###########Prediction Difference between Past Smoker - Never Smoker############
###############################################################################

# Change the name of the file
pdf(file=paste("Pred_Diff_Past_Never_",outcome,".pdf",sep=""),height = 5,width=7)

ylim1 <- c(-0.5,0)
plot(Ppast$pred[,1]-Pnever$pred[,1] ~ datanew$time, type='l',xlim=xlim,
     xlab="Time (years)",ylab=paste("Difference in ",outcome),
     col="#1f78b4",lwd=2,ylim=ylim1,bty="n",legend=NULL)

title(main=paste("Difference in ",outcome, "\n (Past Smoker - Never Smoker)",sep=""),
      sub ="\n \n (for age=65, EL=0 ,white=0, sex=0, alc_past=0, alc_current=0)", cex.sub=0.75, font.sub=3, col.sub="black")


lines(JMPpast-JMPnever ~ datanew$time, type="l",col="#1f78b4",lty=2,lwd=2)

abline(a=0,b=0,col="gray",lwd=2,lty=3)

legend(legend = c("LMM","JM"),x="bottomleft",bty="n",lty=c(1,2),
       col="#1f78b4",lwd=2)


dev.off()


###############################################################################
###########Prediction Difference between Current Smoker -Past Smoker###########
###############################################################################


# Change the name of the file
pdf(file=paste("Pred_Diff_Current_Past_",outcome,".pdf",sep=""),height = 5,width=7)

plot(Pcurrent$pred[,1]-Ppast$pred[,1] ~ datanew$time, type='l',xlim=xlim,
     xlab="Time (years)",ylab=paste("difference in ",outcome),
     col="#1f78b4",lwd=2,ylim=ylim1, bty="n",legend=NULL)

title(main=paste("Difference in ",outcome, "\n (Current Smoker - Past Smoker)",sep=""),
           sub ="\n \n (for age=65, EL=0 ,white=0, sex=0, alc_past=0, alc_current=0)", cex.sub=0.75, font.sub=3, col.sub="black")

lines(JMPcurrent-JMPpast ~ datanew$time, type="l",col="#1f78b4",lty=2,lwd=2)

abline(a=0,b=0,col="gray",lwd=2,lty=3)


legend(legend = c("LMM","JM"),x="bottomleft",bty="n",lty=c(1,2),
       col="#1f78b4",lwd=2)

dev.off()