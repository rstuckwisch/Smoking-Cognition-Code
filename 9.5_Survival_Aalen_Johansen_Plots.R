library(ggplot2)
library(gridExtra)
library(mstate)
library(dplyr)
library(patchwork)
library(reshape2)

load("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/RStudio/Output/models_LMM_REGARDS_Overall.RData")

# specify the name of the dataset for outputs: 
dataset <- "REGARDS"
# specify the name of the outcome for outputs:  
outcome <- "Overall Composite"


### Cumulative transition intensities for dropout and death ----
#(accounting for competing risks)
library(mstate)
library(survival)

# Definition of the possible transitions between states: Health, dropout, death
# Transition 1: health -> dropout
# Transition 2: health -> death
trans <- trans.illdeath()
colnames(trans) <- c("health", 'Dropout','Death')
rownames(trans) <- c("health", 'Dropout','Death')
trans[2,3]<-NA
trans

# Definition of the death and dropout indicators
table(dataS$indEvent)
dataS$IDeath   <- ifelse(dataS$indEvent ==2,1,0)
dataS$IDropout <- ifelse(dataS$indEvent ==1,1,0)
dataS$Death    <- dataS$timeEvent
dataS$Dropout  <- dataS$timeEvent
head(dataS)

dataAJ <- msprep(data = dataS, trans = trans, time = c(NA, "Dropout", "Death"), 
                 status = c(NA, "IDropout", "IDeath"), keep = c("smoke_past", "smoke_current"),
                 id="ID")
head(dataAJ)
names(dataAJ)[1] <- "id"


prnever <-  subset(dataAJ, smoke_past==0 & smoke_current==0)
prpast <- subset(dataAJ, smoke_past==1 & smoke_current==0)
prcurrent <-  subset(dataAJ, smoke_past==0 & smoke_current==1)

attr(prnever, "trans") <- trans
attr(prpast, "trans") <- trans
attr(prcurrent, "trans") <- trans

c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), data=prnever)
c1 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), data=prpast)
c2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), data=prcurrent)

#estimation of transition hazards and associated (co)variances
msf0 <- msfit(c0, trans=trans)
msf1 <- msfit(c1, trans=trans)
msf2 <- msfit(c2, trans=trans)


# Estimated cumulative hazard values at all event times 
par(mfrow=c(1,2))
plot(msf0$Haz$Haz[msf0$Haz$trans==1]~msf0$Haz$time[msf0$Haz$trans==1],type='l',
     ylab="Cumulative Hazard - Dropout", xlab="Time", ylim=c(min(msf0$Haz$Haz), max(msf0$Haz$Haz)))
lines(msf1$Haz$Haz[msf1$Haz$trans==1]~msf1$Haz$time[msf1$Haz$trans==1],lty=2)
lines(msf2$Haz$Haz[msf2$Haz$trans==1]~msf2$Haz$time[msf2$Haz$trans==1],lty=2)
legend("topleft",c("never","past", "current"), lty=c(1,2,3))

plot(msf0$Haz$Haz[msf0$Haz$trans==2]~msf0$Haz$time[msf0$Haz$trans==2],type='l',
     ylab="Cumulative Hazard - Death", xlab="Time", ylim=c(min(msf0$Haz$Haz), max(msf0$Haz$Haz)))
lines(msf1$Haz$Haz[msf1$Haz$trans==2]~msf1$Haz$time[msf1$Haz$trans==2],lty=2)
lines(msf2$Haz$Haz[msf2$Haz$trans==2]~msf2$Haz$time[msf2$Haz$trans==2],lty=2)
legend("topleft",c("never","past", "current"), lty=c(1,2,3))



pt0 <- probtrans(msf0, predt=0)[[1]]
pt1 <- probtrans(msf1, predt=0)[[1]]
pt2 <- probtrans(msf2, predt=0)[[1]]

save(
  pt0, pt1, pt2,
  file=paste("death_dropout_output_",dataset,"_",outcome,".RData",sep=""))

##Make Aalen-Johansen Plots

pdf(file=paste("Aalen-Johansen_plots",dataset,"_",outcome,".pdf",sep=""),height = 3,width=7)
par(mfrow=c(1,3))

plot(pt0$time, pt0$pstate2, type="s", lwd=2, lty=3, ylim=c(0,1), col='#1b9e77',
     xlab="Time", ylab="Probability")
lines(pt1$time, pt1$pstate2, type="s", lwd=2, lty=3, col='#d95f02')
lines(pt2$time, pt2$pstate2, type="s", lwd=2, lty=3, col='#7570b3')
legend("topright", c("Never", "Past", "Current"), lwd=2, lty=c(3,3,3),  col=c('#1b9e77', '#d95f02', '#7570b3'), bty="n")
title(main="Aalen-Johansen \n Dropout")

plot(pt0$time, pt0$pstate3, type="s", lwd=2, lty=3, ylim=c(0,1), col='#1b9e77',
     xlab="Time", ylab="Probability")
lines(pt1$time, pt1$pstate3, type="s", lwd=2, lty=3, col='#d95f02')
lines(pt2$time, pt2$pstate3, type="s", lwd=2, lty=3, col='#7570b3')
legend("topright", c("Never", "Past", "Current"), lwd=2, lty=c(3,3,3),  col=c('#1b9e77', '#d95f02', '#7570b3'), bty="n")
title(main="Aalen-Johansen \n Death")
dev.off()






####Leave in Healthy State (no death or drop out)
pdf(file=paste("Aalen-Johansen_plots",dataset,"_",outcome,".pdf",sep=""),height = 3,width=7)
par(mfrow=c(1,3))
plot(pt0$time, pt0$pstate1, type="s", lwd=2,  ylim=c(0,1), col='#1b9e77',
     xlab="Time", ylab="Probability")
lines(pt1$time, pt1$pstate1, type="s", lwd=2, lty=3, col='#d95f02')
lines(pt2$time, pt2$pstate1, type="s", lwd=2, lty=3, col='#7570b3')
legend("topright", c("Never", "Past", "Current"), lwd=2, lty=c(1,3), col=c('#1b9e77', '#d95f02', '#7570b3'), bty="n")
title(main="Aalen-Johansen \n Leave Health state")
