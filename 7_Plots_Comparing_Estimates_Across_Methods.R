library(ggplot2)
library(gridExtra)
library(mstate)
library(dplyr)
library(patchwork)
library(reshape2)

# specify the name of the dataset for outputs: 
dataset <- "Thesis"
# specify the name of the outcome for outputs:  
outcome <- "Overall Composite"

times_pred  <- c(0, 4, 8, 12, 14)
#times_pred  <- c(0, 5, 15,20)/10
#times_pred<-times_pred[which(times_pred<=maxtime)]

############### PLOTS ###########################

### Comparison of Smoking estimates Between Past and Never Smoked ---- 

load("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/RStudio/Output/estim_current_never_overall.RData")

xlim <- c(min(estim_CN$estim-estim_CN$se),max(estim_CN$estim+estim_CN$se))

for(t in times_pred){
  
  p <- ggplot(estim_CN[estim_CN$time==t,], aes(estim,  as.factor(method)))+
    ylab("Estimates") + xlab(paste("t =",t,sep=""))
  
  limits_low <- aes(y = as.factor(method), yend = as.factor(method), x = estim - 1.96*se, xend = estim + 1.96*se)
  
  p <- p + geom_point() + 
    geom_segment(data=estim_CN[estim_CN$time==t,], limits_low, width=0.2)+
    xlim(xlim)
  
  if(t==times_pred[1]){
    p1 <- p
    p2 <- ggplot(estim_CN[estim_CN$time==t,], aes(estim,  as.factor(method)))
    p3 <- p2 
    p4 <- p2
  }else if(t==times_pred[2]){
    p2 <- p
  }else if(t==times_pred[3]){
    p3 <- p
  }else{
    p4 <- p
  }
}

grid.arrange(p1, p2, p3, p4, ncol = 1)
dev.off()

####Final Plot for Estimates_Difference_Current_Never

p_estimates <- grid.arrange(p1, p2, p3, p4, ncol = 1)
p_estimates 

p_estimates2 <- ggplot(estim_CN, aes(estim,  as.factor(method)))+
  xlab(paste("Current Smoker vs Never Smoked Difference in ", outcome, sep='')) +
  geom_point() + 
  geom_segment(limits_low) +
  facet_wrap(~time, ncol = 1, labeller = function(labels){label_both(labels, sep = " = ")}) +
  ylab("Method") +
  theme_bw()
p_estimates2

p_estimates3 <- p_estimates2 + geom_vline(xintercept= 0, linetype="dashed", color="#1f78b4", size=1.0)
p_estimates3

ggsave("Estimate_Overall_Difference_Current_Never.pdf", width=5.5, height=5.5)

dev.off()


#################################################

### Comparison of Smoking estimates Between Past and Never Smoked ---- 

load("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/RStudio/Output/estim_past_never_overall.RData")

xlim <- c(min(estim_PN$estim-estim_PN$se),max(estim_PN$estim+estim_PN$se))

for(t in times_pred){
  
  p <- ggplot(estim_PN[estim_PN$time==t,], aes(estim,  as.factor(method)))+
    ylab("Estimates") + xlab(paste("t =",t,sep=""))
  
  limits_low <- aes(y = as.factor(method), yend = as.factor(method), x = estim - 1.96*se, xend = estim + 1.96*se)
  
  p <- p + geom_point() + 
    geom_segment(data=estim_PN[estim_PN$time==t,], limits_low, width=0.2)+
    xlim(xlim)
  
  if(t==times_pred[1]){
    p1 <- p
    p2 <- ggplot(estim_PN[estim_PN$time==t,], aes(estim,  as.factor(method)))
    p3 <- p2 
    p4 <- p2
  }else if(t==times_pred[2]){
    p2 <- p
  }else if(t==times_pred[3]){
    p3 <- p
  }else{
    p4 <- p
  }
}

grid.arrange(p1, p2, p3, p4, ncol = 1)
dev.off()

####Final Plot for Estimates_Difference_Current_Never

p_estimates <- grid.arrange(p1, p2, p3, p4, ncol = 1)
p_estimates 

p_estimates2 <- ggplot(estim_PN, aes(estim,  as.factor(method)))+
  xlab(paste("Past Smoker vs Never Smoked Difference in ", outcome, sep='')) +
  geom_point() + 
  geom_segment(limits_low) +
  facet_wrap(~time, ncol = 1, labeller = function(labels){label_both(labels, sep = " = ")}) +
  ylab("Method") +
  theme_bw()
p_estimates2

p_estimates3 <- p_estimates2 + geom_vline(xintercept= 0, linetype="dashed", color="#1f78b4", size=1.0)
p_estimates3

ggsave("Estimate_Overall_Difference_Past_Never.pdf", width=5.5, height=5.5)

dev.off()

