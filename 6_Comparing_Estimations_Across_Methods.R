library(nlme)
library(JM)
library(lcmm)
library(weightQuant)
library(splines)


load("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/RStudio/Output/models_LMM_REGARDS_Overall.RData")
load("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/RStudio/Output/models_JM_REGARDS_Overall.RData")


# specify the name of the dataset for outputs: 
dataset <- "Thesis"
# specify the name of the outcome for outputs:  
outcome <- "Overall Composite"


### Comparison of Smoking estimates across levels ---- 
summary(dataJM$time)
maxtime <- max(dataJM$time)
#maxtime <- 0.5

times_pred  <- c(0, 4, 8, 12, 14)
#times_pred  <- c(0, 5, 15,20)/10
#times_pred<-times_pred[which(times_pred<=maxtime)]

###computation sd LME
n_param <- 26
#ind_se  <- sapply(1:n_param, function(x) sum(1:x)) 
LME_V                              <- matrix(0, length(DquadHLME$best), length(DquadHLME$best))
LME_V[upper.tri(LME_V, diag=TRUE)] <- DquadHLME$V
var_LMEp                            <- diag(LME_V)[c(6,13:14)] 
var_LMEc                            <- diag(LME_V)[c(7,15:16)]
cov_LMEp                            <- c(LME_V[6,13], LME_V[6,14], LME_V[13,14])
cov_LMEc                            <- c(LME_V[7,15], LME_V[7,16], LME_V[15,16])

###computation sd JM
JM_V <- solve(JM_D_level$Hessian)
var_JMp                             <- diag(JM_V)[c(6,13:14)] 
var_JMc                             <- diag(JM_V)[c(7,15:16)] 
cov_JMp                            <- c(JM_V[6,13], JM_V[6,14], JM_V[13,14])
cov_JMc                            <- c(JM_V[7,15], JM_V[7,16], JM_V[15,16])


##########################################

### Comparison of Smoking estimates Between Current and Never Smoked ---- 

diff_CN <- data.frame("time"=rep(times_pred,each=2), "estim"=rep(0,length(times_pred)*2),"se"=rep(0,length(times_pred)*2),
                      "method"=rep(c("LME","JM"),times=length(times_pred)))

diff_CN$method <- factor(diff_CN$method, levels = c("JM", "LME"))

for(t in times_pred){
  #Linear mixed model
  diff_CN$estim[diff_CN$time==t & diff_CN$method=='LME'] <-
    DquadHLME$best[c("smoke_current","time:smoke_current","I(time^2):smoke_current")]%*%c(1,t,t^2)

  diff_CN$se[diff_CN$time==t & diff_CN$method=='LME']    <-
    sqrt(var_LMEc[1] + var_LMEc[2]*t^2 + var_LMEc[3]*(t^2)^2 +
           2*(cov_LMEc[1]*t + cov_LMEc[2]*t^2 + cov_LMEc[3]*t^3))
  
  #joint model
  diff_CN$estim[diff_CN$time==t & diff_CN$method=='JM']  <-
    JM_D_level$coefficients$betas[c("smoke_current","time:smoke_current","I(time^2):smoke_current")]%*%c(1,t,t^2)
  
  diff_CN$se[diff_CN$time==t & diff_CN$method=='JM']     <-
    sqrt(var_JMc[1] + var_JMc[2]*t^2 + var_JMc[3]*(t^2)^2 +
           2*(cov_JMc[1]*t + cov_JMc[2]*t^2 + cov_JMc[3]*t^3))
}

estim_CN <- diff_CN[diff_CN$time%in%c(0,4,8,12,14)&diff_CN$method%in%c("LME","JM"),]
estim_CN$pval <- sapply(estim_CN$estim/estim_CN$se, function(x) ifelse(x<0,pnorm(x ,0,1  ,lower.tail = T)*2, pnorm(x ,0,1  ,lower.tail = F)*2 ))
estim_CN

save(estim_CN, file = "estim_current_never_overall.Rdata")

##########################################

### Comparison of Smoking estimates Between Past and Never Smoked ---- 
summary(dataJM$time)
maxtime <- max(dataJM$time)
#maxtime <- 0.5

times_pred  <- c(0, 4, 8, 12, 14)
#times_pred  <- c(0, 5, 15,20)/10
#times_pred<-times_pred[which(times_pred<=maxtime)]
diff_PN <- data.frame("time"=rep(times_pred,each=2), "estim"=rep(0,length(times_pred)*2),"se"=rep(0,length(times_pred)*2),
                      "method"=rep(c("LME","JM"),times=length(times_pred)))

diff_PN$method <- factor(diff_PN$method, levels = c("JM", "LME"))

for(t in times_pred){
  #Linear mixed model
  diff_PN$estim[diff_PN$time==t & diff_PN$method=='LME'] <-
    DquadHLME$best[c("smoke_past","time:smoke_past","I(time^2):smoke_past")]%*%c(1,t,t^2)
  
  diff_PN$se[diff_PN$time==t & diff_PN$method=='LME']    <-
    sqrt(var_LMEp[1] + var_LMEp[2]*t^2 + var_LMEp[3]*(t^2)^2 +
           2*(cov_LMEp[1]*t + cov_LMEp[2]*t^2 + cov_LMEp[3]*t^3))
  
  #Linear joint model
  diff_PN$estim[diff_PN$time==t & diff_PN$method=='JM']  <-
    JM_D_level$coefficients$betas[c("smoke_past","time:smoke_past","I(time^2):smoke_past")]%*%c(1,t,t^2)
  
  diff_PN$se[diff_PN$time==t & diff_PN$method=='JM']     <-
    sqrt(var_JMp[1] + var_JMp[2]*t^2 + var_JMp[3]*(t^2)^2 +
           2*(cov_JMp[1]*t + cov_JMp[2]*t^2 + cov_JMp[3]*t^3))
}

estim_PN <- diff_PN[diff_PN$time%in%c(0,4,8,12,14)&diff_PN$method%in%c("LME","JM"),]
estim_PN$pval <- sapply(estim_PN$estim/estim_PN$se, function(x) ifelse(x<0,pnorm(x ,0,1  ,lower.tail = T)*2, pnorm(x ,0,1  ,lower.tail = F)*2 ))
estim_PN

save(estim_PN, file = "estim_past_never_overall.Rdata")


