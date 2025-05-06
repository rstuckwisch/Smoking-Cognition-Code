##############################################
############## RStudio Code for ##############
########## The association between ###########
############# smoking status and #############
######### aging cognitive trajectory #########
###### accounting for bias in attrition ######
##### Overall Composite Cognitive Score ######
##############################################

#### Libraries required #####
## please install the packages if you don't have them yet
library(lcmm)
library(nlme)
library(JM)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(gt)
library(gtable)
library(gtsummary)
library(mstate)

load("~/data_overall.RData")

################################################################################
##### Calculating Linear Mixed Model
    # Outcome Measure: Overall Cognition

# library(lcmm)

HLME_overall <- hlme( varY ~ time + I(time^2) 
                      + PE_prior + age0C
                      + smoke_past + smoke_current + alc_past + alc_current 
                      + male + EL + white
                      + smoke_past*(time + I(time^2))
                      + smoke_current*(time + I(time^2))
                      + alc_past*(time + I(time^2))
                      + alc_current*(time + I(time^2))
                      + male*(time + I(time^2))
                      + EL*(time + I(time^2))
                      + white*(time + I(time^2)),
                      random=~ time + I(time^2), subject= "ID",
                      data = dataJM,
                      verbose = TRUE)
summary(HLME_overall)

################################################################################
##### Calculating Joint Models
    # Outcome Measure: Overall Cognition

# library(nlme)
# library(JM)

### JM package requires linear mixed model as an lme object
  ## calculate linear mixed model with lme (same results as above)
LME_quad <- lme( varY ~ time + I(time^2) 
                 + PE_prior + age0C
                 + smoke_past + smoke_current + alc_past + alc_current 
                 + male + EL + white
                 + smoke_past*(time + I(time^2))
                 + smoke_current*(time + I(time^2))
                 + alc_past*(time + I(time^2))
                 + alc_current*(time + I(time^2))
                 + male*(time + I(time^2))
                 + EL*(time + I(time^2))
                 + white*(time + I(time^2)),
                 random=~ time + I(time^2) | ID, 
                 data = dataJM,method='ML',
                 control=lmeControl(returnObject = TRUE,
                                    msVerbose = TRUE))
summary(LME_quad)

### Survival model with CoxPH
coxFit <- 
  coxph(formula = Surv(timeEvent, indSingleEvent) ~ smoke_past + smoke_current + alc_past + alc_current + male + age0C + EL + white,
        data = dataS, 
        x = TRUE)   # x : outputs the design matrix associated with covariates
summary(coxFit)

### Joint Model with current value dependence structure
JM_value <- 
  jointModel(lmeObject = LME_quad,
             survObject = coxFit,
             timeVar = "time",
             parameterization = "value",
             method = "spline-PH-aGH",
             verbose = TRUE,
             control = list(GHk = 3, iter.EM=50))

### Joint Model derivative form
  # (required for slope dependence structure)
dForm <- 
  list( fixed = ~ 1 + I(2*(time)) + 
          smoke_past + smoke_past:I(2*(time)) + 
          smoke_current + smoke_current:I(2*(time)) + 
          alc_past + alc_past:I(2*(time)) + 
          alc_current + alc_current:I(2*(time)) + 
          male + male:I(2*(time)) + 
          EL + EL:I(2*(time)) + 
          white + white:I(2*(time)),
        indFixed = c(2:3, 13:26),
        random = ~ 1 + I(2*time),
        indRandom = 2:3)

Binit <- JM_value$coefficients
Binit$Dalpha <- 0

### Joint Model with slope only dependence structure
JM_slope <-  jointModel(lmeObject = LME_quad,
                        survObject = coxFit,
                        timeVar = "time", # time variable in the mixed model
                        parameterization = "slope", # dependence structure 
                        method = "spline-PH-aGH",# baseline risk model
                        verbose = TRUE, # trace output
                        derivForm = dForm,
                        control = list(GHk = 3, iter.EM=50),init=Binit)

summary(JM_slope)

save(JM_value, JM_slope, # JM_both,
     file=paste("~/Library/CloudStorage/Box-Box/Cognition_Smoking_Munscript/Submissions/AJE Submission/Submission 3/Analysis_new/cog_models/overall/JM_overall.RData",sep=""))

### Joint Model with current value and slope dependence structure
JM_both <-  jointModel(lmeObject = LME_quad,
                       survObject = coxFit,
                       timeVar = "time", # time variable in the mixed model
                       parameterization = "both", # dependence structure 
                       method = "spline-PH-aGH",# baseline risk model
                       verbose = TRUE, # trace output
                       derivForm = dForm,
                       control = list(GHk = 3, iter.EM=100),init=Binit)

summary(JM_both)

## Save models
save(dataJM, coxFit, HLME_overall, LME_quad, JM_value, JM_slope, JM_both,
     file=paste("~/overall_models.RData",sep=""))

#### Compare Joint Models 
#H0: You should use the nested model
#Ha: You should use the complex model

## loglik_test: -2 * [loglikelihood(simple)-loglikelihood(complex)]
#Since the logLik() function gives more information than the numeric value, use the as.numeric() function to isolate the numeric value
loglik <- -2*(as.numeric(logLik(JM_val_quadPrior))-as.numeric(logLik(JM_slope_quadPrior)))

# df = 52-51
pchisq(loglik, df = 1, lower.tail = FALSE)

################################################################################
##### JM Model Estimates
  # Outcome Measure: Overall Cognition

## Longitudinal Process (LMM)
data_long <- data_summary[["CoefTable-Long"]]
names_long <- as.data.table(rownames(data_long))
values_long <- as.data.table(data_long)
estim_JM_long <- cbind(names_long, values_long)
setnames(estim_JM_long, "V1", "Coefficient")

write.csv(estim_JM_cov, "~/Param_JM_Long_Overall.csv", 
          row.names = FALSE)

## Event Process (Survival Model)
data_event <- data_summary[["CoefTable-Event"]]
names_event <- as.data.table(rownames(data_event))
values_event <- as.data.table(data_event)
estim_JM_event <- cbind(names_event, values_event)
setnames(estim_JM_event, "V1", "Coefficient")

write.csv(estim_JM_cov, "~/Param_JM_Event_Overall.csv", 
          row.names = FALSE)

## the variance-covariance matrix of the random effects
data_cov <- data_summary[["D"]]
names_cov <- as.data.table(rownames(data_cov))
values_cov <- as.data.table(data_cov)
estim_JM_cov <- cbind(names_cov, values_cov)
setnames(estim_JM_cov, "V1", "Coefficient")

write.csv(estim_JM_cov, "~/Param_JM_Cov_Overall.csv", 
          row.names = FALSE)


################################################################################
##### Cognitive Composite Predictions
    # Outcome Measure: Overall Cognition

## load data
load("~/overall_models.RData")

#### dataset for new data
datanew <- data.frame(time=seq(0,14,by=2),age0=65,EL=0,male=0,PE_prior=0,white=0,smoke_past=0,smoke_current=0,alc_past=0,alc_current=0)
datanew$age0C <- (datanew$age0 - 65)

##########################
## Never Smoker
datanew$smoke_past=0
datanew$smoke_current=0
overall_never <- predictY(HLME_overall, newdata = datanew, draws=TRUE)
lmm_overall_never <- as.data.frame(overall_never[["pred"]])

## Past Smoker
datanew$smoke_past=1
datanew$smoke_current=0
overall_past <- predictY(HLME_overall, newdata = datanew, draws=TRUE)
lmm_overall_past <- as.data.frame(overall_past[["pred"]])

## Current Smoker
datanew$smoke_past=0
datanew$smoke_current=1
overall_current <- predictY(HLME_overall, newdata = datanew, draws=TRUE)
lmm_overall_current <- as.data.frame(overall_current[["pred"]])

lmm_overall_never$time <- seq(0,14,by=2)
lmm_overall_never$smoke <- "never"
lmm_overall_past$time <- seq(0,14,by=2)
lmm_overall_past$smoke <- "past"
lmm_overall_current$time <- seq(0,14,by=2)
lmm_overall_current$smoke <- "current"

merge1 <- rbind(lmm_overall_never, lmm_overall_past)
merge2 <- rbind(merge1, lmm_overall_current)
pred_HLME_overall65 <- merge2
setnames(pred_HLME_overall80, 
         old = c("Ypred", "lower.Ypred", "upper.Ypred"),
         new = c("pred", "low", "upp"))
pred_HLME_overall65$model <- "LLM"
pred_HLME_overall65$age0 <- 65

##########################
#### Predictions JM

datanew$smoke_past=0
datanew$smoke_current=0
JM_overall_never <- predict(JM_both,newdata=datanew, se.fit=TRUE, interval='confidence') |> as.data.frame()

datanew$smoke_past=1
datanew$smoke_current=0
JM_overall_past <- predict(JM_both,newdata=datanew, se.fit=TRUE, interval='confidence') |> as.data.frame()

datanew$smoke_past=0
datanew$smoke_current=1
JM_overall_current <- predict(JM_both,newdata=datanew, se.fit=TRUE, interval='confidence') |> as.data.frame()

JM_overall_never$time <- seq(0,14,by=2)
JM_overall_never$smoke <- "never"
JM_overall_past$time <- seq(0,14,by=2)
JM_overall_past$smoke <- "past"
JM_overall_current$time <- seq(0,14,by=2)
JM_overall_current$smoke <- "current"

merge1 <- rbind(JM_overall_never, JM_overall_past)
merge2 <- rbind(merge1, JM_overall_current)
pred_JM_overall65 <- subset(merge2, select = -se.fit)
pred_JM_overall65$model <- "JM"
pred_JM_overall65$age0 <- 65

## Save Predictions
save(pred_HLME_overall65, pred_JM_overall65,
     file=paste("~/overall_pred65.RData",sep=""))

################################################################################
##### General Plot for Cognitive Composite Predictions
  ### by Smoking Status and Modeling Method
    # Outcome Measure: Overall Cognition

data_curr <- rbind(pred_HLME_overall65, pred_JM_overall65)

plot0 <- ggplot(data_curr, 
                aes(x=time, y=pred, colour = smoke, linetype=model)) +
  geom_line(linewidth = .75) +
  labs(x = "Time (Years in Study)",
       y = "Overall Composite Cognitive Score") +
  scale_colour_manual(limits = c("never", "past", "current"),
                      values = c("#1b9e77","#d95f02","#7570b3"),
                      labels = c("Never", "Past","Current")) +
  scale_linetype_manual(limits = c("LLM", "JM"),
                        values = c("solid", "dashed"),
                        labels = c("LMM", "JM")) +
  geom_ribbon(aes(ymin=low, ymax=upp, fill = smoke), alpha = 0.1, colour = NA)+
  scale_fill_manual(limits = c("never", "past", "current"),
                    values = c("#1b9e77","#d95f02","#7570b3"),
                    guide = "none")

plot1 <- plot0 +
  scale_x_continuous(limits = c(0, 14),
                     breaks = c(0,2,4,6,8,10,12,14)) +
  scale_y_continuous(limits = c(-1.3,0.0),
                     breaks = c(-1.2, -0.8, -0.4, 0.0))

plot2 <- plot1 +
  theme(axis.line = element_line(colour = "black",
                                 linewidth = 1),
        axis.ticks = element_line(colour = "black",
                                  linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(
    linetype = guide_legend(
      title = "Model",
      override.aes = list(fill = c(NA, NA)),
      position = 'inside'),
    colour = guide_legend(
      title = "Smoking Status",
      position = 'top'
    )) 

plot3 <- plot2 +
  theme(
    legend.direction = 'horizontal',
    legend.position.inside = c(0.25,0.1),
    text=element_text(family='serif'))

load
pdf("~/Figure3.pdf", 
    height=5, 
    width=5,
    family = "Times")
print(plot3); 
dev.off()

################################################################################
##### Calculating Estimated Differences in 
##### Cognitive Composite Predictions
  ### By Smoking Status and Across Modeling Methods
    # Outcome Measure: Overall Cognition

## load data
load("~/overall_models.RData")

###computation sd HLME (RE = quadratic time)
quadHLME_V                                    <- matrix(0, length(HLME_overall$best), length(HLME_overall$best))
quadHLME_V[upper.tri(quadHLME_V, diag=TRUE)]  <- HLME_overall$V
var_past_qHLME                                <- diag(quadHLME_V)[c(6,13:14)] 
var_current_qHLME                             <- diag(quadHLME_V)[c(7,15:16)]
cov_past_qHLME                                <- c(quadHLME_V[6,13], quadHLME_V[6,14], quadHLME_V[13,14])
cov_current_qHLME                             <- c(quadHLME_V[7,15], quadHLME_V[7,16], quadHLME_V[15,16])

###computation sd JM  (RE = quadratic time)
quadJM_V                                      <- solve(JM_both$Hessian)
var_past_qJM                                  <- diag(quadJM_V)[c(6,13:14)] 
var_current_qJM                               <- diag(quadJM_V)[c(7,15:16)] 
cov_past_qJM                                  <- c(quadJM_V[6,13], quadJM_V[6,14], quadJM_V[13,14])
cov_current_qJM                               <- c(quadJM_V[7,15], quadJM_V[7,16], quadJM_V[15,16])

#######################################
### Comparison of Smoking Estimates 
### Between Current and Never Smokers

times_pred  <- c(0, 2, 4, 6, 8, 10, 12, 14)
diff_current <- data.frame("time"=rep(times_pred,each=2), "estim"=rep(0,length(times_pred)*2),"se"=rep(0,length(times_pred)*2),
                           "method"=rep(c("LMM","JM"),times=length(times_pred)))
diff_current$method <- factor(diff_current$method, levels = c("LMM", "JM"))

for(t in times_pred){
## Linear Mixed Model
  diff_current$estim[diff_current$time==t & diff_current$method=='LMM'] <-
    HLME_overall$best[c("smoke_current","time:smoke_current","I(time^2):smoke_current")]%*%c(1,t,t^2)
  
  diff_current$se[diff_current$time==t & diff_current$method=='LMM']    <-
    sqrt(var_current_qHLME[1] + var_current_qHLME[2]*t^2 + var_current_qHLME[3]*(t^2)^2 +
           2*(cov_current_qHLME[1]*t + cov_current_qHLME[2]*t^2 + cov_current_qHLME[3]*t^3))
## Joint Model
  diff_current$estim[diff_current$time==t & diff_current$method=='JM']  <-
    JM_both$coefficients$betas[c("smoke_current","time:smoke_current","I(time^2):smoke_current")]%*%c(1,t,t^2)
  
  diff_current$se[diff_current$time==t & diff_current$method=='JM']     <-
    sqrt(var_current_qJM[1] + var_current_qJM[2]*t^2 + var_current_qJM[3]*(t^2)^2 +
           2*(cov_current_qJM[1]*t + cov_current_qJM[2]*t^2 + cov_current_qJM[3]*t^3))
}

estim_current <- diff_current[diff_current$time%in%c(0,2,4,6,8,10,12,14)&diff_current$method%in%c("LMM","JM"),]
estim_current$pval <- sapply(estim_current$estim/estim_current$se, function(x) ifelse(x<0,pnorm(x ,0,1  ,lower.tail = T)*2, pnorm(x ,0,1  ,lower.tail = F)*2 ))
estim_current$lower <- estim_current$estim - 1.96*estim_current$se
estim_current$upper <- estim_current$estim + 1.96*estim_current$se
estim_current$diff <- "CN"

wider_current <- estim_current |> 
  pivot_wider(names_from = diff, values_from = c(estim, lower, upper))
wider_current <-  subset(wider_current, select = -c(se, pval))

save(estim_current, wider_current,
     file = "~/estim_overall_curr_nev.Rdata")

#######################################
### Comparison of Smoking Estimates 
### Between Past and Never Smokers

times_pred  <- c(0, 2, 4, 6, 8, 10, 12, 14)
diff_past <- data.frame("time"=rep(times_pred,each=2), "estim"=rep(0,length(times_pred)*2),"se"=rep(0,length(times_pred)*2),
                        "method"=rep(c("LMM","JM"),times=length(times_pred)))
diff_past$method <- factor(diff_past$method, levels = c("LMM", "JM"))

for(t in times_pred){
## Linear Mixed Model
  diff_past$estim[diff_past$time==t & diff_past$method=='LMM'] <-
    HLME_overall$best[c("smoke_past","time:smoke_past","I(time^2):smoke_past")]%*%c(1,t,t^2)
  
  diff_past$se[diff_past$time==t & diff_past$method=='LMM']    <-
    sqrt(var_past_qHLME[1] + var_past_qHLME[2]*t^2 + var_past_qHLME[3]*(t^2)^2 +
           2*(cov_past_qHLME[1]*t + cov_past_qHLME[2]*t^2 + cov_past_qHLME[3]*t^3))
## Joint Model
  diff_past$estim[diff_past$time==t & diff_past$method=='JM']  <-
    JM_both$coefficients$betas[c("smoke_past","time:smoke_past","I(time^2):smoke_past")]%*%c(1,t,t^2)
  
  diff_past$se[diff_past$time==t & diff_past$method=='JM']     <-
    sqrt(var_past_qJM[1] + var_past_qJM[2]*t^2 + var_past_qJM[3]*(t^2)^2 +
           2*(cov_past_qJM[1]*t + cov_past_qJM[2]*t^2 + cov_past_qJM[3]*t^3))
}

estim_past <- diff_past[diff_past$time%in%c(0,2,4,6,8,10,12,14)&diff_past$method%in%c("LMM","JM"),]
estim_past$pval <- sapply(estim_past$estim/estim_past$se, function(x) ifelse(x<0,pnorm(x ,0,1  ,lower.tail = T)*2, pnorm(x ,0,1  ,lower.tail = F)*2 ))
estim_past$lower <- estim_past$estim - 1.96*estim_past$se
estim_past$upper <- estim_past$estim + 1.96*estim_past$se
estim_past$diff <- "PN"

wider_past <- estim_past |> 
  pivot_wider(names_from = diff, values_from = c(estim, lower, upper))
wider_past <-  subset(wider_past, select = -c(se, pval))

save(estim_past, wider_past,
     file = "~/estim_overall_past_nev.Rdata")

################################################################################
##### Comparing Cognitive Composite Estimations
  ### Across Modeling Methods
    # Outcome Measure: Overall Cognition
    # Table Summary
# library(tidyverse)
# library(gt)
# library(gtable)
# library(gtsummary)
## load data
load("~/estim_overall_curr_nev.Rdata")
load("~/estim_overall_past_nev.Rdata")

#######################################
table <- merge(wider_current, wider_past, 
               by= c("time", "method")) |> 
  arrange(time, desc(method))

dataM <- table |> 
  mutate(time=recode(time, 
                     "0" = "Baseline", 
                     "2" = "2-Years", 
                     "4" = "4-Years", 
                     "6" = "6-Years", 
                     "8" = "8-Years", 
                     "10" = "10-Years", 
                     "12" = "12-Years", 
                     "14" = "14-Years")) |>
  mutate(method = recode(method, 
                         "JM" = "Joint Model", 
                         "LME" = "Linear Mixed Model"))

table1 <- dataM |>
  gt(rowname_col = "method", 
     groupname_col = "time") |>
  fmt_number(
    columns = c(estim_CN, lower_CN, upper_CN, estim_PN, lower_PN, upper_PN), 
    decimals = 3) |>
  ## Merging Columns to create one column: 
  ## Estim (LCL, UCL)
  cols_merge(
    columns = c("estim_CN","lower_CN","upper_CN"),
    pattern = "{1} ({2},{3})") |>
  cols_merge(
    columns = c("estim_PN","lower_PN","upper_PN"),
    pattern = "{1} ({2},{3})") |>
  ## Renaming Columns 
  ## estim_CN = "Current vs. Never"
  ## estim_PN = "Past vs. Never"
  cols_label(
    estim_CN = "Current vs. Never",
    estim_PN = "Past vs. Never") 

table2 <- table1 |>
  ## Centering Columns
  cols_align(align = "center",
             columns = c(estim_CN, estim_PN)) |>
  ## Adding Header for Time Column
  tab_stubhead(label = "Time in Study") |>
  ## Aligning Methods to the left of Column
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stub()) |>
  ## Indenting Methods for Each Age for easier reading
  tab_stub_indent(
    rows = everything(),
    indent = 5) |>
  ## Adding Header for Estimated Differences Columns
  tab_spanner(
    label = "Estimated Difference",
    columns = c(estim_CN, estim_PN))  |>
  ## Adding Footnote
  tab_footnote(
    footnote = "Estimated Difference (95% Confidence Interval)",
    locations = cells_column_spanners(),
    placement = "left")

gtsave(table2, "Table_Overall_Estim_Diff.docx", 
       path = " ")

################################################################################
##### Comparing Cognitive Composite Estimations
  ### Across Modeling Methods
    # Outcome Measure: Overall Cognition
    # Figure Summary 

# library(ggplot2)
# library(cowplot)
## load data
load("~/estim_overall_curr_nev.Rdata")
load("~/estim_overall_past_nev.Rdata")

#######################################
### Comparison of Smoking Estimates 
### Between Current and Never Smokers
estim_current <- estim_current[-c(3,4,7,8,11,12),]
#### Final Plot for Estimates_Difference_Current_Never (Figure 5A)
current <- ggplot(estim_current, aes(estim,  as.factor(method))) + 
  xlim(-.4,0.02) +
  geom_point() + 
  geom_segment(aes(y = as.factor(method), yend = as.factor(method), 
                   x = estim - 1.96*se, xend = estim + 1.96*se)) +
  facet_wrap(~time, ncol = 1, labeller = function(labels){label_both(labels, sep = " = ")}) +
  xlab("Current Smokers and Never Smokers\nDifference in Overall\nCognitive Composite Scores") +
  ylab("Method") +  
  geom_vline(xintercept= 0, 
             linetype="dashed", 
             color="#1f78b4", linewidth=1.0) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    text=element_text(family='serif'))
current

#######################################
### Comparison of Smoking Estimates 
### Between Past and Never Smokers
estim_past <- estim_past[-c(3,4,7,8,11,12),]
#### Final Plot for Estimates_Difference_Past_Never (Figure 5B)
past <- ggplot(estim_past, aes(estim,  as.factor(method))) + 
  xlim(-.4,0.02) +
  geom_point() + 
  geom_segment(aes(y = as.factor(method), yend = as.factor(method), 
                   x = estim - 1.96*se, xend = estim + 1.96*se)) +
  facet_wrap(~time, ncol = 1, labeller = function(labels){label_both(labels, sep = " = ")}) +
  xlab("Past Smokers and Never Smokers\nDifference in Overall\nCognitive Composite Scores") +
  ylab("Method") +  
  geom_vline(xintercept= 0, 
             linetype="dashed", 
             color="#1f78b4", linewidth=1.0) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    text=element_text(family='serif'))
past

#######################################
### Comparison of Smoking Estimates 
### Between Past and Never Smokers
  # Multipanel Plot

plot <- plot_grid(current,
                  past,
                  ncol=2, 
                  align = "vh", 
                  rel_widths = c(1,1),
                  labels = "AUTO", 
                  label_size = 12)
plot
### Save Plot (Figure 5)
load
pdf("~/Figure5.pdf", 
    height=4.5, 
    width=6.5,
    family = "Times")  #Need to specify non-Helvetica fonts for the rendering of a PDF specifically
print(plot); 
dev.off()

################################################################################
################################################################################
##### Cumulative Transition Intensities for Dropout and Death
  ### (Accounting for Competing Risks)
    # Outcome Measure: Overall Cognition
    # Aalen-Johansen Survival Figures 

# library(mstate)
## load data
load("~/data_overall.RData")

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

prN <-  subset(dataAJ, smoke_past==0 & smoke_current==0)
prP <-  subset(dataAJ, smoke_past==1 & smoke_current==0)
prC <-  subset(dataAJ, smoke_past==0 & smoke_current==1)
attr(prN, "trans") <- trans
attr(prP, "trans") <- trans
attr(prC, "trans") <- trans

c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), data=prN)
c1 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), data=prP)
c2 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), data=prC)

#estimation of transition hazards and associated (co)variances
msf0 <- msfit(c0, trans=trans)
msf1 <- msfit(c1, trans=trans)
msf2 <- msfit(c2, trans=trans)

# Estimated cumulative hazard values at all event times 
par(mfrow=c(1,2))
plot(msf0$Haz$Haz[msf0$Haz$trans==1]~msf0$Haz$time[msf0$Haz$trans==1],type='l',
     ylab="Cumulative Hazard - Dropout", xlab="Time", ylim=c(min(msf0$Haz$Haz), max(msf0$Haz$Haz)))
lines(msf1$Haz$Haz[msf1$Haz$trans==1]~msf1$Haz$time[msf1$Haz$trans==1],lty=2)
lines(msf2$Haz$Haz[msf2$Haz$trans==1]~msf2$Haz$time[msf2$Haz$trans==1],lty=3)
legend("topleft",c("Never","Past","Current"), lty=c(1,2,3))

plot(msf0$Haz$Haz[msf0$Haz$trans==2]~msf0$Haz$time[msf0$Haz$trans==2],type='l',
     ylab="Cumulative Hazard - Death", xlab="Time", ylim=c(min(msf0$Haz$Haz), max(msf0$Haz$Haz)))
lines(msf1$Haz$Haz[msf1$Haz$trans==2]~msf1$Haz$time[msf1$Haz$trans==2],lty=2)
lines(msf2$Haz$Haz[msf2$Haz$trans==2]~msf2$Haz$time[msf2$Haz$trans==2],lty=3)
legend("topleft",c("Never","Past","Current"), lty=c(1,2,3))

pt0 <- probtrans(msf0, predt=0)[[1]]
pt1 <- probtrans(msf1, predt=0)[[1]]
pt2 <- probtrans(msf2, predt=0)[[1]]

##########################
## Format Death Dataset
data_Johansen1 <- data.frame("pstate"=c(pt0$pstate2,
                                        pt1$pstate2,
                                        pt2$pstate2),
                             "pstate_up" = c(pt0$pstate2, pt1$pstate2, pt2$pstate2) + 1.96*c(pt0$se2, pt1$se2, pt2$se2),
                             "pstate_low" = c(pt0$pstate2, pt1$pstate2, pt2$pstate2) - 1.96*c(pt0$se2, pt1$se2, pt2$se2), 
                             "times" = c(pt0$time,
                                         pt1$time,
                                         pt2$time),
                             "status"=c(rep("Never",length(pt0$time)),
                                        rep("Past",length(pt1$time)),
                                        rep("Current",length(pt2$time))))
## Format Drop out Dataset
data_Johansen2 <- data.frame("pstate"=c(pt0$pstate3,
                                        pt1$pstate3,
                                        pt2$pstate3),
                             "pstate_up" = c(pt0$pstate3, pt1$pstate3, pt2$pstate3) + 1.96*c(pt0$se3, pt1$se3, pt2$se3),
                             "pstate_low" = c(pt0$pstate3, pt1$pstate3, pt2$pstate3) - 1.96*c(pt0$se3, pt1$se3, pt2$se3), 
                             "times" = c(pt0$time,
                                         pt1$time,
                                         pt2$time),
                             "status"=c(rep("Never",length(pt0$time)),
                                        rep("Past",length(pt1$time)),
                                        rep("Current",length(pt2$time))))
##########################
## Create Aalen-Johansen Survival Figures
# library(cowplot)
# library(ggplot2)

# Death probability plot
p_Johansen1 <- ggplot(data_Johansen1, aes(x=times)) +
  geom_ribbon(aes(ymin = pstate_low, ymax = pstate_up, fill=status), alpha=0.3) +
  geom_line(aes(y=pstate, color=status), size = 1) +
  ylim(0,1) +
  scale_color_manual(aesthetics = c("colour","fill"),
                     limits = c("Never", "Past", "Current"),
                     values = c("#1b9e77","#d95f02","#7570b3"),
                     labels = c("Never", "Past","Current"),
                     guide = guide_legend(
                       title = "Smoking Status")) +
  theme_bw() +
  theme(plot.title = 
          element_text(size=15,
                       face = "bold", hjust = 0.5),
        text=element_text(family='serif'),
        legend.position = "top",
        axis.line = element_line(colour = "black",
                                 linewidth = 1),
        axis.ticks = element_line(colour = "black",
                                  linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("Dropout probability") + xlab("Time in Years")

# Dropout probability plot
p_Johansen2 <- ggplot(data_Johansen2, aes(x=times)) +
  geom_ribbon(aes(ymin = pstate_low, ymax = pstate_up, fill=status), alpha=0.3) +
  geom_line(aes(y=pstate, color=status), size = 1) +
  ylim(0,1) +
  scale_color_manual(aesthetics = c("colour","fill"),
                     limits = c("Never", "Past", "Current"),
                     values = c("#1b9e77","#d95f02","#7570b3"),
                     labels = c("Never", "Past","Current"),
                     guide = guide_legend(
                       title = "Smoking Status")) + 
  theme_bw() +
  theme(plot.title = 
          element_text(size=15,
                       face = "bold", hjust = 0.5),
        text=element_text(family='serif'),
        legend.position = "top",
        axis.line = element_line(colour = "black",
                                 linewidth = 1),
        axis.ticks = element_line(colour = "black",
                                  linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("Death probability") + xlab("Time in decades") 
p_Johansen2

# Combine Death and Dropout Figures
plots_AJ <- plot_grid(p_Johansen2 + theme(legend.position="none"), 
                      p_Johansen1 + theme(legend.position="none"),  
                      ncol=2, align = "vh", rel_widths = c(1,1),
                      labels = "AUTO", label_size = 12)
plots_AJ

# Save Legend seperately
legend <- cowplot::get_plot_component(p_Johansen1, 'guide-box-top', return_all = TRUE)
cowplot::ggdraw(legend)
legend

# then combine with the top row (legend) for final plot
plots_AJ2 <- plot_grid(legend, plots_AJ, ncol = 1, rel_heights = c(.1, 1))
plots_AJ2

# Save Figure 
load
pdf("~/Figure2.pdf", 
    height=2.5, 
    width=6.5,
    family = "Times")  #Need to specify non-Helvetica fonts for the rendering of a PDF specifically
print(plots_AJ2); 
dev.off()

