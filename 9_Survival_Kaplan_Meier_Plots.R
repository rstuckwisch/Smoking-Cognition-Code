###Load Potential Packages
library(tidyverse)
library(haven)
library(survival)
library(dplyr)
library(gtsummary)
library(dplyr)
library(ggplot2)
library(survminer)
library(ggfortify)


###Open Survival File
data <- read.csv("S:/Regards/analysis/Stuckwisch/THESIS/Code/Overall/RStudio/Output/dataJM_overall.csv", header = TRUE, sep = ",")
data0 <- data[data$time==0,]

smoke <- subset(data0, select = c(ID, smoke_past, smoke_current, indEvent, timeEvent) )

smoke2 <-
  mutate(smoke,smoke_status = case_when(smoke_current == 1 & smoke_past == 0 ~ 3,
                                 smoke_current == 0 & smoke_past == 1 ~ 2,
                                 smoke_current == 0 & smoke_past == 0 ~ 1))

table(smoke2$smoke_status)

smoke2$Death = ifelse(smoke2$indEvent == 2, 1, 0)
smoke2$Dropout = ifelse(smoke2$indEvent == 1, 1, 0)

deathfit <- survfit(Surv(timeEvent, Death)~smoke_status, data=smoke2)
plot(deathfit)

dropoutfit <- survfit(Surv(timeEvent, Dropout)~smoke_status, data=smoke2)
plot(dropoutfit)


# http://rpkgs.datanovia.com/survminer/reference/ggsurvplot_arguments.html
# pval = true provides the log rank test

death <- ggsurvplot(deathfit, conf.int=TRUE, pval = TRUE, risk.table = TRUE,
                          xlab = "Time (Years)",
                          legend.labs=c("Never", "Past", "Current"), legend.title="Smoking Status",
                          break.x.by = 2, xlim = c(0, 14),
                          palette = c("#1b9e77", "#d95f02", "#7570b3"))

dropout <- ggsurvplot(dropoutfit, conf.int=TRUE, pval = TRUE, risk.table = TRUE,
                          ylab = "1 - Retention Probability", xlab = "Time (Years)",
                          legend.labs=c("Never", "Past", "Current"), legend.title="Smoking Status",
                          break.x.by = 2, xlim = c(0, 14),
                          palette = c("#1b9e77", "#d95f02", "#7570b3"))
death
dropout













