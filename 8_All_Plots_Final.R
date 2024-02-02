### Final plot ----
outcome <- "Overall Composite"
traj_plot <- data.frame("Pcurrent"=Pcurrent$pred[,1], "Pcurrent_inf"=Pcurrent$pred[,2],"Pcurrent_sup"=Pcurrent$pred[,3],
                        "times"=datanew$time,
                        "Pnever"=Pnever$pred[,1], "Pnever_inf"=Pnever$pred[,2],"Pnever_sup"=Pnever$pred[,3],
                        "JMPcurrent"=JMPcurrent, "JMPnever"=JMPnever)


traj_plot <- data.frame("Pfemale"=Pfemale$pred[,1], "Pfemale_inf"=Pfemale$pred[,2],"Pfemale_sup"=Pfemale$pred[,3],
                        "times"=datanew$time,
                        "Pmale"=Pmale$pred[,1], "Pmale_inf"=Pmale$pred[,2],"Pmale_sup"=Pmale$pred[,3],
                        "JMPfemale"=JMPfemale, "JMPmale"=JMPmale)

traj_plot_melted_current <- reshape2::melt(traj_plot[, c(4, grep("current", colnames(traj_plot)))], 
                                           id.vars=c("times"), variable.name = "method", value.name = "outcome")
traj_plot_melted_current$method <- forcats::fct_recode(traj_plot_melted_current$method,
                                                       LMM = "Pcurrent", JM = "JMPcurrent")

traj_plot_melted_never <- reshape2::melt(traj_plot[, c(2,4,6,8)], 
                                         id.vars=c("times"), variable.name = "method", value.name = "outcome")
traj_plot_melted_never$method <- forcats::fct_recode(traj_plot_melted_never$method,
                                                     LMM = "Pnever", JM = "JMPnever")

traj_plot_melted <- rbind.data.frame(cbind.data.frame(traj_plot_melted_current, smoke_status="current"), 
                                     cbind.data.frame(traj_plot_melted_never, smoke_status="never")
)

IC_index <- c(grep("_inf", traj_plot_melted$method), grep("_sup", traj_plot_melted$method))
traj_plot_melted_pointestimate <- traj_plot_melted[-IC_index, ]
traj_plot_melted_pointestimate$method <- factor(as.character(traj_plot_melted_pointestimate$method),
                                                levels = c("LMM", "JM"))

traj_plot_melted_IC <- traj_plot_melted[IC_index, ]
summary(traj_plot_melted_IC)
traj_plot_melted_IC$method <- as.factor(as.character(forcats::fct_recode(traj_plot_melted_IC$method,
                                                                         inf = "Pcurrent_inf", 
                                                                         inf = "Pnever_inf", 
                                                                         sup = "Pcurrent_sup", 
                                                                         sup = "Pnever_sup")
))

library(dplyr)
traj_plot_melted_IC <- full_join(
  traj_plot_melted_IC %>% 
    filter(method=="inf") %>% 
    rename(inf = outcome) %>% 
    select(-method),
  traj_plot_melted_IC %>% 
    filter(method=="sup") %>% 
    rename(sup = outcome) %>% 
    select(-method),
  by = c("times", "smoke_status")
)

library(ggplot2)
p_traj <- ggplot(traj_plot_melted_pointestimate, aes(x=times)) +
  geom_ribbon(data = traj_plot_melted_IC,
              aes(ymin = inf, ymax = sup, fill=smoke_status), alpha = 0.2) + 
  geom_line(aes(y=outcome, linetype=method, color=smoke_status), size = 1) +
  scale_color_manual(values = c("purple3", "green3")) + 
  scale_fill_manual(values = c("purple3", "green3")) +
  #scale_x_discrete() + 
  scale_linetype_manual(values = c("twodash", "dotted","solid","dashed")) + 
  theme_bw() +
  theme(plot.title = element_text(size=15, face = "bold", hjust = 0.5)) +
  ylab(paste(outcome, " in ", dataset, sep="")) + xlab("Time in decades") +
  xlim(0, 1.1) +
  #ggtitle(paste(outcome," in ",dataset, " by Smoking Status (for age0=65,EL=0,white=0)",sep='')) +
  NULL
p_traj








#print(plotObj,vp=viewport(layout.pos.row=1:6,layout.pos.col=2))

grid.arrange(p_traj, p_estimates, p_Johansen_DO, p_Johansen_DC, ncol = 2)


library(patchwork)
#p_estimates3 <- p1 +p2+ p3 + p4 + plot_layout(guides = "collect", ncol = 1)
(p_final <-  (p_traj + # theme(legend.position = "left") +
                p_estimates2) / (p_Johansen_DO + guides(color="none", fill="none") + p_Johansen_DC + guides(color="none", fill="none")) + 
    patchwork::plot_layout(heights = c(2,1)))

file <- paste("Plot ", outcome, " in ",dataset, ".pdf", sep="")
pdf(file, width=9, height = 7)
print(p_final)
dev.off()


write.csv(dataJM, "S:\\Regards\\analysis\\Stuckwisch\\THESIS\\R CODE\\dataJM_overall.csv")