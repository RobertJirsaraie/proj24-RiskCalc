#!/usr/bin/env Rscript
######################

invisible(lapply(c("caret","readxl","stringr","tidyr","plyr","dplyr","tidyverse","corrplot","lattice","stringi","parameters","lme4","lmerTest","ggplot2","effectsize","jsonlite"), require, character.only=TRUE))
ROOT<-"/Users/robertjirsaraie/Library/CloudStorage/Box-Box/Research/proj24-RiskCalculator"
COLORS<-fromJSON(paste0(ROOT,"/Analysis/Software/ColorSchemas.json"))
TODAY<-format(Sys.time(), "%Y%m%d")

######
### Accuracy Differences Between Each Timepoint
######

DF<-read.csv(paste0(ROOT,"/analysis/Preproc4_totalprob_r/df-multimodal_mod-XGB_predict.csv"))
DF[which(DF$eventname=="4_year_follow_up_y_arm_1"),"eventname"]<-"Year 4"
DF[which(DF$eventname=="2_year_follow_up_y_arm_1"),"eventname"]<-"Year 2"
DF[which(DF$eventname=="baseline_year_1_arm_1"),"eventname"]<-"Baseline"
F3A<-ggplot() + 
        geom_point(data=DF, aes(x=targets, y=predictions), color="#000000", size=1.50, alpha=0.35) +
        geom_point(data=DF, aes(x=targets, y=predictions, color=eventname), size=1, alpha=0.50) +
        geom_abline(intercept=0, slope=1, linewidth=3, color="#000000", linetype="dashed") + 
        geom_smooth(data=DF, aes(x=targets, y=predictions, group=eventname), color="#000000", fullrange=TRUE, method=lm, se=FALSE, linewidth=3.5) + 
        geom_smooth(data=DF, aes(x=targets, y=predictions, color=eventname, group=eventname), fullrange=TRUE, method=lm, se=FALSE, linewidth=2) + 
        theme(strip.background = element_rect(colour = "black", fill = "black")) +
        theme(strip.text = element_text(color="white", face="bold")) +
        theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
        theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) + 
        theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
        xlab("Current Psychopathology Symptoms") + ylab("Predicted Psychopathology Symptoms") + 
        xlim(floor(min(DF$targets)), ceiling(max(DF$targets))) + 
        ylim(floor(min(DF$targets)), ceiling(max(DF$targets))) + 
        scale_color_manual(values = c("#00e5fa", "#00abfa", "#0228a6")) +
        theme(legend.position = "none")
ggsave(plot=F3A,filename=paste0(ROOT,"/Visualize/FS3A_",{TODAY},".png"),device="png",width=8.5,height=8.5,units='in')

for (EVENT in unique(DF$eventname)){
    SUBSET<-DF[which(DF$eventname==EVENT),]
    print(EVENT)
    print(postResample(pred = SUBSET$predictions, obs = SUBSET$targets))
}

######
### Distributions of Prediction Errors
######

DF<-read.csv(paste0(ROOT,"/analysis/Preproc4_totalprob_r/df-multimodal_mod-XGB_predict.csv"))
DF$errors<-abs(DF$targets-DF$predictions)
DF[which(DF$eventname=="baseline_year_1_arm_1"),"eventname"]<-"Baseline"
DF[which(DF$eventname=="2_year_follow_up_y_arm_1"),"eventname"]<-"Year 2"
DF[which(DF$eventname=="4_year_follow_up_y_arm_1"),"eventname"]<-"Year 4"
DF$eventname <- as.factor(DF$eventname)
MEAN <- ddply(DF, "eventname", summarize, mean=mean(errors))
theme_set(theme_bw(base_size = 16))
F3B<-ggplot(DF, aes(x = errors, color = eventname, fill = eventname)) +
    geom_histogram(aes(y = after_stat(count)), bins = 35, position = "identity", alpha = 0.35) +
    geom_vline(data = MEAN, aes(xintercept = mean, color = eventname), linetype = "longdash", linewidth = 1.5) +
    theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) + 
    theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color="white", face="bold")) + 
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
    scale_color_manual(values = c("#00e5fa", "#00abfa", "#0228a6")) +
    scale_fill_manual(values = c("#00e5fa", "#00abfa", "#0228a6")) +
    xlab("Absolute Prediction Errors") + ylab("Frequency") +
    facet_wrap(~eventname,ncol=1) +
    theme(legend.position = "none")
ggsave(plot=F3B,filename=paste0(ROOT,"/Visualize/FS3B_",{TODAY},".png"),device="png",width=6.5,height=8.5,units='in')

stats_summary <- DF %>%
  group_by(eventname) %>%
  summarize(
    mean = mean(errors, na.rm = TRUE),
    sd = sd(errors, na.rm = TRUE),
    skewness = skewness(errors, na.rm = TRUE),
    median = median(errors, na.rm = TRUE)
)

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######