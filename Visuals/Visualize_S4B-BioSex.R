#!/usr/bin/env Rscript
######################

invisible(lapply(c("caret","readxl","stringr","tidyr","plyr","dplyr","tidyverse","corrplot","lattice","stringi","parameters","lme4","lmerTest","ggplot2","effectsize","jsonlite"), require, character.only=TRUE))
ROOT<-"/Users/rjirsara/Box Sync/Research/proj24-RiskCalculator/Analysis"
DATA<-"/Users/rjirsara/Box Sync/Research/proj24-RiskCalculator/datasets"
TODAY<-format(Sys.time(), "%Y%m%d")

######
### Figure Accuracy Metrics of Males & Females
######

DF<-read.csv(paste0(DATA,"/Preproc4_Primary/n24838x38_demographics.csv"))
DF$DEMO_Male<-factor(DF$DEMO_Male)

theme_set(theme_bw(base_size = 16))
TRAJ<-ggplot() + 
    geom_smooth(data=DF, aes(x=DEMO_Age, y=TARGET_bpm_totalprob_r, group=DEMO_Male), color="#000000", span = 2, fullrange=TRUE, method="loess", se=FALSE, linewidth=8, alpha=0.35) + 
    geom_smooth(data=DF, aes(x=DEMO_Age, y=TARGET_bpm_totalprob_r, color=DEMO_Male, group=DEMO_Male), span = 2, method="loess", se=FALSE, linewidth=5) + 
    xlab("Age (Years)") + ylab("General Psychopathology Symptoms") + 
    scale_color_manual(values=c("#cfa2ef","#9dc8f0")) +
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color = "white", face="bold")) + 
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color="white", face="bold")) +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) + 
    theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
    theme(legend.position = "none")
ggsave(plot=TRAJ,filename=paste0(ROOT,"/Visualize/FS6A_Sex_",{TODAY},".png"),device="png",width=8.5,height=8.5,units='in')

LONG<-read.csv(paste0(DATA,"/Preproc5_Longitudinal/n9599x42_demographics.csv"))
LONG<-LONG[,c("src_subject_id","DEMO_Male","TARGET_bpm_totalprob_lm")]
DF<-DF[,c("src_subject_id","DEMO_Male","TARGET_bpm_totalprob_r")]
names(LONG)[3]<-"PFACT"; names(DF)[3]<-"PFACT"
DF$Results<-"Symptom Severity"
LONG$Results<-"Linear Rate of Change"
DF<-rbind(DF,LONG)
DF$Results=factor(DF$Results,levels=c("Symptom Severity","Linear Rate of Change"))
MEAN <- ddply(DF, c("Results", "DEMO_Male"), summarize, mean = mean(PFACT))
theme_set(theme_bw(base_size = 16))

HIST<-ggplot(DF, aes(x = PFACT, color = DEMO_Male, fill = DEMO_Male)) +
    geom_histogram(aes(y = after_stat(count)), bins = 35, position = "identity", alpha = 0.35) +
    geom_vline(data = MEAN, aes(xintercept = mean, color = DEMO_Male), linetype = "longdash", linewidth = 2.5) +
    theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) + 
    theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color="white", face="bold")) + 
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
    scale_color_manual(values=c("#cfa2ef","#9dc8f0")) +
    scale_fill_manual(values=c("#cfa2ef","#9dc8f0")) +
    xlab("Generalized Psychopathology Symptoms") + ylab("Frequency") +
    facet_wrap(~Results, ncol=1, scale="free") +
    theme(legend.position = "none")
ggsave(plot=HIST,filename=paste0(ROOT,"/Visualize/FS6B_Sex_",{TODAY},".png"),device="png",width=6.5,height=8.5,units='in')

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######