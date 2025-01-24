#!/usr/bin/env Rscript
######################

invisible(lapply(c("e1071","caret","readxl","stringr","tidyr","plyr","dplyr","tidyverse","corrplot","lattice","stringi","parameters","lme4","lmerTest","ggplot2","effectsize","jsonlite"), require, character.only=TRUE))
ROOT<-"/Users/robertjirsaraie/Library/CloudStorage/Box-Box/Research/proj24-RiskCalculator"
COLORS<-fromJSON(paste0(ROOT,"/Analysis/Software/ColorSchemas.json"))
TODAY<-format(Sys.time(), "%Y%m%d")
theme_set(theme_bw(base_size = 16))

######
### Distributions of Psychopathology Severity By Timepoint
######

DF<-read.csv(paste0(ROOT,"/datasets/Preproc4_Primary/n24838x969_multimodal.csv"))
MEAN <- ddply(DF, "eventname", summarize, mean=mean(TARGET_bpm_totalprob_r))
DF[which(DF$eventname=="baseline_year_1_arm_1"),"eventname"]<-"Baseline"
DF[which(DF$eventname=="2_year_follow_up_y_arm_1"),"eventname"]<-"Year 2"
DF[which(DF$eventname=="4_year_follow_up_y_arm_1"),"eventname"]<-"Year 4"
MEAN <- ddply(DF, "eventname", summarize, mean=mean(TARGET_bpm_totalprob_r))
F2A<-ggplot(DF, aes(x = TARGET_bpm_totalprob_r)) +
  geom_histogram(
    aes(y = after_stat(count), fill = after_stat(x), color = after_stat(x)), 
    bins = 35, position = "identity") +
  geom_vline(data = MEAN, aes(xintercept = mean), linetype = "longdash", linewidth = 1.5, color = "#000000") +
  scale_fill_gradientn(
    colors = c("#006400", "#00b015", "#ffaa00", "#fc0000", "#8b0000"),
    values = scales::rescale(c(0, 7, 14, 21, 37)),
    limits = c(0, 37),
    oob = scales::squish) +
  scale_color_gradientn(
    colors = c("#006400", "#00b015", "#ffaa00", "#fc0000", "#8b0000"),
    values = scales::rescale(c(0, 7, 14, 21, 37)),
    limits = c(0, 37),
    oob = scales::squish) +
  xlab("General Psychopathology Symptoms") + ylab("Frequency") +
  facet_wrap(~eventname, ncol = 1) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    strip.background = element_rect(colour = "black", fill = "black"),
    strip.text = element_text(color = "white", face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5),
    legend.position = "none"
  )
ggsave(plot=F2A,filename=paste0(ROOT,"/Visualize/Supplimentary/F2A_",{TODAY},".png"), device="png", width=6.5, height=8.5, units='in')

MEAN <- ddply(DF, "eventname", summarize, 
    mean=mean(TARGET_bpm_totalprob_r, na.rm = TRUE),
    dev=sd(TARGET_bpm_totalprob_r, na.rm = TRUE),
    skewness=skewness(TARGET_bpm_totalprob_r, na.rm = TRUE)
)

######
### Lineplot of Study Enrollment
######

DF<-read.csv(paste0(ROOT,"/datasets/Preproc4_Primary/n24838x969_multimodal.csv"))
DF<-DF[order(DF$DEMO_Age),]; DF$SubjOrder<- 0
for (x in 1:length(DF$src_subject_id)){
    SUBID<-unique(DF$src_subject_id)[x]
    DF[which(DF$src_subject_id==SUBID),dim(DF)[2]]<-x
}; DF<-DF[order(DF$SubjOrder),]; DF$SubjOrder<-factor(DF$SubjOrder)
LONG<-read.csv(paste0(ROOT,"/datasets/Preproc5_Longitudinal/n9599x973_multimodal.csv"))
LONG<-LONG[,c("src_subject_id","TARGET_bpm_totalprob_lm")]
DF<-merge(DF,LONG,by=c("src_subject_id"),all=TRUE)
DF<-DF[!is.na(DF$TARGET_bpm_totalprob_lm),] #Need to Exclude Single Timepoints
F2B<-ggplot() + 
    geom_line(DF, 
        mapping = aes(x = DEMO_Age, y = SubjOrder, group = src_subject_id), 
        size = 0.1, alpha = 0.6, color="gray") +
    geom_point(DF, 
        mapping = aes(x = DEMO_Age, y = SubjOrder, color = TARGET_bpm_totalprob_r), 
        size = 0.85) +
    scale_color_gradientn(
        colors = c("#006400", "#00b015", "#ffaa00", "#fc0000", "#8b0000"),
        values = scales::rescale(c(0, 7, 14, 21, 37)), 
        limits = c(0, 37), 
        oob = scales::squish 
        ) +
    xlab("Age") + ylab("Participants Ordered by Age During Study Enrollment") +
    theme(
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour = "black", fill = "black"),
        strip.text = element_text(color = "white", face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5),
        legend.position = "none"
    )
ggsave(plot=F2B,filename=paste0(ROOT,"/Visualize/Supplimentary/F2B_",{TODAY},".png"), device="png", width=8.5, height=8.5, units='in')

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######