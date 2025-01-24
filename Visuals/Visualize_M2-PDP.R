#!/usr/bin/env Rscript
######################

invisible(lapply(c("caret","readxl","stringr","tidyr","plyr","dplyr","tidyverse","corrplot","lattice","stringi","parameters","lme4","lmerTest","ggplot2","effectsize","jsonlite"), require, character.only=TRUE))
ROOT<-"/Users/rjirsara/Box Sync/Research/proj24-RiskCalculator"
COLORS<-fromJSON(paste0(ROOT,"/Software/ColorSchemas.json"))
TODAY<-format(Sys.time(), "%Y%m%d")

######
### Load Results
######

for (FILE in list.files(paste0(ROOT,"/algorithms"), pattern="partdependency", recursive=TRUE, full.names=TRUE)){
    CONTENT<-read.csv(FILE); CONTENT<-CONTENT[!is.na(CONTENT$GroupKFold),]
    if (!exists("DF")){
        DF<-CONTENT
    } else {
        DF<-rbind(DF,CONTENT)
    }
}

DF$Facet<-paste0(DF$Results,DF$Rank)
DF$Facet<-factor(
    DF$Facet,
    levels=c(
        "r1","lm1","future1",
        "r2","lm2","future2",
        "r3","lm3","future3"
    )
)

#ReLabel Ranking 
DF[which(DF$Rank==1),"Rank"]<-"1st"
DF[which(DF$Rank==2),"Rank"]<-"2nd"
DF[which(DF$Rank==3),"Rank"]<-"3rd"

#Relabels for Results
DF[which(DF$Results=="r"),"Results"]<-"Current Psychopathology Symptoms"
DF[which(DF$Results=="lm"),"Results"]<-"Linear Rate of Change"
DF[which(DF$Results=="future"),"Results"]<-"Future Psychopathology Symptoms"

#Relabels for Predictor
DF[which(DF$Predictor=="DEMO_Male"),"Predictor"]<-"Biological Sex"
DF[which(DF$Predictor=="FAM_fes_y_ss_fc"),"Predictor"]<-"Family Conflict Sum"
DF[which(DF$Predictor=="ENVIR_peq_rumor_vic"),"Predictor"]<-"A Kid Spread Rumors"
DF[which(DF$Predictor=="ENVIR_peq_exclude_vic"),"Predictor"]<-"A Kid Left Me Out"
DF[which(DF$Predictor=="FAM_fes_youth_q1"),"Predictor"]<-"We Fight alot in our Family"
DF[which(DF$Predictor=="ENVIR_peq_ss_reputation_victim"),"Predictor"]<-"Reputational Victimization Sum"

#Revise Categories for Sex Differences
DF <- DF %>% mutate(
    Level = if_else(grepl("Biological Sex", Predictor),
            if_else(Level == 1, "M", "F"),
            as.character(Level))
)
DF$GroupKFold<-factor(DF$GroupKFold)
DF$Level<-factor(DF$Level,levels=c("M","F",unique(sort(as.numeric(DF$Level)))))

#Create Separation Between Numerical and Categorical Predictors
DF$CatPred<-DF$Predictions; DF$NumPred<-DF$Predictions
DF[grepl("Biological Sex",DF$Predictor),"NumPred"]<-NA
DF[!grepl("Biological Sex",DF$Predictor),"CatPred"]<-NA

######
### Average Across CV Folds
######

DF_mean <- DF %>%
  group_by(Facet, Level) %>%
  summarize(Averages = mean(Predictions, na.rm = TRUE))
DF<-merge(DF, DF_mean, by=c("Facet","Level"), all=TRUE)

DF$GrpFIT<-as.character(DF$Facet)
DF[!is.na(DF$CatPred),"GrpFIT"]<-1:length(which(!is.na(DF$CatPred)==TRUE))

######
### Create the GGPlot
######

theme_set(theme_bw(base_size = 16))
F1<-ggplot(DF) +
    geom_jitter(DF, mapping=aes(x=Level, y=CatPred, group=GroupKFold, color=GroupKFold), width=0.25, size=2.5, shape=16, alpha=0.5) + 
    geom_point(DF, mapping=aes(x=Level, y=NumPred, group=GroupKFold, color=GroupKFold), size=1.5, shape=16, alpha=0.25) + 
    geom_smooth(data=DF, aes(x=Level, y=Predictions, group=GroupKFold, color=GroupKFold), method=loess, se=FALSE, linewidth=2.5) +
    scale_color_manual(values = c(
        "1" = "#9e0606",  
        "2" = "#9e0606",  
        "3" = "#9e0606",  
        "4" = "#9e0606",  
        "5" = "#9e0606" 
    )) +
    geom_point(DF, mapping=aes(x=Level, y=Averages), color="#ffffff", size=4, shape=18, alpha=0.85) + 
    geom_point(DF, mapping=aes(x=Level, y=Averages), color="#000000", size=3, shape=18, alpha=0.85) + 
    geom_smooth(DF, mapping = aes(x=Level, y=Averages, group=GrpFIT), color = "#ffffff", method = "loess", se = FALSE, linewidth = 4) +  
    geom_smooth(DF, mapping = aes(x=Level, y=Averages, group=GrpFIT), color = "#000000", method = "loess", se = FALSE, linewidth = 3) +  
    theme(legend.position="none") +
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color="white", face="bold")) +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
    theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) + 
    theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
    xlab("Range of Feature Values") + ylab("Prediction of Mental Health Outcomes") +         
    facet_wrap(~ Facet, scales = "free", nrow = 3, labeller = 
    labeller(Facet = c(
        "r1" = "1st: Family Conflict Sum", 
        "lm1" = "1st: A Kid Left Me Out",    
        "future1" = "1st: Reputational Victimization Sum",             
        "r2" = "2nd: We Fight alot in our Family", 
        "lm2" = "2nd: Biological Sex", 
        "future2" = "2nd: Biological Sex", 
        "r3" = "3rd: Reputational Victimization Sum",
        "lm3" = "3rd: Reputational Victimization Sum",
        "future3" = "3rd: A Kid Spread Rumors"
        )
    )
)
ggsave(plot=F1,filename=paste0(ROOT,"/Analysis/Visualize/F7_PDP_",{TODAY},".png"),device="png",width=15, height=8.5,units='in')

######
### Create the Second Version
######

"#8c0303" #1857d6 
"#0e6e13" #d61818 
"#0618bd" #0ba33e
SUBSET<-DF[grepl("^future",DF$Facet),]
COLOR="#0ba33e"; YAXIS="Future Symptoms"
F1<-ggplot(SUBSET) +
    geom_jitter(SUBSET, mapping=aes(x=Level, y=CatPred, group=GroupKFold, color=GroupKFold), width=0.2, size=3, shape=16, alpha=0.7) + 
    geom_smooth(data=SUBSET, aes(x=Level, y=Predictions, group=GroupKFold), method=loess, color = "#000000", se=FALSE, linewidth=2, alpha=0.7) +
    geom_smooth(data=SUBSET, aes(x=Level, y=Predictions, group=GroupKFold, color=GroupKFold), method=loess, se=FALSE, linewidth=1.5, alpha=0.7) +
    scale_color_manual(values = c(
        "1" = COLOR,  
        "2" = COLOR,  
        "3" = COLOR,  
        "4" = COLOR,  
        "5" = COLOR
    )) +
    scale_y_continuous(limits = c(min(SUBSET$Predictions), max(SUBSET$Predictions))) +
    geom_point(SUBSET, mapping=aes(x=Level, y=Averages), color="#ffffff", size=7, shape=18, alpha=0.85) + 
    geom_point(SUBSET, mapping=aes(x=Level, y=Averages), color="#000000", size=5, shape=18, alpha=0.85) + 
    geom_smooth(SUBSET, mapping = aes(x=Level, y=Averages, group=GrpFIT), color = "#ffffff", method = "loess", se = FALSE, linewidth = 4) +  
    geom_smooth(SUBSET, mapping = aes(x=Level, y=Averages, group=GrpFIT), color = "#000000", method = "loess", se = FALSE, linewidth = 3) +  
    theme(legend.position="none") +
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color="white", face="bold")) +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
    theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) + 
    theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
    xlab("Feature Values") + ylab(YAXIS) +         
    facet_wrap(~ Facet, scales = "free", nrow = 3, labeller = 
    labeller(Facet = c(
        "future1" = "1st: Reputational Victimization Sum",                
        "future2" = "2nd: Biological Sex", 
        "future3" = "3rd: A Kid Spread Rumors"
        )
    )
); ggsave(plot=F1,filename=paste0(ROOT,"/Analysis/Visualize/F1_PDP_C_",{TODAY},".png"),device="png",width=5, height=8.5,units='in')

SUBSET<-DF[grepl("^lm",DF$Facet),]
COLOR="#1857d6"; YAXIS="Rate of Change"
F1<-ggplot(SUBSET) +
    geom_jitter(SUBSET, mapping=aes(x=Level, y=CatPred, group=GroupKFold, color=GroupKFold), width=0.2, size=3, shape=16, alpha=0.7) + 
    geom_smooth(data=SUBSET, aes(x=Level, y=Predictions, group=GroupKFold), method=loess, color = "#000000", se=FALSE, linewidth=2, alpha=0.7) +
    geom_smooth(data=SUBSET, aes(x=Level, y=Predictions, group=GroupKFold, color=GroupKFold), method=loess, se=FALSE, linewidth=1.5, alpha=0.7) +
    scale_color_manual(values = c(
        "1" = COLOR,  
        "2" = COLOR,  
        "3" = COLOR,  
        "4" = COLOR,  
        "5" = COLOR
    )) +
    scale_y_continuous(limits = c(min(SUBSET$Predictions), max(SUBSET$Predictions))) +
    geom_point(SUBSET, mapping=aes(x=Level, y=Averages), color="#ffffff", size=7, shape=18, alpha=0.85) + 
    geom_point(SUBSET, mapping=aes(x=Level, y=Averages), color="#000000", size=5, shape=18, alpha=0.85) + 
    geom_smooth(SUBSET, mapping = aes(x=Level, y=Averages, group=GrpFIT), color = "#ffffff", method = "loess", se = FALSE, linewidth = 4) +  
    geom_smooth(SUBSET, mapping = aes(x=Level, y=Averages, group=GrpFIT), color = "#000000", method = "loess", se = FALSE, linewidth = 3) +  
    theme(legend.position="none") +
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color="white", face="bold")) +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
    theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) + 
    theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
    xlab("Feature Values") + ylab(YAXIS) +         
    facet_wrap(~ Facet, scales = "free", nrow = 3, labeller = 
    labeller(Facet = c(
        "lm1" = "1st: A Kid Left Me Out",                  
        "lm2" = "2nd: Biological Sex", 
        "lm3" = "3rd: Reputational Victimization Sum"
        )
    )
); ggsave(plot=F1,filename=paste0(ROOT,"/Analysis/Visualize/F1_PDP_B_",{TODAY},".png"),device="png",width=5, height=8.5,units='in')

SUBSET<-DF[grepl("^r",DF$Facet),]
COLOR="#d61818"; YAXIS="Current Symptoms"
F1<-ggplot(SUBSET) +
    geom_jitter(SUBSET, mapping=aes(x=Level, y=CatPred, group=GroupKFold, color=GroupKFold), width=0.2, size=3, shape=16, alpha=0.7) + 
    geom_smooth(data=SUBSET, aes(x=Level, y=Predictions, group=GroupKFold), method=loess, color = "#000000", se=FALSE, linewidth=2, alpha=0.7) +
    geom_smooth(data=SUBSET, aes(x=Level, y=Predictions, group=GroupKFold, color=GroupKFold), method=loess, se=FALSE, linewidth=1.5, alpha=0.7) +
    scale_color_manual(values = c(
        "1" = COLOR,  
        "2" = COLOR,  
        "3" = COLOR,  
        "4" = COLOR,  
        "5" = COLOR
    )) +
    scale_y_continuous(limits = c(min(SUBSET$Predictions), max(SUBSET$Predictions))) +
    geom_point(SUBSET, mapping=aes(x=Level, y=Averages), color="#ffffff", size=7, shape=18, alpha=0.85) + 
    geom_point(SUBSET, mapping=aes(x=Level, y=Averages), color="#000000", size=5, shape=18, alpha=0.85) + 
    geom_smooth(SUBSET, mapping = aes(x=Level, y=Averages, group=GrpFIT), color = "#ffffff", method = "loess", se = FALSE, linewidth = 4) +  
    geom_smooth(SUBSET, mapping = aes(x=Level, y=Averages, group=GrpFIT), color = "#000000", method = "loess", se = FALSE, linewidth = 3) +  
    theme(legend.position="none") +
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color="white", face="bold")) +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
    theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) + 
    theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
    xlab("Feature Values") + ylab(YAXIS) +         
    facet_wrap(~ Facet, scales = "free", nrow = 3, labeller = 
    labeller(Facet = c(
        "r1" = "1st: Family Conflict Sum",            
        "r2" = "2nd: We Fight alot in our Family", 
        "r3" = "3rd: Reputational Victimization Sum"
        )
    )
); ggsave(plot=F1,filename=paste0(ROOT,"/Analysis/Visualize/F1_PDP_A_",{TODAY},".png"),device="png",width=5, height=8.5,units='in')

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######