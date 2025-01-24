#!/usr/bin/env Rscript
######################

invisible(lapply(c("caret","readxl","stringr","tidyr","plyr","dplyr","tidyverse","corrplot","lattice","stringi","parameters","lme4","lmerTest","ggplot2","effectsize","jsonlite"), require, character.only=TRUE))
ROOT<-"/Users/rjirsara/Box Sync/Research/proj24-RiskCalculator/analysis"
COLORS<-fromJSON(paste0(ROOT,"/Software/ColorSchemas.json"))
TODAY<-format(Sys.time(), "%Y%m%d")

capitalize <- function(x) {
  ifelse(grepl("MRI", x), x, paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x)))))
}

######
### Load Results
######

#Predictions
for (FILE in grep("Preproc6_totalprob_future", list.files(ROOT, pattern="XGB.*predict", recursive=TRUE, full.names=TRUE), value=TRUE)){
    print(FILE)
    CONTENT<-read.csv(FILE); print(paste0("Loaded File: ", FILE))
    TARGET<-unlist(strsplit(basename(dirname(FILE)),"_"))[2]
    FEATURES<-gsub("df-","",unlist(strsplit(basename(FILE),"_"))[1])
    CONTENT[,'featset']<-FEATURES
    CONTENT[,'target']<-TARGET
    if (!exists("PREDICT")){
        PREDICT<-CONTENT
    } else {
        PREDICT<-rbind(PREDICT,CONTENT)
    }
}; PREDICT$target<-factor(PREDICT$target)

#Accuracy
for (FILE in grep("Preproc6_totalprob_future", list.files(ROOT, pattern="XGB.*accuracy", recursive=TRUE, full.names=TRUE), value=TRUE)){
    CONTENT<-read.csv(FILE); print(paste0("Loaded File: ", FILE))
    TARGET<-unlist(strsplit(basename(dirname(FILE)),"_"))[2]
    FEATURES<-gsub("df-","",unlist(strsplit(basename(FILE),"_"))[1])
    DF <- as.data.frame(t(as.data.frame(colMeans(CONTENT))))
    DF[,'featset']<-FEATURES; DF[,'target']<-TARGET; DF$split<-NULL
    if (!exists("ACCURACY")){
        ACCURACY<-DF
    } else {
        ACCURACY<-rbind(ACCURACY,DF)
    }
}; row.names(ACCURACY)<-NULL

ACCURACY <- ACCURACY %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
PREDICT<-merge(PREDICT,ACCURACY, by=c("featset","target"),all=TRUE)

######
### Prediction Accuracy 
######

PREDICT<-PREDICT[order(PREDICT$test_cod,decreasing=TRUE),]
PFACT<-PREDICT[which(PREDICT$target=="totalprob"),]
MULTI<-PFACT[which(PFACT$featset=="multimodal"),]
FACET<-PFACT[which(PFACT$featset!="multimodal"),]
B0 <- coef(lm(MULTI$predictions ~ MULTI$targets))[1]
B1 <- coef(lm(MULTI$predictions ~ MULTI$targets))[2]
FACET$featcolor<-sapply(FACET$featset, function(x) COLORS$feat_colors[[x]])
FACET$featset <- sapply(FACET$featset, capitalize)
MULTI$featset <- sapply(MULTI$featset, capitalize)
FACET$featset<-factor(FACET$featset,unique(FACET$featset))

theme_set(theme_bw(base_size = 24)); MULTI[which(MULTI$featset=="Multimodal"),"featset"]<-"Multimodal Features"
F1A<-ggplot() + 
        geom_point(data=MULTI, aes(x=targets, y=predictions), color="#000000", size=1.50, alpha=0.35) +
        geom_point(data=MULTI, aes(x=targets, y=predictions), color="#6e6e6e", size=1, alpha=0.35) +
        geom_abline(intercept=0, slope=1, linewidth=3, color="#000000", linetype="dashed") + 
        geom_smooth(data=MULTI, aes(x=targets, y=predictions), color="#000000", fullrange=TRUE, method=lm, se=FALSE, linewidth=3.5) + 
        geom_smooth(data=MULTI, aes(x=targets, y=predictions), color="#6e6e6e", fullrange=TRUE, method=lm, se=FALSE, linewidth=2) + 
        theme(strip.background = element_rect(colour = "black", fill = "black")) +
        theme(strip.text = element_text(color="white", face="bold")) +
        theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
        theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) + 
        theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
        xlab("Future Symptoms") + ylab("Projected Symptoms") + 
        xlim(floor(min(MULTI$targets)), ceiling(max(MULTI$targets))) + 
        ylim(floor(min(MULTI$targets)), ceiling(max(MULTI$targets))) + 
        theme(legend.position = "none") + facet_wrap(~featset)
ggsave(plot=F1A,filename=paste0(ROOT,"/Visualize/F1A_",{TODAY},".png"),device="png",width=7.35,height=8.5,units='in')

theme_set(theme_bw(base_size = 16))
F1B<-ggplot() + 
        geom_point(data=FACET, aes(x=targets, y=predictions), color="#000000", size=1, alpha=0.35) +
        geom_point(data=FACET, aes(x=targets, y=predictions, color=featset), size=0.5, alpha=0.35) +
        geom_abline(intercept=B0, slope=B1, linewidth=2.5, color="#000000", linetype="solid") + 
        geom_abline(intercept=B0, slope=B1, linewidth=1.5, color="#6e6e6e", linetype="solid") + 
        geom_smooth(data=FACET, aes(x=targets, y=predictions), color="#000000", fullrange=TRUE, method=lm, se=FALSE, linewidth=2.5) + 
        geom_smooth(data=FACET, aes(x=targets, y=predictions, color=featset, group=featset), fullrange=TRUE, method=lm, se=FALSE, linewidth=1.5) + 
        theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) + 
        theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
        theme(strip.background = element_rect(colour = "black", fill = "black")) +
        theme(strip.text = element_text(color="white", face="bold")) + 
        theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
        scale_color_manual(values = unique(FACET$featcolor)) +
        xlab("Future Symptoms") + ylab("Projected Symptoms") + 
        theme(legend.position = "none") + facet_wrap(~featset)
ggsave(plot=F1B,filename=paste0(ROOT,"/Visualize/F1B_",{TODAY},".png"),device="png",width=7.5,height=8.5,units='in')

for (EVENT in unique(PREDICT$eventname)){
    SUBSET<-PREDICT[which(PREDICT$eventname==EVENT),]
    print(EVENT)
    print(postResample(pred = SUBSET$predictions, obs = SUBSET$targets))
}

######
### Figure of Gini Impurity
######

for (FILE in grep("Preproc5.*totalprob", list.files(ROOT, pattern="multimodal.*impurity", recursive=TRUE, full.names=TRUE), value=TRUE)){
    CONTENT<-read.csv(FILE); CONTENT[,'gini']<-rowMeans(CONTENT[,2:6])*100
    CONTENT<-CONTENT[!grepl("Split",names(CONTENT))]
    CONTENT <- CONTENT %>% separate(X, into = c("featset", "feature"), sep = "_", extra = "merge")
    TARGET<-unlist(strsplit(basename(dirname(FILE)),"_"))[2]
    CONTENT[,'target']<-TARGET
    if (!exists("GINI")){
        GINI<-CONTENT
    } else {
        GINI<-rbind(GINI,CONTENT)
    }
}
TOTAL <- as.data.frame(GINI %>%
    group_by(featset,feature) %>%
    summarize(
        gini = mean(gini, na.rm = TRUE),    
        target = first(target),
        .groups = "drop"
    )
)
SUMMARY <- ddply(TOTAL, .(featset), summarize, featset = unique(featset), gini = mean(gini, na.rm = TRUE))
SUMMARY <- SUMMARY %>% arrange(desc(gini))
for (FEATSET in COLORS$feat_labels){
    GINI[which(GINI$featset==FEATSET[2]),"featset"]<-FEATSET[1]
}
GINI[which(GINI$featset=="SMRI"),"featset"]<-"sMRI"; GINI<-GINI[order(GINI$featset),]
GINI$featcolor <- sapply(GINI$featset, function(x) COLORS$feat_colors[[x]])
GINI$featset <- sapply(GINI$featset, capitalize)
GINI$featset <- factor(GINI$featset, levels = levels(FACET$featset))
GINI<-GINI[order(GINI$featset),]
theme_set(theme_bw(base_size = 16))
F2<-ggplot() + 
    geom_hline(yintercept = 0, color = "black", linewidth = 0.25, alpha=0.85) + 
    geom_jitter(GINI, width=0.3, size=3.5, alpha=0.5, mapping=aes(x=featset, y=gini, colour=featset)) + 
    scale_color_manual(values = unique(GINI$featcolor)) +
    theme(legend.position="none") + xlab("Feature Sets") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color="white", face="bold")) +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) + 
    theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
    ylab("Predictive Power (Gini Impurities)")
ggsave(plot=F2,filename=paste0(ROOT,"/Visualize/F2_",{TODAY},".png"), device="png", width=15, height=8.5, units='in')

######
### Table of Top Features Per Target
######

for (FILE in grep("Preproc6_totalprob_future.*multimodal.*XGB.*impurity", list.files(ROOT, recursive = TRUE, full.names = TRUE), value = TRUE)){
    CONTENT<-read.csv(FILE); CONTENT[,'gini']<-rowMeans(CONTENT[,2:6])*100
    CONTENT<-CONTENT[!grepl("Split",names(CONTENT))]
    CONTENT <- CONTENT %>% separate(X, into = c("featset", "feature"), sep = "_", extra = "merge")
    TARGET<-gsub("_r","",gsub("bpm_","",basename(dirname(FILE))))
    NITER<-gsub("iter-","",unlist(strsplit(basename(FILE),"_"))[3])
    CONTENT[,'niter']<-NITER; CONTENT[,'target']<-TARGET
    if (!exists("GINI")){
        GINI<-CONTENT
    } else {
        GINI<-rbind(GINI,CONTENT)
    }
}

print(as.data.frame(GINI %>%
    group_by(target) %>%
    arrange(desc(gini)) %>%
    slice_head(n = 50) %>%
    ungroup()
    )
)

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######