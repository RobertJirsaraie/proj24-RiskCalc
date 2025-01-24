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
### Figure Accuracy Metrics Per Psychopathology Dimension
######

for (FILE in grep("Preproc",list.files(ROOT, pattern="XGB.*accuracy", recursive=TRUE, full.names=TRUE),value=TRUE)){
    DF<-read.csv(FILE); print(paste0("Loaded File: ", FILE))
    TARGET<-unlist(strsplit(basename(dirname(FILE)),"_"))[2]
    FEATURES<-gsub("df-","",unlist(strsplit(basename(FILE),"_"))[1])
    DF[,'featset']<-FEATURES; DF[,'target']<-TARGET
    DF[,'result']<-unlist(strsplit(basename(dirname(FILE)),"_"))[1]
    if (!exists("ACCURACY")){
        ACCURACY<-DF
    } else {
        ACCURACY<-rbind(ACCURACY,DF)
    }
}; row.names(ACCURACY)<-NULL
ACCURACY <- ACCURACY %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
ACCURACY[which(ACCURACY$result=="Preproc4"),"result"]<-"Current Symptoms"
ACCURACY[which(ACCURACY$result=="Preproc5"),"result"]<-"Rate of Change"
ACCURACY[which(ACCURACY$result=="Preproc6"),"result"]<-"Future Symptoms"
ACCURACY$result<-factor(ACCURACY$result,levels=c("Current Symptoms","Rate of Change","Future Symptoms"))
JITTER<-ddply(ACCURACY, .(result, target, featset, split), summarize,
            test_cod = mean(test_cod, na.rm = TRUE),
            result = unique(result)
)

SUMMARY <- ddply(ACCURACY, .(result, featset, target), summarize,
            target = unique(target),
            featset = unique(featset),
            train_cod = mean(train_cod, na.rm = TRUE),
            train_mae = mean(train_mae, na.rm = TRUE),
            train_mse = mean(train_mse, na.rm = TRUE),
            test_cod = mean(test_cod, na.rm = TRUE),
            test_mae = mean(test_mae, na.rm = TRUE),
            test_mse = mean(test_mse, na.rm = TRUE),
            result = unique(result)
)

SUMMARY<-SUMMARY[order(SUMMARY$test_cod,decreasing=TRUE), c("result","featset","test_cod","target")]
SUMMARY <- SUMMARY %>% group_by(featset) %>% mutate(avg_test_cod = mean(test_cod, na.rm = TRUE))
SUMMARY <- SUMMARY %>% arrange(desc(avg_test_cod))
SUMMARY$featset <- factor(SUMMARY$featset, levels=unique(SUMMARY$featset))
targ_colors <- unlist(COLORS$targ_colors)
SUMMARY$targcol <- targ_colors[match(SUMMARY$target, names(targ_colors))]
JITTER$targcol <- targ_colors[match(JITTER$target, names(targ_colors))]
SUMMARY$featset <- sapply(as.character(SUMMARY$featset), capitalize)
JITTER$featset <- sapply(as.character(JITTER$featset), capitalize)
SUMMARY$featset<-factor(SUMMARY$featset,levels=unique(SUMMARY[order(SUMMARY$test_cod,decreasing=TRUE),]$featset))
JITTER$featset<-factor(JITTER$featset,levels=unique(SUMMARY[order(SUMMARY$test_cod,decreasing=TRUE),]$featset))

theme_set(theme_bw(base_size = 16))
F3<-ggplot() + 
    geom_hline(yintercept = 0, color = "black", size = 0.6, alpha=0.5) + 
    geom_jitter(JITTER, width=0.25, size=2, alpha=0.35, mapping=aes(x=featset, y=test_cod, colour=target)) + 
    geom_point(SUMMARY, mapping=aes(x=featset, y=test_cod, colour=target, group=target), size=11, shape=18, color="white") + 
    geom_point(SUMMARY, mapping=aes(x=featset, y=test_cod, colour=target, group=target), size=9, shape=18, alpha=0.75) + 
    scale_color_manual(values=targ_colors) +
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color = "white", face="bold")) + 
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color="white", face="bold")) +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) + 
    theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
    xlab("Feature Set") + ylab(expression("Predictive Accuracy" ~ (R^2))) +
    facet_wrap(~result, ncol=1)
ggsave(plot=F3,filename=paste0(ROOT,"/Visualize/FS3_",{TODAY},".png"),device="png",width=15, height=8.5,units='in')

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######