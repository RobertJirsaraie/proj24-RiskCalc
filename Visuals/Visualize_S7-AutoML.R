#!/usr/bin/env Rscript
######################

invisible(lapply(c("caret","readxl","stringr","tidyr","plyr","dplyr","tidyverse","corrplot","lattice","stringi","parameters","lme4","lmerTest","ggplot2","effectsize","jsonlite"), require, character.only=TRUE))
ROOT<-"/Users/rjirsara/Box Sync/Research/proj24-RiskCalculator/analysis"
COLORS<-fromJSON(paste0(ROOT,"/Software/ColorSchemas.json"))
TODAY<-format(Sys.time(), "%Y%m%d")

capitalize <- function(x) {
  ifelse(grepl("MRI", x), x, paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x)))))
}

ORDER<-c(
    "Multimodal",
    "Family",
    "Environmental",
    "Adversity",
    "Behavior",
    "Demographics",
    "Cognition",
    "fMRI",
    "sMRI",
    "dMRI"
)

######
### Figure Accuracy Metrics Per Psychopathology Dimension
######

ACCURACY <- data.frame(RESULTS=character(), FEATSET=character(), ALGO=character(), COD=numeric(), MEAN=character(), stringsAsFactors=FALSE)
for (FILE in grep("Preproc.*totalprob.*mod-.*accuracy", list.files(ROOT, recursive = TRUE, full.names = TRUE), value = TRUE)){    
    if (grepl("_resid", FILE)){
        next  
    }
    DF<-read.csv(FILE); print(paste0("Loaded File: ", FILE))
    COD<-DF[1:5,ncol(DF)]
    df <- data.frame(
      RESULTS = rep(unlist(strsplit(basename(dirname(FILE)), "_"))[1], length(COD)),
      SET = rep(gsub("df-", "", unlist(strsplit(basename(FILE), "_"))[1]), length(COD)),
      MOD = rep(gsub("mod-", "", unlist(strsplit(basename(FILE), "_"))[2]), length(COD)),
      COD = COD,
      MEAN = rep(mean(COD), length(COD)),
      stringsAsFactors = FALSE
    )
    ACCURACY<-rbind(ACCURACY,df)
}; names(ACCURACY)<-c("RESULTS","FEATSET","ALGO","COD","MEAN")
ACCURACY[which(ACCURACY$RESULTS=="Preproc4"),"result"]<-"Current Symptoms"
ACCURACY[which(ACCURACY$RESULTS=="Preproc5"),"result"]<-"Rate of Change"
ACCURACY[which(ACCURACY$RESULTS=="Preproc6"),"result"]<-"Future Symptoms"
ACCURACY$result<-factor(ACCURACY$result,levels=c("Current Symptoms","Rate of Change","Future Symptoms"))
ACCURACY$FEATSET <- sapply(as.character(ACCURACY$FEATSET), capitalize)
ACCURACY$MEAN <- round(as.numeric(ACCURACY$MEAN), digits=3)
ACCURACY$FEATSET <- factor(ACCURACY$FEATSET,levels=ORDER)

theme_set(theme_bw(base_size = 16))
F3<-ggplot() + 
    geom_hline(yintercept = 0, color = "black", size = 0.6, alpha=0.5) + 
    geom_jitter(ACCURACY, width=0.25, size=2, alpha=0.35, mapping=aes(x=FEATSET, y=COD, colour=ALGO)) + 
    geom_point(ACCURACY, mapping=aes(x=FEATSET, y=MEAN, colour=ALGO, group=ALGO), size=11, shape=18, color="white") + 
    geom_point(ACCURACY, mapping=aes(x=FEATSET, y=MEAN, colour=ALGO, group=ALGO), size=9, shape=18, alpha=0.35) + 
    scale_color_manual(values = c("#8f3636", "#8f8f8f"))  +
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