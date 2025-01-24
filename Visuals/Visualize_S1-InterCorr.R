#!/usr/bin/env Rscript
######################

invisible(lapply(c("caret","readxl","stringr","tidyr","plyr","dplyr","tidyverse","corrplot","lattice","stringi","parameters","lme4","lmerTest","ggplot2","effectsize","jsonlite"), require, character.only=TRUE))
ROOT<-"/Users/rjirsara/Box Sync/Research/proj24-RiskCalculator"
COLORS<-fromJSON(paste0(ROOT,"/Analysis/Software/ColorSchemas.json"))
TODAY<-format(Sys.time(), "%Y%m%d")

cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

######
### Correlation Matrix of Inter-Correlations Between Dimensions
######

DF<-read.csv(paste0(ROOT,"/datasets/Preproc4_Primary/n24838x969_multimodal.csv"))
DF<-DF[,c(1:2,which(names(DF)=="DEMO_Age"), grep("TARGET",names(DF)))]
names(DF)[3]<-"Age"
names(DF)[4]<-"Inattention Symptoms"
names(DF)[5]<-"Externalizing Symptoms"
names(DF)[6]<-"Internalizing Symptoms"
names(DF)[7]<-"P-Factor Symptoms"
STORE<-DF
DF<-read.csv(paste0(ROOT,"/datasets/Preproc5_Longitudinal/n9599x973_multimodal.csv"))
DF<-DF[,c(1:2, grep("TARGET",names(DF)))]
DF<-DF[,!grepl("_r$",names(DF))]
names(DF)[3]<-"Internalizing  Changes"
names(DF)[4]<-"Inattention Changes"
names(DF)[5]<-"Externalizing Changes"
names(DF)[6]<-"P-Factor Changes"
CORMAT<-merge(STORE,DF,by=1:2,all=TRUE); CORMAT<-CORMAT[,-c(1:2)]
CORMAT<-CORMAT[,c(1,5,4,3,2,9,6,8,7)]
r.mat<-as.matrix(cor(CORMAT,use="complete.obs", method = "spearman")) ; p.mat <- cor.mtest(CORMAT)
col <- colorRampPalette(c("#8f1010","#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA","#1c4670"))
col <- colorRampPalette(c("#1c4670","#4477AA","#77AADD","#FFFFFF","#EE9988","#BB4444","#8f1010"))
png(filename = paste0(ROOT, "/Visualize/Supplimentary/F1C_",{TODAY},".png"), width = 7.5, height = 7.5, units = "in", res = 300)
corrplot(
  r.mat, 
  method = "color", 
  type = "lower", 
  col = col(100), 
  addCoef.col = "black", 
  tl.col = "black", 
  tl.srt = 45, 
  p.mat = p.mat, 
  sig.level = 0.05, 
  insig = "blank", 
  diag = TRUE, 
  lwd = 9, 
  addgrid.col = "black"
)
dev.off()

######
### Distributions of Psychopathology Severity
######

DF<-read.csv(paste0(ROOT,"/datasets/Preproc4_Primary/n24838x969_multimodal.csv"))
DF<-DF[,c(1:2,which(names(DF)=="DEMO_Age"), grep("TARGET",names(DF)))]
names(DF)[2]<-"Timepoint"
names(DF)[3]<-"Age"
names(DF)[4]<-"Inattention"
names(DF)[5]<-"Externalizing"
names(DF)[6]<-"Internalizing"
names(DF)[7]<-"P-Factor"
PSYCHO <-pivot_longer(DF,cols=4:7,names_to="Dimensions",values_to="Values")
PSYCHO$Dimensions<-factor(PSYCHO$Dimensions, levels=c("P-Factor","Inattention","Externalizing","Internalizing"))
MEAN <- ddply(PSYCHO, "Dimensions", summarize, mean=mean(Values))
F1A<-ggplot(PSYCHO, aes(x = Values, color = Dimensions, fill = Dimensions)) +
    geom_histogram(aes(y = after_stat(count)), bins = 35, position = "identity", alpha = 0.25) +
    geom_vline(data = MEAN, aes(xintercept = mean, color = Dimensions), linetype = "longdash", linewidth = 1.5) +
    theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) + 
    theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color="white", face="bold")) + 
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
    scale_color_manual(values = c("#803b09", "#ffc026", "#c70000", "#003cc7")) +
    scale_fill_manual(values = c("#803b09", "#ffc026", "#c70000", "#003cc7")) +
    xlab("Symptom Severity") + ylab("Frequency") +
    theme(legend.position = "none")
ggsave(plot=F1A,filename=paste0(ROOT,"/Visualize/Supplimentary/F1A_",{TODAY},".png"), device="png", width=5.5, height=3.5, units='in')

######
### Distributions for the Rate of Change in Psychopathology
######

DF<-read.csv(paste0(ROOT,"/datasets/Preproc5_Longitudinal/n9599x973_multimodal.csv"))
DF<-DF[,c(1:2,which(names(DF)=="DEMO_Age"), grep("TARGET",names(DF)))]
DF<-DF[,!grepl("_r$",names(DF))]
names(DF)[2]<-"Timepoint"
names(DF)[3]<-"Age"
names(DF)[4]<-"Inattention"
names(DF)[5]<-"Externalizing"
names(DF)[6]<-"Internalizing"
names(DF)[7]<-"P-Factor"
PSYCHO <-pivot_longer(DF,cols=4:7,names_to="Dimensions",values_to="Values") 
PSYCHO$Dimensions<-factor(PSYCHO$Dimensions, levels=c("P-Factor","Inattention","Externalizing","Internalizing"))
MEAN <- ddply(PSYCHO, "Dimensions", summarize, mean=mean(Values))
F1B<-ggplot(PSYCHO, aes(x = Values, color = Dimensions, fill = Dimensions)) +
    geom_histogram(aes(y = after_stat(count)), bins = 35, position = "identity", alpha = 0.25) +
    geom_vline(data = MEAN, aes(xintercept = mean, color = Dimensions), linetype = "longdash", linewidth = 1.5) +
    theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) + 
    theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color="white", face="bold")) + 
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
    scale_color_manual(values = c("#803b09", "#ffc026", "#c70000", "#003cc7")) +
    scale_fill_manual(values = c("#803b09", "#ffc026", "#c70000", "#003cc7")) +
    xlab("Linear Rate of Change") + ylab("Frequency") +
    theme(legend.position = "none")
ggsave(plot=F1B,filename=paste0(ROOT,"/analysis/Visualize/F1B_",{TODAY},".png"), device="png", width=5.5, height=3.5, units='in')

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######