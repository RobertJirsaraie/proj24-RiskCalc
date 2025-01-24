#!/usr/bin/env Rscript
######################

invisible(lapply(c("caret","readxl","stringr","tidyr","plyr","dplyr","tidyverse","corrplot","lattice","stringi","parameters","lme4","lmerTest","ggplot2","effectsize","jsonlite"), require, character.only=TRUE))
ROOT<-"/Users/robertjirsaraie/Library/CloudStorage/Box-Box/Research/proj24-RiskCalculator"

for (FILE in grep("Preproc6_totalprob.*multimodal.*sex-M.*impurity", list.files(ROOT, recursive = TRUE, full.names = TRUE), value = TRUE)){
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
    slice_head(n = 10) %>%
    ungroup()
    )
); rm(GINI)

######
### Highest Error Case Study
######

DF<-read.csv("/Users/robertjirsaraie/Library/CloudStorage/Box-Box/Research/proj24-RiskCalculator/datasets/Preproc4_Primary/n24838x969_multimodal.csv")
scale_to_100 <- function(df) {
  df[] <- lapply(df, function(column) {
    if (is.numeric(column)) {
      # Scale the column to 0-100
      scaled_column <- 100 * (column - min(column, na.rm = TRUE)) /
        (max(column, na.rm = TRUE) - min(column, na.rm = TRUE))
      return(scaled_column)
    } else {
      return(column)  # Keep non-numeric columns unchanged
    }
  })
  return(df)
}
DF<-scale_to_100(DF)
DF<-DF[which(DF$src_subject_id=="NDAR_INV0LEM88KP"),]