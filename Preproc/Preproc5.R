#!/usr/bin/env Rscript
######################

invisible(lapply(c("stringr","tidyr","plyr","dplyr","tidyverse","ggplot2","readxl","writexl"), require, character.only=TRUE))
ROOT<-"/Users/robertjirsaraie/Library/CloudStorage/Box-Box/Research/proj24-RiskCalculator"
GUIDE<-read.csv(paste0(ROOT,"/datasets/ABCD/ABCD_Dictionary.csv"))
TODAY<-format(Sys.time(), "%Y%m%d")

######
### 1. Remove Features Missing more than 50 Percent
######

DATASETS<-list.files(path = paste0(ROOT,"/datasets/Preproc4_Primary"), pattern = "*.csv$", recursive=TRUE, full.names = TRUE)
DICT<-data.frame(read_excel(paste0(ROOT,"/datasets/Preproc4_Primary/Dictionary-postproc_989x14.xlsx")))
DICT[which(DICT$Completeness < 50),"Inclusion"]<-"No"
write_xlsx(DICT, paste0(ROOT,"/datasets/Preproc4_Primary/Dictionary-postproc_989x14.xlsx"))
EXCLUDE<-DICT[which(DICT$Inclusion=='No'),c("Set","Feature")]
EXCLUDE<-paste(EXCLUDE$Set, EXCLUDE$Feature, sep = "_")

#Multimodal Set
DF<-read.csv(DATASETS[grep("multimodal",DATASETS)])
DF<-DF[,-which(names(DF) %in% EXCLUDE)]
write.csv(DF,paste0(ROOT,"/datasets/Preproc4_Primary/n",dim(DF)[1],"x",dim(DF)[2],"_multimodal.csv"),row.names=FALSE)

#Individual Feature Sets
for (SET in unique(DICT[which(DICT$Inclusion=='No'),"Set"])){
	DF<-read.csv(DATASETS[grep(tolower(SET),DATASETS)]); DF<-DF[,-which(names(DF) %in% EXCLUDE)]
	LABEL<-gsub(".csv","",unlist(strsplit(basename(DATASETS[grep(tolower(SET),DATASETS)]),"_"))[2])
	write.csv(DF,paste0(ROOT,"/datasets/Preproc4_Primary/n",dim(DF)[1],"x",dim(DF)[2],"_",LABEL,".csv"),row.names=FALSE)
	file.remove(DATASETS[grep(tolower(SET),DATASETS)],force=TRUE)
}

######
### 2. Compute the Slopes For Each Person
######

#Only Select Subjects with Multiple Timepoints
DF<-read.csv(paste0(ROOT,"/datasets/Preproc4_Primary/n24838x969_multimodal.csv"))
for (SUBID in unique(DF$src_subject_id)){
  if (length(which(DF$src_subject_id==SUBID)) == 1){
    print(paste0("Removing ", SUBID))
    DF<-DF[which(DF$src_subject_id!=SUBID),]
  }
}
DF<-DF[!is.na(DF$DEMO_Age),]

#Fit a Independent Linear Slope For Each Subject
SLOPES <- as.data.frame(DF %>%
  group_by(src_subject_id) %>%                        
  summarize(
	TARGET_bpm_internal_lm = coef(lm(TARGET_bpm_internal_r ~ DEMO_Age))[2],  
	TARGET_bpm_attention_lm = coef(lm(TARGET_bpm_attention_r ~ DEMO_Age))[2],  
	TARGET_bpm_external_lm = coef(lm(TARGET_bpm_external_r ~ DEMO_Age))[2],  
	TARGET_bpm_totalprob_lm = coef(lm(TARGET_bpm_totalprob_r ~ DEMO_Age))[2],  
  .groups = "drop"                                   
))

######
### 3. Define Longitudinal Features
######

DICT<-read_excel(paste0(ROOT,"/datasets/Preproc4_Primary/Dictionary-postproc_989x14.xlsx"))
DICT<-DICT[which(DICT$Inclusion=="Include"),]; NOBASE<-as.data.frame(DICT[which(DICT$baseline_year_1_arm_1 < 50),])
TRANSPOSE<-NOBASE[,c("Set","Feature","baseline_year_1_arm_1","X2_year_follow_up_y_arm_1","Completeness","Description")]
write_xlsx(TRANSPOSE,paste0(ROOT,"/datasets/Preproc5_Longitudinal/Dictionary-longitudinal_136x7.xlsx"),row.names=FALSE)

######
### Transpose Features Missing Baseline
######

DF<-DF[-which(DF$eventname=="4_year_follow_up_y_arm_1"),]
TRANSPOSED<-paste0(NOBASE$Set,"_",NOBASE$Feature)
DF <- as.data.frame(DF %>% 
	group_by(src_subject_id) %>%
	mutate(across(all_of(TRANSPOSED), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
	ungroup()
)
DF<-DF[which(DF$eventname=="baseline_year_1_arm_1"),]; SLOPES$eventname<-"baseline_year_1_arm_1"
MASTER<-merge(SLOPES,DF,by=c("src_subject_id","eventname"))
write.csv(MASTER,paste0(ROOT,"/datasets/Preproc5_Longitudinal/n9599x973_multimodal.csv"),row.names=FALSE)
SaveFeatSpace("Preproc5_Longitudinal","DEMO","demographics")
SaveFeatSpace("Preproc5_Longitudinal","ENVIR","environmental")
SaveFeatSpace("Preproc5_Longitudinal","FAM","family")
SaveFeatSpace("Preproc5_Longitudinal","BEHAV","behavior") 
SaveFeatSpace("Preproc5_Longitudinal","STRSS","adversity") 
SaveFeatSpace("Preproc5_Longitudinal","COG","cognition") 
SaveFeatSpace("Preproc5_Longitudinal","SMRI","sMRI")
SaveFeatSpace("Preproc5_Longitudinal","fMRI","fMRI")
SaveFeatSpace("Preproc5_Longitudinal","dMRI","dMRI")

######
### Predict Future Timepoint From Baseline
######

DF<-read.csv(paste0(ROOT,"/datasets/Preproc4_Primary/n24838x969_multimodal.csv"))
DF<-DF[which(DF$eventname=="4_year_follow_up_y_arm_1"),]
DF<-DF[,c("src_subject_id",names(DF)[grep("TARGET",names(DF))])]
names(DF)<-gsub("_r","_future",names(DF))
DF$eventname<-"baseline_year_1_arm_1"

MASTER<-merge(DF,MASTER,by=c("src_subject_id","eventname"),all=FALSE)
write.csv(MASTER, paste0(ROOT,"/datasets/Preproc6_Future/n3933x977_multimodal.csv"), row.names=FALSE)
SaveFeatSpace("Preproc6_Future","DEMO","demographics")
SaveFeatSpace("Preproc6_Future","ENVIR","environmental")
SaveFeatSpace("Preproc6_Future","FAM","family")
SaveFeatSpace("Preproc6_Future","BEHAV","behavior") 
SaveFeatSpace("Preproc6_Future","STRSS","adversity") 
SaveFeatSpace("Preproc6_Future","COG","cognition") 
SaveFeatSpace("Preproc6_Future","SMRI","sMRI")
SaveFeatSpace("Preproc6_Future","fMRI","fMRI")
SaveFeatSpace("Preproc6_Future","dMRI","dMRI")

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######