#!/usr/bin/env Rscript
######################

invisible(lapply(c("stringr","tidyr","plyr","dplyr","tidyverse","readxl","writexl"), require, character.only=TRUE))
TODAY<-format(Sys.time(), "%Y%m%d"); theme_set(theme_bw(base_size = 16)); options(warn = -1)
ROOT<-"/Users/robertjirsaraie/Library/CloudStorage/Box-Box/Research/proj24-RiskCalculator"
ABCD<-read.csv(paste0(ROOT,"/datasets/ABCD/ABCD_Dictionary.csv"))
ABCD<-ABCD[,c("var_name","var_label","notes")]
names(ABCD)[1]<-"Feature"
LEVELS <- c(
	"baseline_year_1_arm_1",
	"2_year_follow_up_y_arm_1",
	"4_year_follow_up_y_arm_1"
)

Preproc2 <- function(PREFIX, FILENAME){
	VARIABLES<-paste0(PREFIX,"_",DICT[which(DICT$Set==PREFIX),"Feature"])
	INVARIANT<-DICT[which(DICT$Set==PREFIX & DICT$Invariant=="Yes"),"Feature"]
	DATA<-as.data.frame(features[grep(FILENAME, files)])
	DATA<-DATA[,c("src_subject_id","eventname",VARIABLES)]
	DATA<-DATA[which(DATA$eventname %in% LEVELS),]
	for (INVAR in INVARIANT){
		print(paste0("Transforming Non-Time-Varying Variable: ", INVAR))
		for (SUBID in unique(DATA$src_subject_id)){
			VALS<-DATA[DATA$src_subject_id==SUBID,paste0(PREFIX,"_",INVAR)]
			DATA[DATA$src_subject_id==SUBID,paste0(PREFIX,"_",INVAR)]<-na.omit(VALS)[1]
		}
	}
	return(DATA)
}	

SaveFeatSpace <- function(PREFIX, FILENAME){
	if (!grepl("MRI", PREFIX)){
		DF<-MASTER[,COL<-c(1:2,grep("TARGET",names(MASTER)),grep(PREFIX,names(MASTER)))]
	} else {
		DF<-MASTER[,c(1:2,grep("TARGET",names(MASTER)),grep("Age",names(MASTER)),grep("Sex",names(MASTER)),grep(PREFIX,names(MASTER)))]
		names(DF)<-gsub("DEMO_","",names(DF))
	}
	write.csv(DF,paste0(ROOT,"/Datasets/Preproc2/n",nrow(DF),"x",ncol(DF),"_",FILENAME,".csv"),row.names=FALSE)
}

######
### Load Data Sets
######

files <- list.files(path = paste0(ROOT,"/datasets/Preproc1"), pattern = "*.csv$", full.names = TRUE)
load_csv_files <- function(file){
	label <- tail(unlist(strsplit(gsub(".csv","",basename(file)),"_")),1)
	data <- read.csv(file)
}; features <- lapply(files, load_csv_files)

######
### Preprocess Datasets Sets
######

DICT <- data.frame(read_excel(paste0(ROOT,"/datasets/Preproc1/Dictionary_1424x16.xlsx")))
DICT <- DICT[which(DICT$Inclusion!="Exclude"),]
TARGETS<-Preproc2("TARGET","targets") 
DEMO<-Preproc2("DEMO","demographics")
ENVIR<-Preproc2("ENVIR","environmental")
FAM<-Preproc2("FAM","family")
SOC<-Preproc2("SOC","social")
BEHAV<-Preproc2("BEHAV","behavior") 
STRSS<-Preproc2("STRSS","adversity") 
COG<-Preproc2("COG","cognition") 
SMRI<-Preproc2("SMRI","mri_anat")
fMRI<-Preproc2("fMRI","mri_func")
dMRI<-Preproc2("dMRI","mri_dwi")

######
### Reduce Redundant Features & Merge into Master
######

stp_numbers<-setdiff(1:9,7) 
for (i in stp_numbers){
  wkdy_var <- paste0("BEHAV_stp_", i, "_wkdy_hr_p")
  wkend_var <- paste0("BEHAV_stp_", i, "_wkend_hr_p")
  new_var <- paste0("BEHAV_stp_", i, "_wk_hr_p")
  BEHAV[[new_var]] <- BEHAV[[wkdy_var]] + BEHAV[[wkend_var]]
}
BEHAV <- BEHAV[, !names(BEHAV) %in% c(paste0("BEHAV_stp_", stp_numbers, "_wkdy_hr_p"))]
BEHAV <- BEHAV[, !names(BEHAV) %in% c(paste0("BEHAV_stp_", stp_numbers, "_wkend_hr_p"))]
data_frames <- list(TARGETS, DEMO, ENVIR, FAM, SOC, BEHAV, STRSS, COG, SMRI, fMRI, dMRI)
MASTER <- Reduce(function(x, y) merge(x, y, by = c("src_subject_id","eventname"), all = TRUE), data_frames)
MASTER$eventname <- factor(MASTER$eventname, levels=LEVELS, ordered=TRUE)
MASTER<-MASTER[!is.na(MASTER$TARGET_bpm_totalprob_r),]

######
### Evaluate Completeness
######

ROWS<-as.data.frame(100-(rowSums(is.na(MASTER))/dim(MASTER)[2]*100)); names(ROWS)<-"V1"
ROWS[which(ROWS$V1<75),"V2"]<-"Incomplete"; ROWS[which(ROWS$V1>75),"V2"]<-"Complete"
FIGURE1<-ggplot(ROWS, aes(x=V1, fill=V2)) +
			geom_histogram(bins = 50, alpha = 0.80) + 
			xlim(0, 100) +
			geom_vline(xintercept = 75, color="black")+
			xlab("Percent Complete") +
			scale_fill_manual(values=c("darkgreen","darkred")) +
			theme(legend.position = "none") +
			theme(strip.background = element_rect(colour = "white", fill = "black"))
ggsave(plot=FIGURE1,filename=paste0(ROOT,'/Figures/',TODAY,'_CompleteSubj.pdf'),device="pdf",width=14,height=7.5,units='in')

COLS<-as.data.frame(100-(colSums(is.na(MASTER))/dim(MASTER)[1]*100)); names(COLS)<-"V1"
COLS[which(COLS$V1<75),"V2"]<-"Incomplete"; COLS[which(COLS$V1>75),"V2"]<-"Complete"
theme_set(theme_bw(base_size = 13))
FIGURE2<-ggplot(COLS, aes(x=V1, fill=V2)) +
			xlim(0, 100) +
			geom_histogram(bins = 50, alpha = 0.80) + 
			geom_vline(xintercept = 75, color="black")+
			xlab("Percent Complete") +
			scale_fill_manual(values=c("darkgreen","darkred")) +
			theme(legend.position = "none") +
			theme(strip.background = element_rect(colour = "white", fill = "black"))
ggsave(plot=FIGURE2,filename=paste0(ROOT,'/Figures/',TODAY,'_CompleteFeat.pdf'),device="pdf",width=14,height=7.5,units='in')

######
### Save Feature Spaces
######

SaveFeatSpace("DEMO","demographics")
SaveFeatSpace("ENVIR","environmental")
SaveFeatSpace("FAM","family")
SaveFeatSpace("SOC","social")
SaveFeatSpace("BEHAV","behavior") 
SaveFeatSpace("STRSS","adversity") 
SaveFeatSpace("COG","cognition") 
SaveFeatSpace("SMRI","mri_anat")
SaveFeatSpace("fMRI","mri_func")
SaveFeatSpace("dMRI","mri_dwi")
write.csv(MASTER,paste0(ROOT,"/datasets/Preproc2/n",nrow(MASTER),"x",ncol(MASTER),"_multimodal.csv"),row.names=FALSE)

######
### Save Data Dictionary
######

PRE<-data.frame(read_excel(paste0(ROOT,"/datasets/Preproc1/Dictionary_1424x16.xlsx")))
DICT<-as.data.frame(names(MASTER)[3:ncol(MASTER)])
DICT<-separate(DICT, names(DICT)[1],into = c("Set", "Feature"), sep = "_", extra = "merge")
for (feat in names(MASTER)[3:ncol(MASTER)]){
	FEAT=paste(unlist(strsplit(feat,"_"))[-1], collapse = "_")
	if (length(unique(na.omit(MASTER[,feat]))) >= 3){
		DICT[which(DICT$Feature == FEAT),"VarType"]<-'Numerical'
	} else if (length(unique(na.omit(MASTER[,feat]))) == 2){
		DICT[which(DICT$Feature == FEAT),"VarType"]<-'Categorical'
	} else {
		DICT[which(DICT$Feature == FEAT),"VarType"]<-NA			
	}
	DICT[which(DICT$Feature == FEAT),"StandDev"]<-sd(MASTER[,feat], na.rm = TRUE)
	for (timepoint in sort(unique(MASTER$eventname))){
		subset<-MASTER[which(MASTER$eventname==timepoint),]
		NACount<-length(which(is.na(subset[,feat])==FALSE))
		NAPrecent<-round(NACount/dim(subset)[1]*100, digits=2)
		DICT[which(DICT$Feature == FEAT),timepoint]<-NAPrecent
	}
}
DICT$Completeness<-rowMeans(DICT[,c("baseline_year_1_arm_1","2_year_follow_up_y_arm_1","4_year_follow_up_y_arm_1")])
DICT$Transposed<-"No"; DICT<-merge(DICT, PRE[,c("Set","Feature","Invariant")], by=c("Set","Feature"), all.x = TRUE)
DICT<-merge(DICT, ABCD, by=c("Feature"), all.x = TRUE); DICT<-DICT[,c(2,1,3:ncol(DICT))]
DICT$Set<-factor(DICT$Set,levels=c("DEMO","BEHAV","COG","ENVIR","FAM","SOC","STRSS","SMRI","fMRI","dMRI","TARGET"))
DICT<-DICT[!duplicated(DICT),]; DICT<-DICT[order(DICT$Set),]
write_xlsx(DICT, paste0(ROOT,"/datasets/Preproc2/Dictionary_",nrow(DICT),"x",ncol(DICT),".xlsx"))

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######