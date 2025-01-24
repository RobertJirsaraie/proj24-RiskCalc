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
	"1_year_follow_up_y_arm_1",
	"2_year_follow_up_y_arm_1",
	"3_year_follow_up_y_arm_1",
	"4_year_follow_up_y_arm_1"
)

Extrapolate <- function(DATA, FEAT, DIRECTION){
	DF<-DATA[,c("src_subject_id","eventname",FEAT)]
	DF<-pivot_wider(DF,
		id_cols=src_subject_id,
		names_from=eventname,
		values_from=FEAT)
	DF<-as.data.frame(DF[,c("src_subject_id",LEVELS)])
	if (DIRECTION == "Retroactive"){
		print(paste0("Retroactively Transposing: ",FEAT))
	  	DF<-mutate(DF, `baseline_year_1_arm_1` = coalesce(`baseline_year_1_arm_1`, `1_year_follow_up_y_arm_1`))
	  	DF<-mutate(DF, `2_year_follow_up_y_arm_1` = coalesce(`2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`))
  	} else if (DIRECTION == "Prospective"){
  		print(paste0("Prospectively Transposing: ",FEAT))
	  	DF<-mutate(DF, `2_year_follow_up_y_arm_1` = coalesce(`2_year_follow_up_y_arm_1`, `1_year_follow_up_y_arm_1`))
	  	DF<-mutate(DF, `4_year_follow_up_y_arm_1` = coalesce(`4_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`))
  	} else if (DIRECTION == "Both"){
  		print(paste0("Retroactively & Prospectively Transposing: ",FEAT))  		
	  	DF<-mutate(DF, `baseline_year_1_arm_1` = coalesce(`baseline_year_1_arm_1`, `1_year_follow_up_y_arm_1`))
	  	DF<-mutate(DF, `4_year_follow_up_y_arm_1` = coalesce(`4_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`))
  	}
	DF<-pivot_longer(DF,
		cols=c(2:ncol(DF)),
		names_to="eventname",
		values_to=paste0(FEAT,"_TRANSPOSED"))
	DATA<-merge(DATA,DF,by=c("src_subject_id","eventname"))
	DATA[,FEAT]<-DATA[,paste0(FEAT,"_TRANSPOSED")]
	DATA<-DATA[,!grepl("_TRANSPOSED",names(DATA))]
	return(DATA)
}

Preproc3 <- function(PREFIX, FILENAME){
	VARIABLES<-paste0(PREFIX,"_",DICT[which(DICT$Set==PREFIX),"Feature"])
	TRANSPOSE<-DICT[which(DICT$Set==PREFIX & DICT$Transposed!="No"),]
	INVARIANT<-DICT[which(DICT$Set==PREFIX & DICT$Invariant=="Yes"),"Feature"]
	DATA<-as.data.frame(features[grep(FILENAME, files)])
	DATA<-DATA[,c("src_subject_id","eventname",VARIABLES)]
	DATA<-DATA[which(DATA$eventname %in% LEVELS),]
	DATA$eventname<-factor(DATA$eventname,levels=LEVELS)
	DATA[DATA == 777] <- NA; DATA[DATA == 999] <- NA
	if (nrow(TRANSPOSE) > 0 && FILENAME != "targets"){
		for (ROW in 1:nrow(TRANSPOSE)){
			FEAT<-paste0(TRANSPOSE[ROW,"Set"],"_",TRANSPOSE[ROW,"Feature"])
			DATA<-Extrapolate(DATA, FEAT, TRANSPOSE[ROW,"Transposed"])
		}
	}
	for (INVAR in INVARIANT){
		print(paste0("Transforming Non-Time-Varying Variable: ", INVAR))
		for (SUBID in unique(DATA$src_subject_id)){
			VALS<-DATA[DATA$src_subject_id==SUBID,paste0(PREFIX,"_",INVAR)]
			DATA[DATA$src_subject_id==SUBID,paste0(PREFIX,"_",INVAR)]<-na.omit(VALS)[1]
		}
	}
	return(DATA)
}

SaveFeatSpace <- function(SUBDIRECT, PREFIX, FILENAME){
	if (!grepl("MRI", PREFIX)){
		DF<-MASTER[,COL<-c(1:2,grep("TARGET",names(MASTER)),grep(PREFIX,names(MASTER)))]
	} else {
		DF<-MASTER[,c(1:2,grep("TARGET",names(MASTER)),grep("Age",names(MASTER)),grep("Sex",names(MASTER)),grep(PREFIX,names(MASTER)))]
		names(DF)<-gsub("DEMO_","",names(DF))
	}
	write.csv(DF,paste0(ROOT,"/datasets/",SUBDIRECT,"/n",nrow(DF),"x",ncol(DF),"_",FILENAME,".csv"),row.names=FALSE)
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

DICT<-data.frame(read_excel(paste0(ROOT,"/datasets/Preproc1/Dictionary_1424x16.xlsx")))
DICT<-DICT[which(DICT$Inclusion!="Exclude"),]
TARGETS<-Preproc3("TARGET","targets") 
DEMO<-Preproc3("DEMO","demographics")
ENVIR<-Preproc3("ENVIR","environmental")
FAM<-Preproc3("FAM","family") 
SOC<-Preproc3("SOC","social")
BEHAV<-Preproc3("BEHAV","behavior") 
STRSS<-Preproc3("STRSS","adversity") 
COG<-Preproc3("COG","cognition") 
SMRI<-Preproc3("SMRI","mri_anat")
fMRI<-Preproc3("fMRI","mri_func")
dMRI<-Preproc3("dMRI","mri_dwi")

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
DICT<-DICT[!grepl("_wkend_hr_p", DICT$Feature),]
DICT$Feature<-gsub("_wkdy_hr_p","_wk_hr_p", DICT$Feature)
data_frames<-list(TARGETS, DEMO, ENVIR, FAM, SOC, BEHAV, STRSS, COG, SMRI, fMRI, dMRI)
MASTER<-Reduce(function(x, y) merge(x, y, by = c("src_subject_id","eventname"), all = TRUE), data_frames)
LVLS <- c("baseline_year_1_arm_1","2_year_follow_up_y_arm_1","4_year_follow_up_y_arm_1")
MASTER$eventname <- factor(MASTER$eventname, levels=LVLS, ordered=TRUE)
MASTER<-MASTER[!is.na(MASTER$TARGET_bpm_totalprob_r),]

######
### Evaluate Completeness
######

ROWS<-as.data.frame(100-(rowSums(is.na(MASTER))/dim(MASTER)[2]*100)); names(ROWS)<-"V1"
ROWS[which(ROWS$V1<70),"V2"]<-"Incomplete"; ROWS[which(ROWS$V1>70),"V2"]<-"Complete"
FIGURE1<-ggplot(ROWS, aes(x=V1, fill=V2)) +
			geom_histogram(bins = 50, alpha = 0.80) + 
			geom_vline(xintercept = 70, color="black")+
			xlab("Percent Complete") +
			scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by=10)) + 
			scale_fill_manual(values=c("darkgreen","darkred")) +
			theme(legend.position = "none") +
			theme(strip.background = element_rect(colour = "white", fill = "black"))
ggsave(plot=FIGURE1,filename=paste0(ROOT,'/Figures/Preproc3_CompleteSubj.pdf'),device="pdf",width=14,height=7.5,units='in')

COLS<-as.data.frame(100-(colSums(is.na(MASTER))/dim(MASTER)[1]*100)); names(COLS)<-"V1"
COLS[which(COLS$V1<70),"V2"]<-"Incomplete"; COLS[which(COLS$V1>70),"V2"]<-"Complete"
theme_set(theme_bw(base_size = 13))
FIGURE2<-ggplot(COLS, aes(x=V1, fill=V2)) +
			geom_histogram(bins = 50, alpha = 0.80) + 
			geom_vline(xintercept = 70, color="black")+
			xlab("Percent Complete") +
			scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by=10)) + 
			scale_fill_manual(values=c("darkgreen","darkred")) +
			theme(legend.position = "none") +
			theme(strip.background = element_rect(colour = "white", fill = "black"))
ggsave(plot=FIGURE2,filename=paste0(ROOT,'/Figures/Preproc3_CompleteFeat.pdf'),device="pdf",width=14,height=7.5,units='in')

######
### Save Feature Spaces
######

SaveFeatSpace("Preproc3","DEMO","demographics")
SaveFeatSpace("Preproc3","ENVIR","environmental")
SaveFeatSpace("Preproc3","FAM","family")
SaveFeatSpace("Preproc3","BEHAV","behavior") 
SaveFeatSpace("Preproc3","STRSS","adversity") 
SaveFeatSpace("Preproc3","COG","cognition") 
SaveFeatSpace("Preproc3","SMRI","mri_anat")
SaveFeatSpace("Preproc3","fMRI","mri_func")
SaveFeatSpace("Preproc3","dMRI","mri_dwi")
write.csv(MASTER,paste0(ROOT,"/datasets/Preproc3/n",nrow(MASTER),"x",ncol(MASTER),"_multimodal.csv"),row.names=FALSE)

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
	DICT[which(DICT$Feature == FEAT),"StandDev"]<-round(sd(MASTER[,feat], na.rm = TRUE),digits=2)
	DICT[which(DICT$Feature == FEAT),"Range"]<-round(diff(range(MASTER[,feat],na.rm = TRUE)),digits=2)
	for (timepoint in sort(unique(MASTER$eventname))){
		subset<-MASTER[which(MASTER$eventname==timepoint),]
		Count<-length(which(is.na(subset[,feat])==FALSE))
		Percent<-round(Count/dim(subset)[1]*100, digits=2)
		DICT[which(DICT$Feature == FEAT),timepoint]<-Percent
	}
	Count<-length(which(is.na(MASTER[,feat])==FALSE))
	Percent<-round(Count/dim(MASTER)[1]*100, digits=2)
	DICT[which(DICT$Feature == FEAT),"Completeness"]<-Percent
}

DICT<-merge(DICT, PRE[,c("Set","Feature","Transposed","Invariant")],by=c("Set","Feature"), all.x=TRUE)
DICT<-merge(DICT, ABCD, by=c("Feature"), all.x = TRUE); DICT<-DICT[,c(2,1,3:ncol(DICT))]
DICT$Set<-factor(DICT$Set,levels=c("DEMO","BEHAV","COG","ENVIR","FAM","SOC","STRSS","SMRI","fMRI","dMRI","TARGET"))
DICT<-DICT[!duplicated(DICT),]; DICT<-DICT[order(DICT$Set),]
write_xlsx(DICT, paste0(ROOT,"/datasets/Preproc3/Dictionary_",nrow(DICT),"x",ncol(DICT),".xlsx"))

######
### Visualize Completness by Target Variable
######

DF2<-read.csv(paste0(ROOT,"/datasets/Preproc2/n24838x1119_multimodal.csv"))
DF2$Complete<-(100-(rowSums(is.na(DF2))/dim(DF2)[2]*100)); DF2$preproc<-2
DF2<-DF2[,c("src_subject_id","eventname","TARGET_bpm_totalprob_r","Complete","preproc")]
DF3<-read.csv(paste0(ROOT,"/datasets/Preproc3/n24838x1119_multimodal.csv"))
DF3$Complete<-(100-(rowSums(is.na(DF3))/dim(DF3)[2]*100)); DF3$preproc<-3
DF3<-DF3[,c("src_subject_id","eventname","TARGET_bpm_totalprob_r","Complete","preproc")]
DF<-rbind(DF2, DF3); DF$preproc<-as.factor(DF$preproc)
F1<-ggplot() + 
        geom_point(data=DF, aes(x=Complete, y=TARGET_bpm_totalprob_r),color="#000000", alpha=0.2, size=1.5) +
        geom_point(data=DF, aes(x=Complete, y=TARGET_bpm_totalprob_r, color=preproc), alpha=0.2, size=0.75) +
        geom_smooth(data=DF, aes(x=Complete, y=TARGET_bpm_totalprob_r, group=preproc),color="#000000", alpha=1, fullrange=TRUE, method=lm, se=FALSE, linewidth=4) + 
        geom_smooth(data=DF, aes(x=Complete, y=TARGET_bpm_totalprob_r, color=preproc), alpha=1, fullrange=TRUE, method=lm, se=FALSE, linewidth=3) + 
        theme(strip.background = element_rect(colour = "black", fill = "black")) +
        theme(strip.text = element_text(color="white", face="bold")) + 
        ylab("P-Factor Scores") + xlab("Percent Complete") + 
        scale_color_manual(values = c("#9e1405", "#03871f")) + 
        facet_wrap(~preproc) + theme(legend.position="none")
ggsave(plot=F1,filename=paste0(ROOT,"/F1_",{TODAY},".png"),device="png",width=14,height=7.5,units='in')

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######