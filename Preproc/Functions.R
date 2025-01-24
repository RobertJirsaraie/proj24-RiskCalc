#!/usr/bin/env Rscript
######################

invisible(lapply(c("stringr","tidyr","plyr","dplyr","tidyverse","ggplot2"), require, character.only=TRUE))
ROOT<-"/Users/robertjirsaraie/Library/CloudStorage/Box-Box/Research/proj24-RiskCalculator"

LoadFeatures <- function(TABLE_NAMES){
	FILES<-list.files(path = paste0(ROOT,"/datasets/ABCD/"), pattern = "*.csv$", recursive=TRUE, full.names = TRUE)
	TABLE_FILES<-FILES[grepl(paste(TABLE_NAMES, collapse = "|"), FILES)]
	TABLE_DFS<-lapply(TABLE_FILES, read.csv)
	DATA <- reduce(TABLE_DFS, ~ full_join(.x, .y, by = c("src_subject_id","eventname")))
	DATA <- DATA %>% mutate(across(everything(), ~ ifelse(. %in% c(777, 999), NA, .)))
	for (COL in grep("_l$",names(DATA),value=TRUE)){
		if (gsub("_l$","",COL) %in% names(DATA)){
			TP1<-gsub("_l$","",COL)
			DATA[,TP1]<-ifelse(!is.na(DATA[,TP1]), DATA[,TP1], DATA[,COL])
			DATA[,COL]<-NULL
		}
	}
	write.csv(GUIDE[which(GUIDE$var_name %in% names(DATA)),c(2,3)],"~/Desktop/latest.csv",row.names=F)
	return(as.data.frame(DATA))
}


GUIDE[which(GUIDE$var_name %in% names(DATA)),c(2,3)]

DataDictionary <- function(data, SUBDIRECT, PREFIX){
	DATA <- as.data.frame(names(data)[3:ncol(data)])
	data$eventname <- factor(data$eventname, levels=LEVELS, ordered=TRUE)
	DATA <- separate(DATA, names(DATA)[1],into = c("Set", "Feature"), sep = "_", extra = "merge")
	for (feat in  names(data)[3:ncol(data)]){
		FEAT=paste(unlist(strsplit(feat,"_"))[-1], collapse = "_")
		if (length(unique(na.omit(data[,feat]))) >= 3){
			DATA[which(DATA$Feature == FEAT),"VarType"]<-'Numerical'
		} else if (length(unique(na.omit(data[,feat]))) == 2){
			DATA[which(DATA$Feature == FEAT),"VarType"]<-'Categorical'
		} else {
			DATA[which(DATA$Feature == FEAT),"VarType"]<-NA			
		}
		for (timepoint in sort(unique(data$eventname))){
			subset<-data[which(data$eventname==timepoint),]
			NACount<-length(which(is.na(subset[,feat])==FALSE))
			NAPrecent<-round(NACount/dim(subset)[1]*100, digits=2)
			DATA[which(DATA$Feature == FEAT),timepoint]<-NAPrecent
		}
	}
	DATA$Completeness<-rowMeans(DATA[,c("baseline_year_1_arm_1","2_year_follow_up_y_arm_1","4_year_follow_up_y_arm_1")])
	SAVE<-paste0(ROOT,"/Datasets/",SUBDIRECT,"/",PREFIX,"_",nrow(DATA),"x",ncol(DATA),"_",TODAY,".csv")
	DATA$Transposed<-0; DATA$Invariant<-0; DATA$Description<-NA
	print(paste0("Saving Data: ",SAVE))
	dir.create(dirname(SAVE), showWarnings = FALSE)
	write.csv(DATA, SAVE, row.names=F)
	return(DATA)
}

Extrapolate <- function(DATA, FEAT, DIRECTION){
	DF<-DATA[,c("src_subject_id","eventname",FEAT)]
	DF<-pivot_wider(DF, id_cols=src_subject_id, names_from=eventname, values_from=FEAT)
	DF<-as.data.frame(DF[,c("src_subject_id",LEVELS[which(LEVELS %in% names(DF))])])
	if (DIRECTION == "Retroactive"){
		print(paste0("Retroactively Transposing: ", FEAT))
	  	DF<-mutate(DF, `baseline_year_1_arm_1` = coalesce(`baseline_year_1_arm_1`, `1_year_follow_up_y_arm_1`))
	  	DF<-mutate(DF, `2_year_follow_up_y_arm_1` = coalesce(`2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`))
  	} else if (DIRECTION == "Prospective"){
  		print(paste0("Prospectively Transposing: ", FEAT))
	  	DF<-mutate(DF, `2_year_follow_up_y_arm_1` = coalesce(`2_year_follow_up_y_arm_1`, `1_year_follow_up_y_arm_1`))
	  	DF<-mutate(DF, `4_year_follow_up_y_arm_1` = coalesce(`4_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`))
  	} else if (DIRECTION == "Both"){
  		print(paste0("Retroactively & Prospectively Transposing: ", FEAT))  		
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

Preproc4 <- function(PREFIX, DATA){
	VARIABLES<-paste0(PREFIX,"_",DICT[which(DICT$Set==PREFIX),"Feature"])
	TRANSPOSE<-DICT[which(DICT$Set==PREFIX & DICT$Transposed!="No"),]
	INVARIANT<-DICT[which(DICT$Set==PREFIX & DICT$Invariant=="Yes"),"Feature"]
	DATA<-DATA[,c("src_subject_id","eventname",VARIABLES)]
	DATA<-DATA[which(DATA$eventname %in% LEVELS),]
	DATA$eventname<-factor(DATA$eventname,levels=LEVELS)
	DATA[DATA == 777] <- NA; DATA[DATA == 999] <- NA
	if (nrow(TRANSPOSE) > 0 && PREFIX != "TARGET"){
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
	write.csv(DF,paste0(ROOT,"/Datasets/",SUBDIRECT,"/n",nrow(DF),"x",ncol(DF),"_",FILENAME,".csv"),row.names=FALSE)
}

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
