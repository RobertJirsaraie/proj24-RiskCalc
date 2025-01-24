#!/usr/bin/env Rscript
######################

invisible(lapply(c("stringr","tidyr","plyr","dplyr","tidyverse","ggplot2"), require, character.only=TRUE))
ROOT<-"/Users/robertjirsaraie/Library/CloudStorage/Box-Box/Research/proj24-RiskCalculator"
GUIDE<-read.csv(paste0(ROOT,"/datasets/ABCD/ABCD_Dictionary.csv"))
GUIDE<-GUIDE[,c('table_name',"var_name","var_label","notes")]
LEVELS<-c(
	"baseline_year_1_arm_1",
	"6_month_follow_up_arm_1",
	"1_year_follow_up_y_arm_1",
	"18_month_follow_up_arm_1",
	"2_year_follow_up_y_arm_1",
	"30_month_follow_up_arm_1",
	"3_year_follow_up_y_arm_1",
	"42_month_follow_up_arm_1",
	"4_year_follow_up_y_arm_1"
)

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

######
### Mental Health Targets
######

FILES<-list.files(path = paste0(ROOT,"/datasets/ABCD/"), pattern = "*.csv$", recursive=TRUE, full.names = TRUE)
BPM<-read.csv(FILES[grep("mh_y_bpm",FILES)])
BPM<-BPM[!is.na(BPM$bpm_y_scr_totalprob_r),]; BPM<-BPM[!is.na(BPM$bpm_y_scr_internal_r),]
BPM<-BPM[,c("src_subject_id","eventname","bpm_y_scr_totalprob_r","bpm_y_scr_internal_r","bpm_y_scr_external_r","bpm_y_scr_attention_r")]
BPM[which(BPM$eventname=="6_month_follow_up_arm_1"),"eventname"]<-"baseline_year_1_arm_1"
BPM<-BPM[BPM$eventname %in% c("baseline_year_1_arm_1", "2_year_follow_up_y_arm_1","4_year_follow_up_y_arm_1"),]
names(BPM)<-gsub("bpm_y_scr_","TARGET_bpm_",names(BPM))
write.csv(BPM,paste0(ROOT,"/datasets/Preproc1/n",nrow(BPM),"x",ncol(BPM),"_targets.csv"),row.names=FALSE)

######
### Demographics Features
######

DEMOGRAPHICS<-LoadFeatures(c(
	"abcd_y_lt",
	"abcd_p_demo",
	"ce_y_meim",
	"ce_y_via",
	"gish_p_sex",
	"gish_p_gi"
))

DEMOGRAPHICS$DEMO_Age<-as.numeric(DEMOGRAPHICS$interview_age)/12
DEMOGRAPHICS$DEMO_Male<-ifelse(DEMOGRAPHICS$demo_sex_v2.x == 1, 1, 0)
DEMOGRAPHICS$DEMO_RaceWhite<-ifelse(DEMOGRAPHICS$demo_race_a_p___10 == 1, 1, 0)
DEMOGRAPHICS$DEMO_RaceNative<-ifelse(DEMOGRAPHICS$demo_race_a_p___10 == 1, 1, 0)
DEMOGRAPHICS$DEMO_RaceBlack<-ifelse(DEMOGRAPHICS$demo_race_a_p___11 == 1, 1, 0)
DEMOGRAPHICS$DEMO_RaceNative<-ifelse(DEMOGRAPHICS$demo_race_a_p___12 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___13 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___14 == 1 |
                                    DEMOGRAPHICS$demo_race_a_p___15 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___16 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___17 == 1, 1, 0)
DEMOGRAPHICS$DEMO_RaceAsian<-ifelse(DEMOGRAPHICS$demo_race_a_p___18 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___19 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___20 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___21 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___22 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___23 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___24 == 1, 1, 0)
DEMOGRAPHICS$DEMO_RaceNative<-ifelse(DEMOGRAPHICS$demo_race_a_p___12 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___13 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___14 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___15 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___16 == 1 | 
                                    DEMOGRAPHICS$demo_race_a_p___17 == 1, 1, 0)
DEMOGRAPHICS$DEMO_RaceOther <- ifelse(DEMOGRAPHICS$demo_race_a_p___25 == 1, 1, 0)
DEMOGRAPHICS$DEMO_RaceHispanic <- ifelse(DEMOGRAPHICS$demo_ethn_v2 == 1, 1, 0)
DEMOGRAPHICS$DEMO_Immigrant <- ifelse(DEMOGRAPHICS$demo_origin_v2 != 189, 1, 0)
DEMOGRAPHICS$DEMO_FirstGen <- ifelse(DEMOGRAPHICS$demo_prnt_origin_v2 != 189 | 
                                    DEMOGRAPHICS$demo_biofather_v2 != 189 | 
                                    DEMOGRAPHICS$demo_biomother_v2 != 189, 1, 0)
DEMOGRAPHICS$DEMO_Catholic <- ifelse(DEMOGRAPHICS$demo_relig_v2 == 4, 1, 0) 
DEMOGRAPHICS$DEMO_Jewish <- ifelse(DEMOGRAPHICS$demo_relig_v2 == 5, 1, 0)
DEMOGRAPHICS$DEMO_Mormon <- ifelse(DEMOGRAPHICS$demo_relig_v2 == 6, 1, 0)
DEMOGRAPHICS$DEMO_Jahovahs <- ifelse(DEMOGRAPHICS$demo_relig_v2 == 7, 1, 0)
DEMOGRAPHICS$DEMO_Muslim <- ifelse(DEMOGRAPHICS$demo_relig_v2 == 8, 1, 0)
DEMOGRAPHICS$DEMO_Hindu <- ifelse(DEMOGRAPHICS$demo_relig_v2 == 10, 1, 0)
DEMOGRAPHICS$DEMO_Christian <- ifelse(DEMOGRAPHICS$demo_relig_v2 == 13, 1, 0)
DEMOGRAPHICS$DEMO_Atheist <- ifelse(DEMOGRAPHICS$demo_relig_v2 == 14, 1, 0)
DEMOGRAPHICS$DEMO_Religious <- ifelse(DEMOGRAPHICS$demo_relig_v2 != 14, 1, 0)
DEMOGRAPHICS$DEMO_Adopted <- ifelse(!is.na(DEMOGRAPHICS$demo_adopt_agex_v2), 1, 0)
DEMOGRAPHICS$DEMO_ParentMarried <- ifelse(DEMOGRAPHICS$demo_prnt_marital_v2 == 1, 1, 0)
DEMOGRAPHICS$DEMO_ParentDivorce <- ifelse(DEMOGRAPHICS$demo_prnt_marital_v2 == 3, 1, 0)
DEMOGRAPHICS$DEMO_ParentSeparate <- ifelse(DEMOGRAPHICS$demo_prnt_marital_v2 == 4, 1, 0)
DEMOGRAPHICS$DEMO_OutofWedLock <- ifelse(DEMOGRAPHICS$demo_prnt_marital_v2 == 5, 1, 0)
DEMOGRAPHICS$DEMO_StepParent <- ifelse(DEMOGRAPHICS$demo_prnt_marital_v2 == 6, 1, 0)
DEMOGRAPHICS$DEMO_SingleParent <- ifelse(DEMOGRAPHICS$demo_prnt_prtnr_v2 == 2, 1, 0)
DEMOGRAPHICS[,"DEMO_ParentEducation"]<-as.numeric(DEMOGRAPHICS$demo_prnt_ed_v2)
DEMOGRAPHICS[,"DEMO_ParentIncome"]<-as.numeric(DEMOGRAPHICS$demo_prnt_income_v2)
DEMOGRAPHICS[,"DEMO_FamilyIncome"]<-as.numeric(DEMOGRAPHICS$demo_comb_income_v2)
DEMOGRAPHICS[,"DEMO_FamilySize"]<-as.numeric(DEMOGRAPHICS$demo_roster_v2)
DEMOGRAPHICS[which(DEMOGRAPHICS$DEMO_FamilySize > 10),"DEMO_FamilySize"]<-10
DEMOGRAPHICS[,"DEMO_Homosexual"]<-ifelse(DEMOGRAPHICS$kbi_p_c_gay == 4, NA, DEMOGRAPHICS$kbi_p_c_gay)
DEMOGRAPHICS[,"DEMO_Transexual"]<-ifelse(DEMOGRAPHICS$kbi_p_c_trans == 4, NA, DEMOGRAPHICS$kbi_p_c_trans)
DEMOGRAPHICS[,"DEMO_Homosexual"]<-ifelse(DEMOGRAPHICS$kbi_p_c_gay == 3, 0, 1)
DEMOGRAPHICS[,"DEMO_Transexual"]<-ifelse(DEMOGRAPHICS$kbi_p_c_trans == 3, 0, 1)
DEMOGRAPHICS[,"DEMO_ReligionServices"]<-as.numeric(DEMOGRAPHICS$demo_yrs_1)
DEMOGRAPHICS[,"DEMO_ReligionImportance"]<-as.numeric(DEMOGRAPHICS$demo_yrs_2)
DEMOGRAPHICS$DEMO_CultureUSA<-DEMOGRAPHICS$via_ss_amer
DEMOGRAPHICS$DEMO_CultureHeritage<-DEMOGRAPHICS$via_ss_hc
DEMOGRAPHICS$DEMO_meim_ss_explore<-DEMOGRAPHICS$meim_ss_exp
DEMOGRAPHICS$DEMO_meim_ss_commit<-DEMOGRAPHICS$meim_ss_com
DEMOGRAPHICS$DEMO_meim_ss_total<-DEMOGRAPHICS$meim_ss_total
for (COLNUM in grep("abcdmeim",names(DEMOGRAPHICS))){
	DEMOGRAPHICS[,paste0("DEMO_",names(DEMOGRAPHICS)[COLNUM])]<-DEMOGRAPHICS[,COLNUM]
}
DEMOGRAPHICS<-DEMOGRAPHICS[,c(1,2,grep("DEMO_",names(DEMOGRAPHICS)))]
write.csv(DEMOGRAPHICS,paste0(ROOT,"/datasets/Preproc1/n",nrow(DEMOGRAPHICS),"x",ncol(DEMOGRAPHICS),"_demographics.csv"),row.names=FALSE)

######
### Family Features
######

FAMILY<-LoadFeatures(c(
	"ce_p_fes",
	"ce_p_pm",
	"ce_p_macv",
	"ce_y_macv",
	"ce_y_crpbi",
	"ce_y_fes",
	"ce_y_mnbs",
	"ce_y_pet",
	"ce_y_pm",
	"mh_p_fhx",
	"mh_y_le"
))

FAMILY$FAM_Drug_HX <- ifelse(FAMILY$fam_history_5_yes_no == 1, 1, 0)
FAMILY$FAM_MDD_HX <- ifelse(FAMILY$fam_history_6_yes_no == 1, 1, 0)
FAMILY$FAM_Mania_HX <- ifelse(FAMILY$fam_history_7_yes_no == 1, 1, 0)
FAMILY$FAM_Schizo_HX <- ifelse(FAMILY$fam_history_8_yes_no == 1, 1, 0)
FAMILY$FAM_Violent_HX <- ifelse(FAMILY$fam_history_9_yes_no == 1, 1, 0)
FAMILY$FAM_Nerves_HX <- ifelse(FAMILY$fam_history_10_yes_no == 1, 1, 0)
FAMILY$FAM_General_HX <- ifelse(FAMILY$fam_history_11_yes_no == 1, 1, 0)
FAMILY$FAM_Hosp_HX <- ifelse(FAMILY$fam_history_12_yes_no == 1, 1, 0)
FAMILY$FAM_Suicide_HX <- ifelse(FAMILY$fam_history_13_yes_no == 1, 1, 0)
FAMILY$FAM_MDD_HX_Father <- ifelse(FAMILY$fam_history_q5d_drugs___0 == 0, 0, 1)
FAMILY$FAM_MDD_HX_Mother <- ifelse(FAMILY$fam_history_q5d_drugs___0 == 0, 0, 1)
FAMILY$FAM_MDD_HX_Father <- ifelse(FAMILY$fam_history_q6a_depression == 0, 0, 1)
FAMILY$FAM_MDD_HX_Mother <- ifelse(FAMILY$fam_history_q6d_depression == 0, 0, 1)
FAMILY$FAM_MDD_HX_Mother <- ifelse(FAMILY$fam_history_q13a_suicide == 0, 0, 1)
FAMILY$FAM_MDD_HX_Mother <- ifelse(FAMILY$fam_history_q13d_suicide == 0, 0, 1)
FAMILY$FAM_Alcohol_hx<-abs(FAMILY$famhx_ss_parent_alc_p)
FAMILY$FAM_Drugs_hx<-abs(FAMILY$famhx_ss_parent_dg_p)
FAMILY$FAM_Mania_hx<-abs(FAMILY$famhx_ss_parent_ma_p)
FAMILY$FAM_MDD_hx<-abs(FAMILY$famhx_ss_parent_dprs_p)
FAMILY$FAM_Paranoia_hx<-abs(FAMILY$famhx_ss_parent_vs_p)
FAMILY$FAM_Trouble_hx<-abs(FAMILY$famhx_ss_parent_trb_p)
FAMILY$FAM_Nerves_hx<-abs(FAMILY$famhx_ss_parent_nrv_p)
FAMILY$FAM_Doctor_hx<-abs(FAMILY$famhx_ss_parent_prf_p)
FAMILY$FAM_Hospital_hx<-abs(FAMILY$famhx_ss_parent_hspd_p)
FAMILY$FAM_Suicide_hx<-abs(FAMILY$famhx_ss_parent_scd_p)
KEEP<-grep("fam_enviro",names(FAMILY))
KEEP<-c(KEEP,grep("^fes_youth_q", names(FAMILY)))
KEEP<-c(KEEP,grep("^crpbi_parent", names(FAMILY)))
KEEP<-c(KEEP,grep("^macv_q", names(FAMILY)))
KEEP<-c(KEEP,grep("^mnbs_ss_", names(FAMILY)))
KEEP<-c(KEEP,grep("^fes.*_ss_.*sum$", names(FAMILY)))
KEEP<-c(KEEP,grep("^mex_american.*_p$", names(FAMILY)))
KEEP<-c(KEEP,grep("parental_monitor_ss_mean", names(FAMILY)))
KEEP<-c(KEEP,grep("^macv_.*ss(?!.*(_nm|_nt)$)", names(FAMILY), perl = TRUE))
KEEP<-c(KEEP,grep("^crpbi_.*ss(?!.*(_nm|_nt)$)", names(FAMILY), perl = TRUE))
KEEP<-c(KEEP,grep("^fes_y_ss(?!.*(_nm|_nt|_na)$)", names(FAMILY), perl = TRUE))
KEEP<-c(KEEP,grep("^pmq_y_ss(?!.*(_nm|_nt|_na)$)", names(FAMILY), perl = TRUE))
KEEP<-c(KEEP,grep("^ple_y_ss(?!.*(_nm|_nt|_na)$)", names(FAMILY), perl = TRUE))
for (COLNUM in KEEP){
	FAMILY[,paste0("FAM_",names(FAMILY)[COLNUM])]<-FAMILY[,COLNUM]
}
FAMILY<-FAMILY[,c(1,2,grep("FAM_",names(FAMILY)))]
write.csv(FAMILY,paste0(ROOT,"/datasets/Preproc1/n",nrow(FAMILY),"x",ncol(FAMILY),"_family.csv"),row.names=FALSE); rm(FAMILY)

######
### Social Features
######

SOCIAL<-LoadFeatures(c(
	"ce_y_pnh",
	"ce_y_psb",
	"ce_p_psb",
	"gish_y_sex",
	"mh_p_ksads_bg",
	"mh_y_or",
	"mh_y_peq",
	"sd_y_pb",
	"sd_y_rd"
))

SOCIAL$SOC_pnh_ss_protective<-abs(SOCIAL$pnh_ss_protective_scale)
SOCIAL$SOC_Heterosexual <- ifelse(floor(SOCIAL$kbi_y_sex_orient) == 3, 1, 0)
SOCIAL$SOC_ParentFriend <- ifelse(SOCIAL$kbi_p_c_best_friend == 1, 1, 0)
SOCIAL$SOC_BestFriend <- ifelse(SOCIAL$kbi_p_c_best_friend == 1, 1, 0)
SOCIAL[is.na(SOCIAL$kbi_p_c_best_friend_len),'kbi_p_c_best_friend_len']<-0
SOCIAL$SOC_BFriendLen<-SOCIAL$kbi_p_c_best_friend_len
SOCIAL$SOC_FriendGrp<-ifelse(SOCIAL$kbi_p_c_reg_friend_group == 1, 1, 0)
SOCIAL[is.na(SOCIAL$kbi_p_c_reg_friend_group_len),'kbi_p_c_reg_friend_group_len']<-0
SOCIAL$SOC_GrpFriendLen<-SOCIAL$kbi_p_c_reg_friend_group_len
SOCIAL$SOC_GrpFriendUnapr<-SOCIAL$kbi_p_c_reg_friend_group_opin
KEEP<-grep("^gish_.*_y$", names(SOCIAL))
KEEP<-c(KEEP,grep("^peq_.*_perp$", names(SOCIAL)))
KEEP<-c(KEEP,grep("^peq_ss_(?!.*(admin|_nm|_nt|_na|victim)$)", names(SOCIAL), perl = TRUE))
for (VAR in grep("^socialdev_.*[0-9]$", names(SOCIAL),value=TRUE)){
	CONTENT<-SOCIAL[,VAR]
	CONTENT[which(CONTENT > 97)] <- NaN
	SOCIAL[,paste0("SOC_",gsub("social","",VAR))]<-ifelse(CONTENT == 0, 0, 1)
}
for (COLNUM in KEEP){
	SOCIAL[,paste0("SOC_",names(SOCIAL)[COLNUM])]<-SOCIAL[,COLNUM]
}
SOCIAL<-SOCIAL[,c(1,2,grep("SOC_",names(SOCIAL)))]
write.csv(SOCIAL,paste0(ROOT,"/datasets/Preproc1/n",nrow(SOCIAL),"x",ncol(SOCIAL),"_social.csv"),row.names=FALSE); rm(SOCIAL)

######
### Environmental Features
######

ENVIRONMENTAL<-LoadFeatures(c(
	"ce_p_comc",
	"ce_p_nsc",
	"ce_y_nsc",
	"ce_y_srpf",
	"ce_y_srpf",
	"abcd_y_lf",
	"led_l_seda_demo_s",
	"led_l_seda_demo_d",
	"led_l_nbhsoc",
	"led_l_densbld",
	"led_l_denspop",
	"led_l_walk",
	"led_l_crime",
	"led_l_leadrisk",
	"led_l_rentmort",
	"led_l_socmob",
	"led_l_ejscreen",
	"led_l_ice",
	"led_l_adi",
	"led_l_rxopioid"
))

ENVIRONMENTAL$comc_ss_cohesion_p<-rowMeans(ENVIRONMENTAL[,c("comc_phenx_close_knit_p", "comc_phenx_help_p", "comc_phenx_get_along_p", "comc_phenx_share_values_p", "comc_phenx_trusted_p")],na.rm=TRUE)
ENVIRONMENTAL$comc_ss_control_p<-rowMeans(ENVIRONMENTAL[,c("comc_phenx_skip_p", "comc_phenx_graffiti_p", "comc_phenx_disrespect_p", "comc_phenx_fight_p", "comc_phenx_budget_p")],na.rm=TRUE)
ENVIRONMENTAL$comc_ss_collective_capacity_p<-rowMeans(ENVIRONMENTAL[,grep("comc_phenx_",names(ENVIRONMENTAL), perl = TRUE,value=TRUE)[-1]],na.rm=TRUE)
ENVIRONMENTAL$ENVIR_ss_mean_3_items <- as.numeric(ENVIRONMENTAL$nsc_p_ss_mean_3_items)
KEEP<-grep("^comc_.*_p$", names(ENVIRONMENTAL))
KEEP<-c(KEEP,grep("^reshist_add", names(ENVIRONMENTAL)))
KEEP<-c(KEEP,grep("^neighborhood", names(ENVIRONMENTAL)))
KEEP<-c(KEEP,grep("^school_.*_y$", names(ENVIRONMENTAL)))
KEEP<-c(KEEP,grep("ledsch_seda_d_", names(ENVIRONMENTAL)))
KEEP<-c(KEEP,grep("led_sch_seda_s_", names(ENVIRONMENTAL)))
KEEP<-c(KEEP,grep("srpf_y_ss_(?!.*(_nm|_nt|_na)$)", names(ENVIRONMENTAL), perl = TRUE))
for (COLNUM in KEEP){
	ENVIRONMENTAL[,paste0("ENVIR_",names(ENVIRONMENTAL)[COLNUM])]<-ENVIRONMENTAL[,COLNUM]
}
ENVIRONMENTAL<-ENVIRONMENTAL[,c(1,2,grep("ENVIR_",names(ENVIRONMENTAL)))]
write.csv(ENVIRONMENTAL,paste0(ROOT,"/datasets/Preproc1/n",nrow(ENVIRONMENTAL),"x",ncol(ENVIRONMENTAL),"_environmental.csv"),row.names=FALSE); rm(ENVIRONMENTAL)

######
### Behavior/Biology Features
######

BEHAVIOR<-LoadFeatures(c(
	"ce_p_sag",
	"ce_y_sag",
	"ce_y_pbp",
	"cvd_y_fitb_act_w",
	"cvd_y_fitb_slp_w",
	"nt_p_psq",
	"ph_y_yrb",
	"ph_p_otbi",
	"ph_y_sal_horm",
	"su_p_ksads_sud",
	"su_y_alc_exp",
	"su_y_can_exp"
))

BEHAVIOR$hormone_scr_dhea_rep1<-suppressWarnings(as.numeric(BEHAVIOR$hormone_scr_dhea_rep1))
BEHAVIOR[grep("^sag", names(BEHAVIOR))]<-floor(BEHAVIOR[grep("^sag", names(BEHAVIOR))])
BEHAVIOR[grep("^sag", names(BEHAVIOR))]<-lapply(BEHAVIOR[grep("^sag", names(BEHAVIOR))], function(x) ifelse(x == -1, 0, x))
BEHAVIOR$BEHAV_hormone_female <- ifelse(BEHAVIOR$hormone_sal_sex == 1, 1, 0)
BEHAVIOR$BEHAV_hormone_male <- ifelse(BEHAVIOR$hormone_sal_sex == 2, 1, 0)
BEHAVIOR$BEHAV_HeadInjurySeverity <- rowSums(data.frame(BEHAVIOR$tbi_2,BEHAVIOR$tbi_3,BEHAVIOR$tbi_4,BEHAVIOR$tbi_5,BEHAVIOR$tbi_6o,BEHAVIOR$tbi_7a),na.rm=TRUE)
BEHAVIOR$BEHAV_HeadInjuryBinary <- ifelse(BEHAVIOR$BEHAV_HeadInjurySeverity == 0, 0, 1)
BEHAVIOR$BEHAV_HeadInjuryWorstSS <- as.numeric(BEHAVIOR$tbi_ss_worst_overall)
BEHAVIOR$BEHAV_Hospitalization <- ifelse(BEHAVIOR$tbi_1 == 1, 1, 0)
BEHAVIOR$BEHAV_PeerSubUse_Marijuana <- ifelse(BEHAVIOR$ksads_dud_raw_604_p == 0, 0, 1)
BEHAVIOR$BEHAV_PeerSubUse_Stimulants <- ifelse(BEHAVIOR$ksads_dud_raw_605_p == 0, 0, 1)
BEHAVIOR$BEHAV_PeerSubUse_Sedative <- ifelse(BEHAVIOR$ksads_dud_raw_606_p == 0, 0, 1)
BEHAVIOR$BEHAV_PeerSubUse_Cocaine <- ifelse(BEHAVIOR$ksads_dud_raw_607_p == 0, 0, 1)
BEHAVIOR$BEHAV_PeerSubUse_Opioids <- ifelse(BEHAVIOR$ksads_dud_raw_608_p == 0, 0, 1)
BEHAVIOR$BEHAV_PeerSubUse_Psychedelic <- ifelse(BEHAVIOR$ksads_dud_raw_609_p == 0, 0, 1)
BEHAVIOR$BEHAV_PeerSubUse_Tobacco <- ifelse(BEHAVIOR$ksads_dud_raw_610_p == 0, 0, 1)
BEHAVIOR$BEHAV_PeerSubUse_Solvents <- ifelse(BEHAVIOR$ksads_dud_raw_611_p == 0, 0, 1)
BEHAVIOR$BEHAV_SubUse_Marijuana <- ifelse(BEHAVIOR$ksads_dud_raw_614_p == 0, 0, 1)
BEHAVIOR$BEHAV_SubUse_Stimulants <- ifelse(BEHAVIOR$ksads_dud_raw_615_p == 0, 0, 1)
BEHAVIOR$BEHAV_SubUse_Sedative <- ifelse(BEHAVIOR$ksads_dud_raw_616_p == 0, 0, 1)
BEHAVIOR$BEHAV_SubUse_Cocaine <- ifelse(BEHAVIOR$ksads_dud_raw_617_p == 0, 0, 1)
BEHAVIOR$BEHAV_SubUse_Opioids <- ifelse(BEHAVIOR$ksads_dud_raw_618_p == 0, 0, 1)
BEHAVIOR$BEHAV_SubUse_Psychedelic <- ifelse(BEHAVIOR$ksads_dud_raw_619_p == 0, 0, 1)
BEHAVIOR$BEHAV_SubUse_Tobacco <- ifelse(BEHAVIOR$ksads_dud_raw_620_p == 0, 0, 1)
BEHAVIOR$BEHAV_SubUse_Solvents <- ifelse(BEHAVIOR$ksads_dud_raw_621_p == 0, 0, 1)
KEEP<-grep("^sag", names(BEHAVIOR))
KEEP<-c(KEEP,grep("^psq_.*_p$", names(BEHAVIOR)))
KEEP<-c(KEEP,grep("^stp_.*_hr_p$", names(BEHAVIOR)))
KEEP<-c(KEEP,grep("^physical_activity.*_y$", names(BEHAVIOR)))
KEEP<-c(KEEP,grep("^pbp_(?!.*(_nm|_nt|admin)$)", names(BEHAVIOR), perl = TRUE))
KEEP<-c(KEEP,grep("^aeq_(?!.*(_nm|_nt|admin)$)", names(BEHAVIOR), perl = TRUE))
KEEP<-c(KEEP,grep("^meeq_(?!.*(_nm|_nt|admin)$)", names(BEHAVIOR), perl = TRUE))
KEEP<-c(KEEP,grep("^hormone_(?!.*(_qns|_nd|_y|_sex)$)", names(BEHAVIOR), perl = TRUE))
for (COLNUM in KEEP){
	BEHAVIOR[,paste0("BEHAV_",names(BEHAVIOR)[COLNUM])]<-BEHAVIOR[,COLNUM]
}
BEHAVIOR<-BEHAVIOR[,c(1,2,grep("BEHAV_",names(BEHAVIOR)))]
write.csv(BEHAVIOR,paste0(ROOT,"/datasets/Preproc1/n",nrow(BEHAVIOR),"x",ncol(BEHAVIOR),"_behavior.csv"),row.names=FALSE); rm(BEHAVIOR)

######
### Adversity Features
######

ADVERSITY<-LoadFeatures(c(
	"abcd_p_demo",
	"mh_p_ksads_ptsd",
	"mh_y_le", 
	"mh_p_le",
	"mh_y_ksads_bg",
	"mh_p_ksads_bg",
	"mh_y_peq",
	"sd_y_apq", 
	"sd_y_vict"
))

ADVERSITY$STRSS_Bullying<-ADVERSITY$kbi_p_c_bully
KEEP<-grep("^demo_fam_exp.*_v2$", names(ADVERSITY))
KEEP<-c(KEEP,grep("^peq_.*_vic$", names(ADVERSITY)))
KEEP<-c(KEEP,grep("^ksads_ptsd_raw_.*_p$", names(ADVERSITY)))
KEEP<-c(KEEP,grep("^ple_(?!.*(admin|fu2_y|fu_y|_yr_y|_fu_p|_fu2_p|_yr_p|_nt|_nm)$)", names(ADVERSITY), perl = TRUE))
KEEP<-c(KEEP,grep("^peq_ss_(?!.*(admin|_nm|_nt|_na|_aggression|_aggs)$)", names(ADVERSITY), perl = TRUE))
for (COLNUM in KEEP){
	ADVERSITY[,paste0("STRSS_",names(ADVERSITY)[COLNUM])]<-ADVERSITY[,COLNUM]
}
ADVERSITY<-ADVERSITY[,c(1,2,grep("STRSS_",names(ADVERSITY)))]
write.csv(ADVERSITY,paste0(ROOT,"/datasets/Preproc1/n",nrow(ADVERSITY),"x",ncol(ADVERSITY),"_adversity.csv"),row.names=FALSE); rm(ADVERSITY)

######
### Cognition/Decision-Making Features
######

COGNITION<-LoadFeatures(c(
	"ce_y_sag",
	"ce_y_wps",
	"nc_y_cct",
	"nc_y_nihtb",
	"nc_y_wisc",
	"nc_y_gdt"
))

COGNITION$COG_skipschool_days <- ifelse(COGNITION$sag_days_skip_school == -1, NA, COGNITION$sag_days_skip_school)
COGNITION$COG_lastyr_grades <- ifelse(COGNITION$sag_grades_last_yr == -1, NA, COGNITION$sag_grades_last_yr)
COGNITION$COG_delayed_cash_task <- ifelse(COGNITION$cash_choice_task == 2, 1, 0)
COGNITION$COG_WISCV_TotalRaw <- COGNITION$pea_wiscv_trs
COGNITION$COG_WISCV_TotalScaled <- COGNITION$pea_wiscv_tss 
COGNITION$COG_WISCV_rMatrix <- COGNITION$pea_wiscv_item_a_rs 
COGNITION$COG_WISCV_rSerialOrder <- COGNITION$pea_wiscv_item_b_rs 
KEEP<-grep("^nihtbx_.*_agecorrected$", names(COGNITION))
KEEP<-c(KEEP,grep("^gdt", names(COGNITION)))
for (COLNUM in KEEP){
	COGNITION[,paste0("COG_",names(COGNITION)[COLNUM])]<-COGNITION[,COLNUM]
}
COGNITION<-COGNITION[,c(1,2,grep("COG_",names(COGNITION)))]
write.csv(COGNITION,paste0(ROOT,"/datasets/Preproc1/n",nrow(COGNITION),"x",ncol(COGNITION),"_cognition.csv"),row.names=FALSE); rm(COGNITION)

######
### MRI - Anatomical Features
######

ANATOMICAL<-LoadFeatures(c(
	"mri_y_smr_thk_dsk",
	"mri_y_smr_vol_dsk",
	"mri_y_smr_vol_aseg"
))

names(ANATOMICAL)<-gsub("smri_","SMRI_",names(ANATOMICAL))
write.csv(ANATOMICAL,paste0(ROOT,"/datasets/Preproc1/n",nrow(ANATOMICAL),"x",ncol(ANATOMICAL),"_mri_anat.csv"),row.names=FALSE); rm(ANATOMICAL)

######
### MRI - Functional Features
######

FUNCTIONAL<-LoadFeatures(c(
	"mri_y_rsfmr_cor_gp_gp",
	"mri_y_tfmr_nback_ngfvntf_dsk"
))

names(FUNCTIONAL)<-gsub("rsfmri","fMRI",names(FUNCTIONAL))
names(FUNCTIONAL)<-gsub("tfmri_nback_all_","fMRI_nback_all_",names(FUNCTIONAL))
FUNCTIONAL<-FUNCTIONAL[,c(1,2,grep("fMRI_",names(FUNCTIONAL)))]
write.csv(FUNCTIONAL,paste0(ROOT,"/datasets/Preproc1/n",nrow(FUNCTIONAL),"x",ncol(FUNCTIONAL),"_mri_func.csv"),row.names=FALSE); rm(FUNCTIONAL)

######
### MRI - Diffusion Features
######

DIFFUSION<-LoadFeatures(c(
	"mri_y_dti_fa_is_wm_dsk",
	"mri_y_dti_md_is_wm_dsk",
	"mri_y_dti_vol_is_at"
))

names(DIFFUSION)<-gsub("dmri","dMRI",names(DIFFUSION))
write.csv(DIFFUSION,paste0(ROOT,"/datasets/Preproc1/n",nrow(DIFFUSION),"x",ncol(DIFFUSION),"_mri_dwi.csv"),row.names=FALSE); rm(DIFFUSION)

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######