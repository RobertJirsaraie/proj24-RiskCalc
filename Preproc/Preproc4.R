#!/usr/bin/env Rscript
######################

invisible(lapply(c("stringr","tidyr","plyr","dplyr","tidyverse","ggplot2","readxl","writexl"), require, character.only=TRUE))
ROOT<-"/Users/robertjirsaraie/Library/CloudStorage/Box-Box/Research/proj24-RiskCalculator"
GUIDE<-read.csv(paste0(ROOT,"/datasets/ABCD/ABCD_Dictionary.csv"))
TODAY<-format(Sys.time(), "%Y%m%d")
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

source(paste0(ROOT,"/datasets/Software/Functions.R"))

######
### 1. Feature Selection and Engineering
######

# Mental Health Targets
FILES<-list.files(path = paste0(ROOT,"/datasets/ABCD/"), pattern = "*.csv$", recursive=TRUE, full.names = TRUE)
BPM<-read.csv(FILES[grep("mh_y_bpm",FILES)])
BPM<-BPM[!is.na(BPM$bpm_y_scr_totalprob_r),]; BPM<-BPM[!is.na(BPM$bpm_y_scr_internal_r),]
BPM<-BPM[,c("src_subject_id","eventname","bpm_y_scr_totalprob_r","bpm_y_scr_internal_r","bpm_y_scr_external_r","bpm_y_scr_attention_r")]
BPM[which(BPM$eventname=="6_month_follow_up_arm_1"),"eventname"]<-"baseline_year_1_arm_1"
BPM<-BPM[BPM$eventname %in% c("baseline_year_1_arm_1", "2_year_follow_up_y_arm_1","4_year_follow_up_y_arm_1"),]
names(BPM)<-gsub("bpm_y_scr_","TARGET_bpm_",names(BPM))

# Demographics Features
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

# Family Features
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
KEEP<-grep("^fam_enviro",names(FAMILY))
KEEP<-c(KEEP,grep("^fes_youth_q", names(FAMILY)))
KEEP<-c(KEEP,grep("^crpbi_parent", names(FAMILY)))
KEEP<-c(KEEP,grep("^parent_monitor_q", names(FAMILY)))
KEEP<-c(KEEP,grep("^macv_q", names(FAMILY)))
KEEP<-c(KEEP,grep("^mnbs_ss_", names(FAMILY)))
KEEP<-c(KEEP,grep("^fam_enviro.*_p$", names(FAMILY)))
KEEP<-c(KEEP,grep("^fes.*_p$", names(FAMILY)))
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

# Environmental Features
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
	"led_l_rxopioid",
	"gish_p_gi",
	"mh_y_peq",
	"mh_p_ksads_bg"
))

ENVIRONMENTAL$comc_ss_cohesion_p<-rowMeans(ENVIRONMENTAL[,c("comc_phenx_close_knit_p", "comc_phenx_help_p", "comc_phenx_get_along_p", "comc_phenx_share_values_p", "comc_phenx_trusted_p")],na.rm=TRUE)
ENVIRONMENTAL$comc_ss_control_p<-rowMeans(ENVIRONMENTAL[,c("comc_phenx_skip_p", "comc_phenx_graffiti_p", "comc_phenx_disrespect_p", "comc_phenx_fight_p", "comc_phenx_budget_p")],na.rm=TRUE)
ENVIRONMENTAL$comc_ss_collective_capacity_p<-rowMeans(ENVIRONMENTAL[,grep("comc_phenx_",names(ENVIRONMENTAL), perl = TRUE,value=TRUE)[-1]],na.rm=TRUE)
ENVIRONMENTAL$ENVIR_ss_mean_3_items <- as.numeric(ENVIRONMENTAL$nsc_p_ss_mean_3_items)
ENVIRONMENTAL$ENVIR_best_friend <-ifelse(ENVIRONMENTAL$kbi_p_c_best_friend == 1, 1, 0)
ENVIRONMENTAL$ENVIR_GrpFriendLen<-ENVIRONMENTAL$kbi_p_c_reg_friend_group_len
KEEP<-grep("^comc_.*_p$", names(ENVIRONMENTAL))
KEEP<-c(KEEP,grep("^reshist_add", names(ENVIRONMENTAL)))
KEEP<-c(KEEP,grep("^neighborhood", names(ENVIRONMENTAL)))
KEEP<-c(KEEP,grep("^school_.*_y$", names(ENVIRONMENTAL)))
KEEP<-c(KEEP,grep("ledsch_seda_d_", names(ENVIRONMENTAL)))
KEEP<-c(KEEP,grep("led_sch_seda_s_", names(ENVIRONMENTAL)))
KEEP<-c(KEEP,grep("^peq_.*_vic$", names(ENVIRONMENTAL)))
KEEP<-c(KEEP,grep("^peq_.*_perp$", names(ENVIRONMENTAL)))
KEEP<-c(KEEP,grep("^peq_ss_(?!.*(admin|_nm|_nt|_na|victim)$)", names(ENVIRONMENTAL), perl = TRUE))
KEEP<-c(KEEP,grep("^peq_ss_(?!.*(admin|_nm|_nt|_na|_aggression|_aggs)$)", names(ENVIRONMENTAL), perl = TRUE))
KEEP<-c(KEEP,grep("srpf_y_ss_(?!.*(_nm|_nt|_na)$)", names(ENVIRONMENTAL), perl = TRUE))
for (COLNUM in KEEP){
	ENVIRONMENTAL[,paste0("ENVIR_",names(ENVIRONMENTAL)[COLNUM])]<-ENVIRONMENTAL[,COLNUM]
}
ENVIRONMENTAL<-ENVIRONMENTAL[,c(1,2,grep("ENVIR_",names(ENVIRONMENTAL)))]

# Behavior/Biology Features
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
stp_numbers<-setdiff(1:9,7) 
for (i in stp_numbers){
  wkdy_var <- paste0("BEHAV_stp_", i, "_wkdy_hr_p")
  wkend_var <- paste0("BEHAV_stp_", i, "_wkend_hr_p")
  new_var <- paste0("BEHAV_stp_", i, "_wk_hr_p")
  BEHAVIOR[[new_var]] <- BEHAVIOR[[wkdy_var]] + BEHAVIOR[[wkend_var]]
}
BEHAVIOR <- BEHAVIOR[, !names(BEHAVIOR) %in% c(paste0("BEHAV_stp_", stp_numbers, "_wkdy_hr_p"))]
BEHAVIOR <- BEHAVIOR[, !names(BEHAVIOR) %in% c(paste0("BEHAV_stp_", stp_numbers, "_wkend_hr_p"))]
BEHAVIOR<-BEHAVIOR[,c(1,2,grep("BEHAV_",names(BEHAVIOR)))]

# Adversity Features
ADVERSITY<-LoadFeatures(c(
	"abcd_p_demo",
	"mh_p_ksads_ptsd",
	"mh_y_le", 
	"mh_p_le",
	"mh_y_ksads_bg",
	"mh_p_ksads_bg",
	"sd_y_apq", 
	"sd_y_vict"
))

ADVERSITY$STRSS_Bullying<-ADVERSITY$kbi_p_c_bully
KEEP<-grep("^demo_fam_exp.*_v2$", names(ADVERSITY))
KEEP<-c(KEEP,grep("^ksads_ptsd_raw_.*_p$", names(ADVERSITY)))
KEEP<-c(KEEP,grep("^ple_(?!.*(admin|fu2_y|fu_y|_yr_y|_fu_p|_fu2_p|_yr_p|_nt|_nm)$)", names(ADVERSITY), perl = TRUE))
for (COLNUM in KEEP){
	ADVERSITY[,paste0("STRSS_",names(ADVERSITY)[COLNUM])]<-ADVERSITY[,COLNUM]
}
ADVERSITY<-ADVERSITY[,c(1,2,grep("STRSS_",names(ADVERSITY)))]

# Cognition/Decision-Making Features
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

# MRI - Anatomical Features
ANATOMICAL<-LoadFeatures(c(
	"mri_y_smr_thk_dsk",
	"mri_y_smr_vol_dsk",
	"mri_y_smr_vol_aseg"
))

names(ANATOMICAL)<-gsub("smri_","SMRI_",names(ANATOMICAL))

# MRI - Functional Features
FUNCTIONAL<-LoadFeatures(c(
	"mri_y_rsfmr_cor_gp_gp",
	"mri_y_tfmr_nback_ngfvntf_dsk"
))

names(FUNCTIONAL)<-gsub("rsfmri","fMRI",names(FUNCTIONAL))
names(FUNCTIONAL)<-gsub("tfmri_nback_all_","fMRI_nback_all_",names(FUNCTIONAL))
FUNCTIONAL<-FUNCTIONAL[,c(1,2,grep("fMRI_",names(FUNCTIONAL)))]

# MRI - Diffusion Features
DIFFUSION<-LoadFeatures(c(
	"mri_y_dti_fa_is_wm_dsk",
	"mri_y_dti_md_is_wm_dsk",
	"mri_y_dti_vol_is_at"
))

names(DIFFUSION)<-gsub("dmri","dMRI",names(DIFFUSION))

######
### 2. Merge Feature Set Togeather and Generate Data Dictionary
######

#Merge the Master Dataset
data_frames <- list(BPM, DEMOGRAPHICS, ENVIRONMENTAL, FAMILY, BEHAVIOR, ADVERSITY, COGNITION, ANATOMICAL, FUNCTIONAL, DIFFUSION)
MASTER <- Reduce(function(x, y) merge(x, y, by = c("src_subject_id","eventname"), all = TRUE), data_frames)
MASTER$eventname <- factor(MASTER$eventname, levels=LEVELS, ordered=TRUE)
MASTER<-MASTER[!is.na(MASTER$TARGET_bpm_totalprob_r),]

#Create the Data Dictionary
DICT <- DataDictionary(MASTER,"Preproc4","TEMP"); DICT$Completeness<-round(DICT$Completeness,digits=2)
DICT <- DICT[, !names(DICT) %in% c("Transposed", "Invariant", "Inclusion", "Description")]
PRIOR <- data.frame(read_excel(paste0(ROOT,"/Datasets/Preproc1/Dictionary_1424x16.xlsx")))
PRIOR<-PRIOR[,c("Set","Feature","Transposed","Invariant","Inclusion")]
DICT<-merge(DICT,PRIOR,by=c("Set","Feature"),all=TRUE)

######
### 3. Remove Highly Redundant Features (r > 0.9)
######

COR<-cor(MASTER[,-c(1:6)], use="pairwise.complete.obs")
high_corr_pairs <- which(abs(COR) > 0.9, arr.ind = TRUE)
high_corr_pairs <- high_corr_pairs[high_corr_pairs[, 1] < high_corr_pairs[, 2], ]
RESULTS <- data.frame(
  Var1 = rownames(COR)[high_corr_pairs[, 1]],
  Var2 = colnames(COR)[high_corr_pairs[, 2]],
  Correlation = COR[high_corr_pairs]
)
split_vars <- strsplit(RESULTS$Var2, "_"); RESULTS$Set <- sapply(split_vars, `[`, 1)
RESULTS$Feature <- sapply(split_vars, function(x) paste(x[-1], collapse = "_"))
RESULTS<-RESULTS[,c("Set","Feature","Correlation")]
RESULTS$Correlation<-round(RESULTS$Correlation, digits=2)
REDUNDANT<-unique(RESULTS$Feature)

STORE<-DICT$Inclusion; DICT$Inclusion<-NULL
DICT[which(DICT$Feature %in% REDUNDANT),"Redundant"]<-"Yes"
DICT[is.na(DICT$Redundant),"Redundant"]<-"No"
DICT$Inclusion<-STORE

DICT<-merge(DICT, RESULTS, by=c("Set","Feature"),all=TRUE)
DICT <- DICT %>% mutate(Inclusion = ifelse(Redundant == "Yes", "Exclude", Inclusion))
write_xlsx(DICT, paste0(ROOT,"/datasets/Preproc4/Dictionary_",nrow(DICT),"x",ncol(DICT),".xlsx"))
#Manually Revise Newly Added Features For Columns: Invariant, Transposed, Redundant, Inclusion
file.remove(paste0(ROOT,"/datasets/Preproc4/Dictionary_",nrow(DICT),"x",ncol(DICT),".xlsx"),force=TRUE)

######
### Revised Dataframes By Transpose, Invariant, & Redundant Columns
######

DICT <- data.frame(read_excel(paste0(ROOT,"/datasets/Preproc4/Dictionary-preproc_1421x11.xlsx")))
DICT <- DICT[which(DICT$Inclusion!="Exclude"),]
TARGETS<-Preproc4("TARGET", BPM) 
DEMO<-Preproc4("DEMO", DEMOGRAPHICS)
ENVIR<-Preproc4("ENVIR",ENVIRONMENTAL)
FAM<-Preproc4("FAM",FAMILY)
BEHAV<-Preproc4("BEHAV",BEHAVIOR) 
STRSS<-Preproc4("STRSS",ADVERSITY) 
COG<-Preproc4("COG",COGNITION) 
SMRI<-Preproc4("SMRI",ANATOMICAL)
fMRI<-Preproc4("fMRI",FUNCTIONAL)
dMRI<-Preproc4("dMRI",DIFFUSION)

######
### Merge Feature Sets and Save 
######

data_frames<-list(TARGETS, DEMO, ENVIR, FAM, BEHAV, STRSS, COG, SMRI, fMRI, dMRI)
MASTER<-Reduce(function(x, y) merge(x, y, by = c("src_subject_id","eventname"), all = TRUE), data_frames)
LVLS <- c("baseline_year_1_arm_1","2_year_follow_up_y_arm_1","4_year_follow_up_y_arm_1")
MASTER$eventname <- factor(MASTER$eventname, levels=LVLS, ordered=TRUE)
MASTER<-MASTER[!is.na(MASTER$TARGET_bpm_totalprob_r),]
SaveFeatSpace("Preproc4","DEMO","demographics")
SaveFeatSpace("Preproc4","ENVIR","environmental")
SaveFeatSpace("Preproc4","FAM","family")
SaveFeatSpace("Preproc4","BEHAV","behavior") 
SaveFeatSpace("Preproc4","STRSS","adversity") 
SaveFeatSpace("Preproc4","COG","cognition") 
SaveFeatSpace("Preproc4","SMRI","sMRI")
SaveFeatSpace("Preproc4","fMRI","fMRI")
SaveFeatSpace("Preproc4","dMRI","dMRI")
write.csv(MASTER,paste0(ROOT,"/datasets/Preproc4/n",nrow(MASTER),"x",ncol(MASTER),"_multimodal.csv"),row.names=FALSE)

######
### Save Data Dictionary
######

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

DICT<-merge(DICT, GUIDE, by=c("Feature"), all.x = TRUE); 
DICT<-DICT[,c(2,1,3:ncol(DICT))]
DICT$Set<-factor(DICT$Set,levels=c("DEMO","COG","BEHAV","FAM","STRSS","ENVIR","SMRI","fMRI","dMRI","TARGET"))
DICT<-DICT[!duplicated(DICT),]; DICT<-DICT[order(DICT$Set),]
write_xlsx(DICT, paste0(ROOT,"/datasets/Preproc4/Dictionary-postproc_",nrow(DICT),"x",ncol(DICT),".xlsx"))

#Change Inclusion 
#Change Completness
#summary(factor(DICT$Set))
# BEHAV    COG   DEMO   dMRI  ENVIR    FAM   fMRI   SMRI  STRSS TARGET 
#    78     14     32    160    143    160    148    170     80      4 
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######