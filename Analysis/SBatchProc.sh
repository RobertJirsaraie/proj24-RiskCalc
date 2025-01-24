#!/bin/bash
###########

ROOT="/Users/rjirsara/Box Sync/Research/proj24-RiskCalculator"
ROOT="/Users/robertjirsaraie/Library/CloudStorage/Box-Box/Research/proj24-RiskCalculator/"

######
### Optimize XGB Algorithems Per Dimension of Psychopathology
######

for PREPROC in `echo Preproc4_Primary Preproc5_Longitudinal Preproc6_Future` ; do
	SUFFIX=`echo $PREPROC | cut -d '_' -f2 | tr '[:upper:]' '[:lower:]'`
	if [[ $SUFFIX == "longitudinal" ]]; then
	    SUFFIX="lm"
	elif [[ $SUFFIX == "primary" ]]; then
	    SUFFIX="r"
	fi
	for DF in `ls ${ROOT}/datasets/${PREPROC} | grep .csv` ; do
		LABEL=`echo $DF | cut -d '.' -f1 | awk -F'_' '{print $NF}'`
		for TARG in `echo totalprob internal external attention`; do 
			echo ""
			echo "###### ⚡⚡⚡⚡ ##### ⚡⚡⚡⚡ ###### ⚡⚡⚡⚡ ####"
			echo "${PREPROC}: Optimizing XGBoost Algo Using ${LABEL} Features to Predict ${TARG}"
			python3 $ROOT/analysis/Software/Optimize-XGB.py $DF TARGET_bpm_${TARG}_${SUFFIX} $PREPROC 
		done
	done
done

######
### Optimize Sex-Specific XGB Algorithems
######

for PREPROC in `echo Preproc4_Primary Preproc5_Longitudinal Preproc6_Future` ; do
	SUFFIX=`echo $PREPROC | cut -d '_' -f2 | tr '[:upper:]' '[:lower:]'`
	if [[ $SUFFIX == "longitudinal" ]]; then
	    SUFFIX="lm"
	elif [[ $SUFFIX == "primary" ]]; then
	    SUFFIX="r"
	fi
	for DF in `ls ${ROOT}/datasets/${PREPROC} | grep .csv` ; do
		LABEL=`echo $DF | cut -d '.' -f1 | awk -F'_' '{print $NF}'`
		for SEX in `echo M F`; do 
			echo ""
			echo "###### ⚡⚡⚡⚡ ##### ⚡⚡⚡⚡ ###### ⚡⚡⚡⚡ ####"
			echo "${PREPROC}: Optimizing XGBoost Algo Using ${LABEL} Features for ${SEX}"
			python3 $ROOT/analysis/Software/Optimize-Sex.py $DF TARGET_bpm_totalprob_${SUFFIX} $PREPROC $SEX
		done
	done
done

######
### Optimize Future Prediction Algorithems (Baseline Controlled)
######

for DF in `ls ${ROOT}/datasets/Preproc6_Future | grep .csv` ; do
	LABEL=`echo $DF | cut -d '.' -f1 | awk -F'_' '{print $NF}'`
	echo ""
	echo "###### ⚡⚡⚡⚡ ##### ⚡⚡⚡⚡ ###### ⚡⚡⚡⚡ ####"
	echo "Training Model Using ${LABEL} Features "
	python3 $ROOT/analysis/$TODAY/Software/Optimize-XGB.py $DF TARGET_bpm_totalprob_resid Preproc6_Future
done

######
### Generate Partial Dependency Insights
######

python3 $ROOT/analysis/Software/Optimize-PDP.py n24838x969_multimodal.csv TARGET_bpm_totalprob_r Preproc4_Primary 
python3 $ROOT/analysis/Software/Optimize-PDP.py n9599x973_multimodal.csv TARGET_bpm_totalprob_lm Preproc5_Longitudinal
python3 $ROOT/analysis/Software/Optimize-PDP.py n3933x977_multimodal.csv TARGET_bpm_totalprob_future Preproc6_Future

######
### Optimize Algorithems via the AutoML Ensamble Framework
######

for PREPROC in `echo Preproc4_Primary Preproc5_Longitudinal Preproc6_Future` ; do
	SUFFIX=`echo $PREPROC | cut -d '_' -f2 | tr '[:upper:]' '[:lower:]'`
	if [[ $SUFFIX == "longitudinal" ]]; then
	    SUFFIX="lm"
	elif [[ $SUFFIX == "primary" ]]; then
	    SUFFIX="r"
	fi
	for DF in `ls ${ROOT}/datasets/${PREPROC} | grep .csv` ; do
		LABEL=`echo $DF | cut -d '.' -f1 | awk -F'_' '{print $NF}'`
		for STACK in `seq 0 2`; do 
			echo ""
			echo "###### ⚡⚡⚡⚡ ##### ⚡⚡⚡⚡ ###### ⚡⚡⚡⚡ ####"
			echo "${PREPROC}: Optimizing AutoML Using ${LABEL} Features with ${STACK} Stacks"
			python3 $ROOT/analysis/Software/Optimize-AutoML.py $DF TARGET_bpm_totalprob_${SUFFIX} $PREPROC $STACK
		done
	done
done

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######