#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#pip install --upgrade pip
#pip install fastai --user
#pip install -U lightgbm --user
#pip install --upgrade xgboost --user
#pip install autogluon.tabular --user
#####################################

import pandas as pd, numpy as np, joblib, random, scipy.stats as stats, sklearn, json
import os, seaborn as sns, matplotlib.pyplot as plt, pickle, warnings, glob, sys
from sklearn.model_selection import GridSearchCV, StratifiedKFold, GroupKFold
from sklearn.model_selection import StratifiedGroupKFold, GroupShuffleSplit
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from sklearn.preprocessing import MinMaxScaler, StandardScaler
from sklearn import preprocessing 
from xgboost import XGBClassifier
from datetime import date
from autogluon.tabular import TabularPredictor
import autogluon.tabular, fastai, torch, autogluon.core as agc

ROOT="/Users/robertjirsaraie/Library/CloudStorage/Box-Box/Research/proj24-RiskCalculator"

DF=sys.argv[1]; TARG=sys.argv[2]; PREFIX=sys.argv[3]; STACKS=int(sys.argv[4])

ANALYSIS=f'{ROOT}/analysis/{PREFIX.split("_")[0]}_{TARG.replace("TARGET_bpm_","")}'
SET=os.path.basename(f'{ROOT}/datasets/{PREFIX}/{DF}').split('_')[1].split('.')[0]
warnings.filterwarnings("ignore", category=UserWarning)
TODAY = date.today().strftime("%Y%m%d")
nGPUs=torch.cuda.device_count()

######
### Prepare Data
######

#Load Data
df=pd.read_csv(f'{ROOT}/datasets/{PREFIX}/{DF}'); subids = df['src_subject_id']
target = df[TARG]; bin_target = pd.qcut(target, q=5, labels=False, duplicates='drop')
features = df.filter(regex='^(?!TARGET_)').drop(columns=['src_subject_id','eventname'])

#Define Cross Validation Splits
GrpValSplit = GroupShuffleSplit(n_splits=1, test_size=0.2)
GrpStratKFold = StratifiedGroupKFold(n_splits=5)
GrpKFold = GroupKFold(n_splits=5)

######
### Optimize Predictive Algorithem
######

for i, (train_idx, test_idx) in enumerate(GrpStratKFold.split(X=features, y=bin_target, groups=subids)):
    print(f"Starting CV Iteration {i+1}: Train IDs {train_idx} Test IDs {test_idx}")
    X_train, X_test = features.iloc[train_idx], features.iloc[test_idx]
    y_train, y_test = target.iloc[train_idx], target.iloc[test_idx]
    test_subids = df.loc[X_test.index, 'src_subject_id']
    train_subids = df.loc[X_train.index, 'src_subject_id']
    train_main_idx, val_idx = next(GrpValSplit.split(X_train, y_train, groups=train_subids))
    X_train_main, X_val = X_train.iloc[train_main_idx], X_train.iloc[val_idx]
    y_train_main, y_val = y_train.iloc[train_main_idx], y_train.iloc[val_idx]
    #Merge Datasets
    TRAIN=pd.concat([X_train_main, y_train_main, train_subids.iloc[train_main_idx]],axis=1)
    TEST=pd.concat([X_test, y_test, test_subids],axis=1)
    #Define Predictive Algorithem
    ALGOR=f"{ROOT}/algorithms/algo-AutoML_cv-{i+1}_{SET}_{TARG.split('_')[-1]}"
    PREDICTOR = TabularPredictor(
            label=TARG,
            eval_metric='rmse',
            problem_type='regression',
            path=ALGOR,
    )
    #Train Model
    PREDICTOR.fit(
            train_data=TRAIN,
            presets='best_quality',
            time_limit=600,
            num_stack_levels=STACKS,
            ag_args_fit={'num_gpus': nGPUs},
            verbosity=0
    )
    train_pred=pd.Series(np.round(PREDICTOR.predict(TRAIN),decimals=2), index=TRAIN.index)
    train_ids=df.loc[TRAIN.index,['src_subject_id','eventname']]
    results=pd.concat([train_ids, TRAIN[TARG], train_pred], axis=1)
    results.columns = ['subject', 'eventname', 'targets', 'predictions']
    errors_train = pd.DataFrame({
        'split': [i+1],
        'train_mse': [mean_squared_error(results['targets'], results['predictions'])],
        'train_mae': [mean_absolute_error(results['targets'], results['predictions'])],
        'train_cod': [r2_score(results['targets'], results['predictions'])],
    })
    #Evaluate Accuracy on Testing Data
    predict=pd.Series(np.round(PREDICTOR.predict(TEST),decimals=2), index=TEST.index)
    test_ids=df.loc[TEST.index,['src_subject_id','eventname']]
    results=pd.concat([test_ids, TEST[TARG], predict], axis=1)
    results.columns = ['subject', 'eventname', 'targets', 'predictions']
    errors_test = pd.DataFrame({
        'split': [i+1],
        'train_mse': [mean_squared_error(results['targets'], results['predictions'])],
        'train_mae': [mean_absolute_error(results['targets'], results['predictions'])],
        'train_cod': [r2_score(results['targets'], results['predictions'])],
    })
    #Store Results
    errors=pd.concat([errors_train, errors_test], axis=1); print(errors)
    if i == 0:
        ERRORS=errors
        RESULTS=results
    else:
        ERRORS=pd.concat([ERRORS, errors])
        RESULTS=pd.concat([RESULTS, results], axis=0)

### Save Results
os.makedirs(f'{ANALYSIS}', exist_ok=True)
ERRORS.to_csv(f'{ANALYSIS}/df-{SET}_mod-AutoML_accuracy.csv',index=False)
RESULTS.to_csv(f'{ANALYSIS}/df-{SET}_mod-AutoML_predict.csv',index=False)

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
