#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#pip install --upgrade pip
#pip3 install xgboost --user
############################

import pandas as pd, numpy as np, joblib, os, warnings, glob, sklearn, sys
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from sklearn.model_selection import GridSearchCV, StratifiedKFold, GroupKFold
from sklearn.model_selection import StratifiedGroupKFold, GroupShuffleSplit
from sklearn.inspection import partial_dependence
from xgboost import XGBRegressor
warnings.filterwarnings("ignore")

DF=sys.argv[1]; TARG=sys.argv[2]; PREFIX=sys.argv[3]

ROOT="/Users/robertjirsaraie/Library/CloudStorage/Box-Box/Research/proj24-RiskCalculator"
ANALYSIS=f'{ROOT}/analysis/{PREFIX.split("_")[0]}_{TARG.replace("TARGET_bpm_","")}'
ALGORITHMS=f'{ROOT}/algorithms'; LABEL=TARG.split('_')[-1].lower()

######
### Load Data
######

df = pd.read_csv(f'{ROOT}/datasets/{PREFIX}/{DF}'); subids = df['src_subject_id']
target = df[TARG]; bin_target = pd.qcut(target, q=5, labels=False, duplicates='drop')
features = df.filter(regex='^(?!TARGET_)').drop(columns=['src_subject_id','eventname'])

######
### Define Predictive Algorithem
######

GrpStratKFold = StratifiedGroupKFold(n_splits=5, shuffle=True, random_state=777)
GrpValSplit = GroupShuffleSplit(n_splits=1, test_size=0.2)
GrpKFold = GroupKFold(n_splits=5)

Hyperparameters = {
    'n_estimators': [2500],
    'learning_rate': [0.05],
    'early_stopping_rounds': [500],
    'max_depth':  [3],
    'gamma': [0, 2, 4],
    'reg_alpha':  [0, 2, 4, 8],
    'min_child_weight': [0, 2, 4, 8]
}

Model = XGBRegressor(
    objective='reg:squarederror',
    tree_method='hist',
    device='cpu'
)

GRID = GridSearchCV(
    estimator=Model,
    param_grid=Hyperparameters,
    cv=GrpKFold,
    n_jobs=-1,
    scoring="r2",
    verbose=False
)

######
### Find Top 3 Features
######

GINI = pd.read_csv(f'{ANALYSIS}/df-multimodal_mod-XGB_impurity.csv')
GINI = GINI.set_index(GINI.iloc[:, 0]).drop(GINI.columns[0], axis=1)
GINI['Means'] = GINI.apply(lambda row: sum(row)/len(row), axis=1)
GINI = GINI.sort_values(by='Means', ascending=False)
GINI = GINI.index[:3].tolist()
SELECT = [features.columns.tolist().index(var) for var in GINI if var in features.columns]

######
### Optimize Predictive Algorithem
######

FINAL = pd.DataFrame(index=['Results', 'GroupKFold', 'Predictor', 'Rank', 'Level', 'Predictions'])
for i, (train_idx, test_idx) in enumerate(GrpStratKFold.split(X=features, y=bin_target, groups=subids)):
    print(f"Starting CV Iteration {i+1}: Train IDs {train_idx} Test IDs {test_idx}")
    X_train, X_test = features.iloc[train_idx], features.iloc[test_idx]
    y_train, y_test = target.iloc[train_idx], target.iloc[test_idx]
    train_subids = df.loc[X_train.index, 'src_subject_id']
    train_main_idx, val_idx = next(GrpValSplit.split(X_train, y_train, groups=train_subids))
    X_train_main, X_val = X_train.iloc[train_main_idx], X_train.iloc[val_idx]
    y_train_main, y_val = y_train.iloc[train_main_idx], y_train.iloc[val_idx]    
    MODEL=f"{ALGORITHMS}/algor-XGB_cv-{i+1}_{LABEL}.dat"
    if os.path.exists(MODEL):
        GRID = joblib.load(MODEL)
    else:
        GRID.fit(
            X_train_main, 
            y_train_main.values,
            groups=train_subids.iloc[train_main_idx],
            eval_set=[(X_val, y_val)],
            verbose=False
        ); joblib.dump(GRID, MODEL)
    for INDEX, VAR in enumerate(GINI):
        PDP = partial_dependence(GRID, X_test, features=[SELECT[INDEX]], kind='average')
        RESULTS = pd.DataFrame({
            "Results": LABEL,
            "GroupKFold": i+1,
            "Predictor": VAR,  
            "Rank": INDEX+1, 
            "Level": PDP['grid_values'][0],
            "Predictions": PDP['average'][0]
        })
        FINAL=pd.concat([FINAL,RESULTS])

FINAL.to_csv(f'{ALGORITHMS}/partdependency-{LABEL}.csv',index=False)

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######