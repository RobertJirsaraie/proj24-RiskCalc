#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#pip install --upgrade pip
#pip3 install xgboost --user
############################

import pandas as pd, numpy as np, joblib, os, warnings, glob, sklearn, sys
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from sklearn.model_selection import GridSearchCV, StratifiedKFold, GroupKFold
from sklearn.model_selection import StratifiedGroupKFold, GroupShuffleSplit
from sklearn.ensemble import AdaBoostRegressor
from xgboost import XGBRegressor
warnings.filterwarnings("ignore")

DF=sys.argv[1]; TARG=sys.argv[2]; PREFIX=sys.argv[3]; SEX=sys.argv[4]

ROOT="/Users/rjirsara/Box Sync/Research/proj24-RiskCalculator"
ANALYSIS=f'{ROOT}/analysis/{PREFIX.split("_")[0]}_{TARG.replace("TARGET_bpm_","")}'
SET=os.path.basename(f'{ROOT}/datasets/Preproc4/{DF}').split('_')[1].split('.')[0]

######
### Define Sex-Specific Subset
######

if SEX == "M":
    SELECT=1
else:
    SELECT=0

FILES=os.listdir(f'{ROOT}/datasets/{PREFIX}')
MULTI=[file for file in FILES if 'multimodal' in file][0]
MASTER=pd.read_csv(f'{ROOT}/datasets/{PREFIX}/{MULTI}')
SUBSET=MASTER.loc[MASTER['DEMO_Male']==SELECT,"src_subject_id"].tolist()

######
### Load Data
######

df=pd.read_csv(f'{ROOT}/datasets/{PREFIX}/{DF}')
df=df[df['src_subject_id'].isin(SUBSET)]; subids = df['src_subject_id']
target = df[TARG]; bin_target = pd.qcut(target, q=5, labels=False, duplicates='drop')
features = df.filter(regex='^(?!TARGET_)').drop(columns=['src_subject_id','eventname'])

######
### Define Predictive Algorithem
######

#DEVICE = "cuda" if torch.cuda.is_available() else 'cpu'
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
### Optimize Predictive Algorithem
######

IMPORTANCE = pd.DataFrame(index=features.columns)
HYPERPARAMETERS = pd.DataFrame(index=['split', 'n_estimators', 'max_depth', 'gamma', 'reg_alpha', 'min_child_weight'])
for i, (train_idx, test_idx) in enumerate(GrpStratKFold.split(X=features, y=bin_target, groups=subids)):
    print(f"Starting CV Iteration {i+1}: Train IDs {train_idx} Test IDs {test_idx}")
    X_train, X_test = features.iloc[train_idx], features.iloc[test_idx]
    y_train, y_test = target.iloc[train_idx], target.iloc[test_idx]
    train_subids = df.loc[X_train.index, 'src_subject_id']
    train_main_idx, val_idx = next(GrpValSplit.split(X_train, y_train, groups=train_subids))
    X_train_main, X_val = X_train.iloc[train_main_idx], X_train.iloc[val_idx]
    y_train_main, y_val = y_train.iloc[train_main_idx], y_train.iloc[val_idx] 
    GRID.fit(
        X_train_main, 
        y_train_main.values,
        groups=train_subids.iloc[train_main_idx],
        eval_set=[(X_val, y_val)],
        verbose=False
    )
    #Evaulate Tuned Parameters & Gini Impurity
    parameters = GRID.best_estimator_.get_params(); parameters['split'] = i+1
    print(f"Number of Trees: {GRID.best_estimator_.best_iteration}")
    print(f"Tree Depth: {parameters['max_depth']}")
    print(f"Gamma: {parameters['gamma']}")
    print(f"Min-Weight: {parameters['min_child_weight']}")
    parameters = pd.DataFrame.from_dict(parameters, orient='index')
    parameters = parameters.rename(columns={parameters.columns[0]: f'Split{i+1}'})
    parameters.at['n_estimators',f'Split{i+1}']=GRID.best_estimator_.best_iteration
    HYPERPARAMETERS = HYPERPARAMETERS.merge(parameters, how='left', left_index=True, right_index=True)
    impurity = GRID.best_estimator_.feature_importances_
    impurity = pd.Series(impurity, name=f'Split{i+1}', index=IMPORTANCE.index)
    IMPORTANCE = pd.concat([IMPORTANCE, impurity], axis=1)
    #Evaulate Predictive Accuracy
    test_subids=df.loc[y_test.index,['src_subject_id','eventname']]
    train_subids=df.loc[y_train.index,['src_subject_id','eventname']]
    validate=pd.Series(np.round(GRID.predict(X_train).flatten(),decimals=2), index=y_train.index)
    results = pd.concat([train_subids, y_train, validate], axis=1)
    results.columns = ['subject', 'eventname', 'targets', 'predictions']
    errors_train = pd.DataFrame({
        'split': [i+1],
        'train_mse': [mean_squared_error(y_train, validate)],
        'train_mae': [mean_absolute_error(y_train, validate)],
        'train_cod': [r2_score(y_train, validate)],
    })
    predict=pd.Series(np.round(GRID.predict(X_test).flatten(),decimals=2), index=y_test.index)
    results=pd.concat([test_subids, y_test, predict], axis=1)
    results.columns = ['subject', 'eventname', 'targets', 'predictions']
    errors_test = pd.DataFrame({
        'test_mse': [mean_squared_error(y_test, predict)],
        'test_mae': [mean_absolute_error(y_test, predict)],
        'test_cod': [r2_score(y_test, predict)],
    }); errors=pd.concat([errors_train, errors_test], axis=1); print(errors)
    if i == 0:
        ERRORS=errors
        RESULTS=results
    else:
        ERRORS=pd.concat([ERRORS, errors])
        RESULTS=pd.concat([RESULTS, results], axis=0)

### Save Results
os.makedirs(f'{ANALYSIS}', exist_ok=True)
ERRORS.to_csv(f'{ANALYSIS}/df-{SET}_sex-{SEX}_accuracy.csv',index=False)
RESULTS.to_csv(f'{ANALYSIS}/df-{SET}_sex-{SEX}_predict.csv',index=False)
IMPORTANCE.to_csv(f'{ANALYSIS}/df-{SET}_sex-{SEX}_impurity.csv',index=True)
HYPERPARAMETERS.to_csv(f'{ANALYSIS}/df-{SET}_sex-{SEX}_parameters.csv',index=True)

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######