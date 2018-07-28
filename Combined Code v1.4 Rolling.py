# Load Fundamental Libraries
import csv
import numpy as np
import pandas as pd
import sklearn
from datetime import datetime
import seaborn as sns
from  itertools import chain

# Visualization Libraries
import matplotlib.pyplot as plt
sns.set(style="whitegrid", font_scale=1)
%matplotlib inline

# PCA Libraries
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA

import statsmodels.api as sm
import statsmodels.formula.api as smf

# Linear Regression Libraries
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn import metrics
from sklearn.metrics import r2_score
from sklearn import linear_model

#Random Forest and Gradient Boosting Libraries
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import RandomizedSearchCV
from sklearn.model_selection import GridSearchCV
from sklearn import ensemble
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import KFold

#Load Dataset
df = pd.read_csv('Combined_Dataset_v3.csv', parse_dates= ['Value Date'], index_col= "Value Date")
df = df.sort_index()
df.head()

#Response Variables
Y_ABS = df['ABS_12M']
Y_CMBS = df['CMBS_12M']
Y_IGCorp = df['IGCorp_12M']
Y_MBS = df['MBS_12M']
Y_USHighYield = df['USHighYield_12M']
Y_GovtRelated = df['GovtRelated_12M']

#Declaring Variables
df_features = df[df.columns[12:75]]
df_features.head()

#Training Set for Features (2005 through 2016)
all_train = df_features[(df_features.index > '2004-12-01') & (df_features.index <= '2016-12-31')]
all_train.info()

#Training Set for Response Variables
ABS_train = Y_ABS[(Y_ABS.index > '2004-12-01') & (Y_ABS.index <= '2016-12-31')]
CMBS_train = Y_CMBS[(Y_CMBS.index > '2004-12-01') & (Y_CMBS.index <= '2016-12-31')]
IGCorp_train = Y_IGCorp[(Y_IGCorp.index > '2004-12-01') & (Y_IGCorp.index <= '2016-12-31')]
GovtRelated_train = Y_GovtRelated[(Y_GovtRelated.index > '2004-12-01') & (Y_GovtRelated.index <= '2016-12-31')]
MBS_train = Y_MBS[(Y_MBS.index > '2004-12-01') & (Y_MBS.index <= '2016-12-31')]
USHighYield_train = Y_USHighYield[(Y_USHighYield.index > '2004-12-01') & (Y_USHighYield.index <= '2016-12-31')]

##########################
#Multi-collinearity

def trimm_correlated(df_in, threshold):
    df_corr = df_in.corr(method='pearson', min_periods=1)
    df_not_correlated = ~(df_corr.mask(np.tril(np.ones([len(df_corr)]*2, dtype=bool))).abs() > threshold).any()
    un_corr_idx = df_not_correlated.loc[df_not_correlated[df_not_correlated.index] == True].index
    df_out = df_in[un_corr_idx]
    return df_out

#all_train = trimm_correlated(all_train,.9)
all_train.columns
all_train.shape

#########################


###############
#Rolling Section Start

#Linear Model

#Set a window for 5 years
window=60

#Establish Linear Models
ABS_model_ols = linear_model.LinearRegression()
MBS_model_ols = linear_model.LinearRegression()
CMBS_model_ols = linear_model.LinearRegression()
IGCorp_model_ols = linear_model.LinearRegression()
GovtRelated_model_ols = linear_model.LinearRegression()
USHighYield_model_ols = linear_model.LinearRegression()

#Window to test
window_test = 12

iStart = 0
i = 1

#Always copy and run from this point forward, don't start at the for loop.

#Setting empty lists to hold results
ABS_predictions = []
ABS_actuals = []
MBS_predictions = []
MBS_actuals = []
CMBS_predictions = []
CMBS_actuals = []
IGCorp_predictions = []
IGCorp_actuals = []
GovtRelated_predictions = []
GovtRelated_actuals = []
USHighYield_predictions = []
USHighYield_actuals = []
all_ABS_preds = []
all_MBS_preds = []
all_CMBS_preds = []
all_IGCorp_preds = []
all_GovtRelated_preds = []
all_USHighYield_preds = []

#Loop through the dataset fitting a model based on 5 years
#Skip a Year, then test on the following year
#Take the December return of that year
#Store it 
for iStart in range(0, len(all_train),window_test):
    iEnd = iStart+window + 1
    iStart = iStart + 1
    if iEnd == 133:
        break
    print('training:',iStart,iEnd)

    pred_start = iEnd + window_test
    pred_end = pred_start + window_test + 1
    print('test:', pred_start, pred_end)
    
    #ABS
    ABS_model_ols.fit(all_train[iStart:iEnd], ABS_train[iStart:iEnd])
    ABS_preds = ABS_model_ols.predict(all_train[pred_start:pred_end])
    ABS_actual = ABS_train[pred_start:pred_end]
    ABS_return = ABS_actual.values
    ABS_predictions.append(ABS_preds[11])
    ABS_actuals.append(ABS_return[11])
    all_ABS_preds.append(ABS_preds)
    print('ABS preds:',ABS_predictions)
    print('ABS actual:',ABS_actuals)
    print('ABS r2',r2_score(ABS_return,ABS_preds))
    #MBS
    MBS_model_ols.fit(all_train[iStart:iEnd], MBS_train[iStart:iEnd])
    MBS_preds = MBS_model_ols.predict(all_train[pred_start:pred_end])
    MBS_actual = MBS_train[pred_start:pred_end]
    MBS_return = MBS_actual.values
    MBS_predictions.append(MBS_preds[11])
    MBS_actuals.append(MBS_return[11])
    all_MBS_preds.append(MBS_preds)
    print('MBS preds:',MBS_predictions)
    print('MBS actual:',MBS_actuals)
    print('MBS r2',r2_score(MBS_return,MBS_preds))
    #CMBS
    CMBS_model_ols.fit(all_train[iStart:iEnd], CMBS_train[iStart:iEnd])
    CMBS_preds = CMBS_model_ols.predict(all_train[pred_start:pred_end])
    CMBS_actual = CMBS_train[pred_start:pred_end]
    CMBS_return = CMBS_actual.values
    CMBS_predictions.append(CMBS_preds[11])
    CMBS_actuals.append(CMBS_return[11])
    all_CMBS_preds.append(CMBS_preds)
    print('CMBS preds:',CMBS_predictions)
    print('CMBS actual:',CMBS_actuals)
    print('CMBS r2',r2_score(CMBS_return,CMBS_preds))
    #IGCorp
    IGCorp_model_ols.fit(all_train[iStart:iEnd], IGCorp_train[iStart:iEnd])
    IGCorp_preds = IGCorp_model_ols.predict(all_train[pred_start:pred_end])
    IGCorp_actual = IGCorp_train[pred_start:pred_end]
    IGCorp_return = IGCorp_actual.values
    IGCorp_predictions.append(IGCorp_preds[11])
    IGCorp_actuals.append(IGCorp_return[11])
    all_IGCorp_preds.append(IGCorp_preds)
    print('IGCorp preds:',IGCorp_predictions)
    print('IGCorp actual:',IGCorp_actuals)
    print('IGCorp r2',r2_score(IGCorp_return,IGCorp_preds))
    #GovtRelated
    GovtRelated_model_ols.fit(all_train[iStart:iEnd], GovtRelated_train[iStart:iEnd])
    GovtRelated_preds = GovtRelated_model_ols.predict(all_train[pred_start:pred_end])
    GovtRelated_actual = GovtRelated_train[pred_start:pred_end]
    GovtRelated_return = GovtRelated_actual.values
    GovtRelated_predictions.append(GovtRelated_preds[11])
    GovtRelated_actuals.append(GovtRelated_return[11])
    all_GovtRelated_preds.append(GovtRelated_preds)
    print('GovtRelated preds:',GovtRelated_predictions)
    print('GovtRelated actual:',GovtRelated_actuals)
    print('GovtRelated r2',r2_score(GovtRelated_return,GovtRelated_preds))
    #USHighYield
    USHighYield_model_ols.fit(all_train[iStart:iEnd], USHighYield_train[iStart:iEnd])
    USHighYield_preds = USHighYield_model_ols.predict(all_train[pred_start:pred_end])
    USHighYield_actual = USHighYield_train[pred_start:pred_end]
    USHighYield_return = USHighYield_actual.values
    USHighYield_predictions.append(USHighYield_preds[11])
    USHighYield_actuals.append(USHighYield_return[11])
    all_USHighYield_preds.append(USHighYield_preds)
    print('USHighYield preds:',USHighYield_predictions)
    print('USHighYield actual:',USHighYield_actuals)
    print('USHighYield r2',r2_score(USHighYield_return,USHighYield_preds))
    
    
#Years with results    
years = [2011,2012,2013,2014,2015,2016]

#Dataframes to hold results
ABS_performance = pd.DataFrame({'Year':years,'ABS_preds': ABS_predictions,'ABS_actuals': ABS_actuals})
MBS_performance = pd.DataFrame({'Year':years,'MBS_preds': MBS_predictions,'MBS_actuals': MBS_actuals})
CMBS_performance = pd.DataFrame({'Year':years,'CMBS_preds': CMBS_predictions,'CMBS_actuals': CMBS_actuals})
IGCorp_performance = pd.DataFrame({'Year':years,'IGCorp_preds': IGCorp_predictions,'IGCorp_actuals': IGCorp_actuals})
GovtRelated_performance = pd.DataFrame({'Year':years,'GovtRelated_preds': GovtRelated_predictions,'GovtRelated_actuals': GovtRelated_actuals})
USHighYield_performance = pd.DataFrame({'Year':years,'USHighYield_preds': USHighYield_predictions,'USHighYield_actuals': USHighYield_actuals})

#Predictions and Actuals
performance = pd.DataFrame({'Year':years,'ABS_preds': ABS_predictions,'ABS_actuals': ABS_actuals,'MBS_preds': MBS_predictions,'MBS_actuals': MBS_actuals,
'CMBS_preds': CMBS_predictions,'CMBS_actuals': CMBS_actuals,'IGCorp_preds': IGCorp_predictions,'IGCorp_actuals': IGCorp_actuals,
'GovtRelated_preds': GovtRelated_predictions,'GovtRelated_actuals': GovtRelated_actuals,'USHighYield_preds': USHighYield_predictions,'USHighYield_actuals': USHighYield_actuals})

#Use the year as an index
performance = performance.set_index('Year')
#Create rankings based on highest return
performance.rank(axis=0, ascending=False)
#Flip the axis
performance.transpose()

#Compare the next two dataframes 
#Predictions
all_predictions = pd.DataFrame({'Year':years,'ABS': ABS_predictions,'MBS': MBS_predictions,'CMBS': CMBS_predictions,
'IGCorp': IGCorp_predictions,'GovtRelated': GovtRelated_predictions,'USHighYield': USHighYield_predictions})
all_predictions = all_predictions.set_index('Year')
all_predictions = all_predictions.transpose()
predicted_rankings = all_predictions.rank(axis=0, method='dense',ascending=False).astype('int64')
#predicted_rankings.to_csv('linear_rankings.csv', sep=',')
#all_predictions.to_csv('linear_preds.csv', sep=',')


#Monthly Predictions
mp = pd.DataFrame({'Year':years,'ABS': all_ABS_preds,'MBS': all_MBS_preds,'CMBS': all_CMBS_preds,
'IGCorp': all_IGCorp_preds,'GovtRelated': all_GovtRelated_preds,'USHighYield': all_USHighYield_preds})
monthly_predictions = pd.DataFrame({'Year':np.repeat(tf.Year.values,mp.ABS.str.len()),'ABS': list(chain.from_iterable(mp.ABS)),'MBS': list(chain.from_iterable(mp.MBS)),
'CMBS': list(chain.from_iterable(mp.CMBS)), 'IGCorp': list(chain.from_iterable(mp.IGCorp)), 'GovtRelated': list(chain.from_iterable(mp.GovtRelated)),'USHighYield': list(chain.from_iterable(mp.USHighYield))})
monthly_predictions = monthly_predictions.set_index('Year')
monthly_predictions.to_csv('linear_monthly_preds.csv', sep=',')

#Actuals
all_actuals = pd.DataFrame({'Year':years, 'ABS': ABS_actuals,'MBS': MBS_actuals,'CMBS': CMBS_actuals,
'IGCorp': IGCorp_actuals, 'GovtRelated': GovtRelated_actuals,'USHighYield': USHighYield_actuals})
all_actuals = all_actuals.set_index('Year')
all_actuals = all_actuals.transpose()
actual_rankings = all_actuals.rank(axis=0, ascending=False).astype('int64')
actual_rankings.to_csv('actual_rankings.csv', sep=',')
all_actuals.to_csv('actual_return.csv', sep=',')


########
#End of All Predictors section

#########

#Random Forest

window=60

#Establish  Models
ABS_rf = RandomForestRegressor(random_state=50)
MBS_rf = RandomForestRegressor(random_state=50)
CMBS_rf = RandomForestRegressor(random_state=50)
IGCorp_rf = RandomForestRegressor(random_state=50)
GovtRelated_rf = RandomForestRegressor(random_state=50)
USHighYield_rf = RandomForestRegressor(random_state=50)

#Window to test
window_test = 12

iStart = 0
i = 1

#Always copy and run from this point forward, don't start at the for loop.

#Setting empty lists to hold results
ABS_predictions = []
ABS_actuals = []
MBS_predictions = []
MBS_actuals = []
CMBS_predictions = []
CMBS_actuals = []
IGCorp_predictions = []
IGCorp_actuals = []
GovtRelated_predictions = []
GovtRelated_actuals = []
USHighYield_predictions = []
USHighYield_actuals = []

all_ABS_preds = []
all_MBS_preds = []
all_CMBS_preds = []
all_IGCorp_preds = []
all_GovtRelated_preds = []
all_USHighYield_preds = []
list_10_ABS = []
list_10_MBS = []
list_10_CMBS = []
list_10_IGCorp = []
list_10_GovtRelated = []
list_10_USHighYield = []

#Loop through the dataset fitting a model based on 5 years
#Skip a Year, then test on the following year
#Take the December return of that year
#Store it 
for iStart in range(0, len(all_train),window_test):
    iEnd = iStart+window + 1
    iStart = iStart + 1
    if iEnd == 133:
        break
    print('training:',iStart,iEnd)

    pred_start = iEnd + window_test
    pred_end = pred_start + window_test
    print('test:', pred_start, pred_end)
    
    
    ####  Random Forest CV 
    # Number of trees in random forest
    #n_estimators = [int(x) for x in np.linspace(start = 200, stop = 2000, num = 10)]
    # Number of features to consider at every split
    #max_features = ['auto', 'sqrt']
    # Maximum number of levels in tree
    #max_depth = [int(x) for x in np.linspace(10, 110, num = 11)]
    #max_depth.append(None)
    # Minimum number of samples required to split a node
    #min_samples_split = [2, 5, 10]
    # Minimum number of samples required at each leaf node
    #min_samples_leaf = [1, 2, 4]
    # Method of selecting samples for training each tree
    #bootstrap = [True, False]
    # Create the random grid
    #random_grid = {'n_estimators': n_estimators,
    #           'max_features': max_features,
    #           'max_depth': max_depth,
    #           'min_samples_split': min_samples_split,
    #           'min_samples_leaf': min_samples_leaf,
    #           'bootstrap': bootstrap}
    #print(random_grid)

    # Random search of parameters, using 3 fold cross validation, 
    # search across 100 different combinations, and use all available cores
    #rf_random = RandomizedSearchCV(estimator = rf, param_distributions = random_grid, n_iter = 100, cv = 3, verbose=2, random_state=50, n_jobs = -1)

    # Fit the random search model
    #rf_random.fit(all_train[iStart:iEnd], ABS_train[iStart:iEnd])
    #print(rf_random.best_params_)
    
    # Create the parameter grid based on the results of random search 
    param_grid = {
        'bootstrap': [True],
        'max_depth': [20, 80, 100],
        'max_features': ['auto'],
        'min_samples_leaf': [1, 2, 4],
        'min_samples_split': [2, 4, 7],
        'n_estimators': [200, 400, 1000, 1400]
    }

    rf = RandomForestRegressor(warm_start= True, random_state=50)

    # Instantiate the grid search model
    ABS_grid_search = GridSearchCV(estimator = rf, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
    ABS_grid_search.fit(all_train[iStart:iEnd],ABS_train[iStart:iEnd])
    ABS_grid_search.best_params_
    ABS_rf = RandomForestRegressor(**ABS_grid_search.best_params_,random_state=50)
    
    MBS_grid_search = GridSearchCV(estimator = rf, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
    MBS_grid_search.fit(all_train[iStart:iEnd],MBS_train[iStart:iEnd])
    MBS_grid_search.best_params_
    MBS_rf = RandomForestRegressor(**MBS_grid_search.best_params_,random_state=50)
    
    CMBS_grid_search = GridSearchCV(estimator = rf, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
    CMBS_grid_search.fit(all_train[iStart:iEnd],CMBS_train[iStart:iEnd])
    CMBS_grid_search.best_params_
    CMBS_rf = RandomForestRegressor(**CMBS_grid_search.best_params_,random_state=50)
    
    IGCorp_grid_search = GridSearchCV(estimator = rf, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
    IGCorp_grid_search.fit(all_train[iStart:iEnd],IGCorp_train[iStart:iEnd])
    IGCorp_grid_search.best_params_
    IGCorp_rf = RandomForestRegressor(**IGCorp_grid_search.best_params_,random_state=50)
    
    GovtRelated_grid_search = GridSearchCV(estimator = rf, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
    GovtRelated_grid_search.fit(all_train[iStart:iEnd],GovtRelated_train[iStart:iEnd])
    GovtRelated_grid_search.best_params_
    GovtRelated_rf = RandomForestRegressor(**GovtRelated_grid_search.best_params_,random_state=50)
    
    USHighYield_grid_search = GridSearchCV(estimator = rf, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
    USHighYield_grid_search.fit(all_train[iStart:iEnd],USHighYield_train[iStart:iEnd])
    USHighYield_grid_search.best_params_
    USHighYield_rf = RandomForestRegressor(**USHighYield_grid_search.best_params_,random_state=50)
    
    #ABS
    feature_list = (all_train.columns)
    ABS_rf.fit(all_train[iStart:iEnd],ABS_train[iStart:iEnd])
    importances = list(ABS_rf.feature_importances_)
    # List of tuples with variable and importance
    feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
    # Sort the feature importances by most important first
    feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
    # Print out the feature and importances 
    #ABS_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
    [print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:10]]
    
    print(ABS_rf.predict(all_train[pred_start:pred_end]))

    #Top Ten Factors
    ABS_RF_top10 = [x[0] for x in feature_importances[0:10]]
    ABS_RF_top10
    list_10_ABS.append(ABS_RF_top10)

    #Make Top Ten DF
    ABS_RF_all_train = all_train[ABS_RF_top10]
    ABS_RF_all_train.shape
    
    #Fit Top Ten
    #ABS_rf.fit(ABS_RF_all_train[iStart:iEnd],ABS_train[iStart:iEnd])
    

    ABS_preds = ABS_rf.predict(all_train[pred_start:pred_end])
    #ABS_preds = ABS_rf.predict(ABS_RF_all_train[pred_start:pred_end])
    ABS_actual = ABS_train[pred_start:pred_end]
    ABS_return = ABS_actual.values
    ABS_predictions.append(ABS_preds[11])
    ABS_actuals.append(ABS_return[11])
    all_ABS_preds.append(ABS_preds)
    print('ABS rf preds:',ABS_predictions)
    print('ABS actual:',ABS_actuals)
    print('ABS r2',r2_score(ABS_return,ABS_preds))
    print('ABS rf RMSE:', np.sqrt(metrics.mean_squared_error(ABS_return,ABS_preds)))
 
    #MBS
    feature_list = (all_train.columns)
    MBS_rf.fit(all_train[iStart:iEnd],MBS_train[iStart:iEnd])
    importances = list(MBS_rf.feature_importances_)
    # List of tuples with variable and importance
    feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
    # Sort the feature importances by most important first
    feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
    # Print out the feature and importances 
    #MBS_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
    [print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:10]]
    
    print(MBS_rf.predict(all_train[pred_start:pred_end]))

    #Top Ten Factors
    MBS_RF_top10 = [x[0] for x in feature_importances[0:10]]
    MBS_RF_top10
    list_10_MBS.append(MBS_RF_top10)

    #Make Top Ten DF
    MBS_RF_all_train = all_train[MBS_RF_top10]
    MBS_RF_all_train.shape
    
    #Fit Top Ten
    #MBS_rf.fit(MBS_RF_all_train[iStart:iEnd],MBS_train[iStart:iEnd])
    

    MBS_preds = MBS_rf.predict(all_train[pred_start:pred_end])
    #MBS_preds = MBS_rf.predict(MBS_RF_all_train[pred_start:pred_end])
    MBS_actual = MBS_train[pred_start:pred_end]
    MBS_return = MBS_actual.values
    MBS_predictions.append(MBS_preds[11])
    MBS_actuals.append(MBS_return[11])
    print('MBS rf preds:',MBS_predictions)
    all_MBS_preds.append(MBS_preds)
    print('MBS actual:',MBS_actuals)
    print('MBS r2',r2_score(MBS_return,MBS_preds))
    print('MBS rf RMSE:', np.sqrt(metrics.mean_squared_error(MBS_return,MBS_preds)))
    
    #CMBS
    feature_list = (all_train.columns)
    CMBS_rf.fit(all_train[iStart:iEnd],CMBS_train[iStart:iEnd])
    importances = list(CMBS_rf.feature_importances_)
    # List of tuples with variable and importance
    feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
    # Sort the feature importances by most important first
    feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
    # Print out the feature and importances 
    #CMBS_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
    [print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:10]]
    
    print(CMBS_rf.predict(all_train[pred_start:pred_end]))

    #Top Ten Factors
    CMBS_RF_top10 = [x[0] for x in feature_importances[0:10]]
    CMBS_RF_top10
    list_10_CMBS.append(CMBS_RF_top10)

    #Make Top Ten DF
    CMBS_RF_all_train = all_train[CMBS_RF_top10]
    CMBS_RF_all_train.shape
    
    #Fit Top Ten
    #CMBS_rf.fit(CMBS_RF_all_train[iStart:iEnd],CMBS_train[iStart:iEnd])
    
    CMBS_preds = CMBS_rf.predict(all_train[pred_start:pred_end])
    #CMBS_preds = CMBS_rf.predict(CMBS_RF_all_train[pred_start:pred_end])
    CMBS_actual = CMBS_train[pred_start:pred_end]
    CMBS_return = CMBS_actual.values
    CMBS_predictions.append(CMBS_preds[11])
    CMBS_actuals.append(CMBS_return[11])
    all_CMBS_preds.append(CMBS_preds)
    print('CMBS rf preds:',CMBS_predictions)
    print('CMBS actual:',CMBS_actuals)
    print('CMBS r2',r2_score(CMBS_return,CMBS_preds))
    print('CMBS rf RMSE:', np.sqrt(metrics.mean_squared_error(CMBS_return,CMBS_preds)))
    
    #IGCorp
    feature_list = (all_train.columns)
    IGCorp_rf.fit(all_train[iStart:iEnd],IGCorp_train[iStart:iEnd])
    importances = list(IGCorp_rf.feature_importances_)
    # List of tuples with variable and importance
    feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
    # Sort the feature importances by most important first
    feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
    # Print out the feature and importances 
    #IGCorp_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
    [print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:10]]
    
    print(IGCorp_rf.predict(all_train[pred_start:pred_end]))

    #Top Ten Factors
    IGCorp_RF_top10 = [x[0] for x in feature_importances[0:10]]
    IGCorp_RF_top10
    list_10_IGCorp.append(IGCorp_RF_top10)

    #Make Top Ten DF
    IGCorp_RF_all_train = all_train[IGCorp_RF_top10]
    IGCorp_RF_all_train.shape
    
    #Fit Top Ten
    #IGCorp_rf.fit(IGCorp_RF_all_train[iStart:iEnd],IGCorp_train[iStart:iEnd])
    
    IGCorp_preds = IGCorp_rf.predict(all_train[pred_start:pred_end])
    #IGCorp_preds = IGCorp_rf.predict(IGCorp_RF_all_train[pred_start:pred_end])
    IGCorp_actual = IGCorp_train[pred_start:pred_end]
    IGCorp_return = IGCorp_actual.values
    IGCorp_predictions.append(IGCorp_preds[11])
    IGCorp_actuals.append(IGCorp_return[11])
    all_IGCorp_preds.append(IGCorp_preds)
    print('IGCorp rf preds:',IGCorp_predictions)
    print('IGCorp actual:',IGCorp_actuals)
    print('IGCorp r2',r2_score(IGCorp_return,IGCorp_preds))
    print('IGCorp rf RMSE:', np.sqrt(metrics.mean_squared_error(IGCorp_return,IGCorp_preds)))
    
    #GovtRelated
    feature_list = (all_train.columns)
    GovtRelated_rf.fit(all_train[iStart:iEnd],GovtRelated_train[iStart:iEnd])
    importances = list(GovtRelated_rf.feature_importances_)
    # List of tuples with variable and importance
    feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
    # Sort the feature importances by most important first
    feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
    # Print out the feature and importances 
    #GovtRelated_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
    [print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:10]]
    
    print(GovtRelated_rf.predict(all_train[pred_start:pred_end]))

    #Top Ten Factors
    GovtRelated_RF_top10 = [x[0] for x in feature_importances[0:10]]
    GovtRelated_RF_top10
    list_10_GovtRelated.append(GovtRelated_RF_top10)

    #Make Top Ten DF
    GovtRelated_RF_all_train = all_train[GovtRelated_RF_top10]
    GovtRelated_RF_all_train.shape
    
    #Fit Top Ten
    #GovtRelated_rf.fit(GovtRelated_RF_all_train[iStart:iEnd],GovtRelated_train[iStart:iEnd])
    
    GovtRelated_preds = GovtRelated_rf.predict(all_train[pred_start:pred_end])
    #GovtRelated_preds = GovtRelated_rf.predict(GovtRelated_RF_all_train[pred_start:pred_end])
    GovtRelated_actual = GovtRelated_train[pred_start:pred_end]
    GovtRelated_return = GovtRelated_actual.values
    GovtRelated_predictions.append(GovtRelated_preds[11])
    GovtRelated_actuals.append(GovtRelated_return[11])
    all_GovtRelated_preds.append(GovtRelated_preds)
    print('GovtRelated rf preds:',GovtRelated_predictions)
    print('GovtRelated actual:',GovtRelated_actuals)
    print('GovtRelated r2',r2_score(GovtRelated_return,GovtRelated_preds))
    print('GovtRelated rf RMSE:', np.sqrt(metrics.mean_squared_error(GovtRelated_return,GovtRelated_preds)))
    
    #USHighYield
    feature_list = (all_train.columns)
    USHighYield_rf.fit(all_train[iStart:iEnd],USHighYield_train[iStart:iEnd])
    importances = list(USHighYield_rf.feature_importances_)
    # List of tuples with variable and importance
    feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
    # Sort the feature importances by most important first
    feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
    # Print out the feature and importances 
    #USHighYield_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
    [print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:10]]
    
    print(USHighYield_rf.predict(all_train[pred_start:pred_end]))

    #Top Ten Factors
    USHighYield_RF_top10 = [x[0] for x in feature_importances[0:10]]
    USHighYield_RF_top10
    list_10_USHighYield.append(USHighYield_RF_top10)

    #Make Top Ten DF
    USHighYield_RF_all_train = all_train[USHighYield_RF_top10]
    USHighYield_RF_all_train.shape
    
    #Fit Top Ten
    #USHighYield_rf.fit(USHighYield_RF_all_train[iStart:iEnd],USHighYield_train[iStart:iEnd])
    

    USHighYield_preds = USHighYield_rf.predict(all_train[pred_start:pred_end])
    #USHighYield_preds = USHighYield_rf.predict(USHighYield_RF_all_train[pred_start:pred_end])
    USHighYield_actual = USHighYield_train[pred_start:pred_end]
    USHighYield_return = USHighYield_actual.values
    USHighYield_predictions.append(USHighYield_preds[11])
    USHighYield_actuals.append(USHighYield_return[11])
    all_USHighYield_preds.append(USHighYield_preds)
    print('USHighYield rf preds:',USHighYield_predictions)
    print('USHighYield actual:',USHighYield_actuals)
    print('USHighYield r2',r2_score(USHighYield_return,USHighYield_preds))
    print('USHighYield rf RMSE:', np.sqrt(metrics.mean_squared_error(USHighYield_return,USHighYield_preds)))
    
#Years with results    
years = [2011,2012,2013,2014,2015,2016]

#Dataframes to hold results
ABS_performance = pd.DataFrame({'Year':years,'ABS_preds': ABS_predictions,'ABS_actuals': ABS_actuals})
MBS_performance = pd.DataFrame({'Year':years,'MBS_preds': MBS_predictions,'MBS_actuals': MBS_actuals})
CMBS_performance = pd.DataFrame({'Year':years,'CMBS_preds': CMBS_predictions,'CMBS_actuals': CMBS_actuals})
IGCorp_performance = pd.DataFrame({'Year':years,'IGCorp_preds': IGCorp_predictions,'IGCorp_actuals': IGCorp_actuals})
GovtRelated_performance = pd.DataFrame({'Year':years,'GovtRelated_preds': GovtRelated_predictions,'GovtRelated_actuals': GovtRelated_actuals})
USHighYield_performance = pd.DataFrame({'Year':years,'USHighYield_preds': USHighYield_predictions,'USHighYield_actuals': USHighYield_actuals})

#Predictions and Actuals
performance = pd.DataFrame({'Year':years,'ABS_preds': ABS_predictions,'ABS_actuals': ABS_actuals,'MBS_preds': MBS_predictions,'MBS_actuals': MBS_actuals,
'CMBS_preds': CMBS_predictions,'CMBS_actuals': CMBS_actuals,'IGCorp_preds': IGCorp_predictions,'IGCorp_actuals': IGCorp_actuals,
'GovtRelated_preds': GovtRelated_predictions,'GovtRelated_actuals': GovtRelated_actuals,'USHighYield_preds': USHighYield_predictions,'USHighYield_actuals': USHighYield_actuals})

#Use the year as an index
performance = performance.set_index('Year')
#Create rankings based on highest return
performance.rank(axis=0, ascending=False)
#Flip the axis
performance.transpose()

#Compare the next two dataframes 
#Predictions
all_predictions = pd.DataFrame({'Year':years,'ABS': ABS_predictions,'MBS': MBS_predictions,'CMBS': CMBS_predictions,
'IGCorp': IGCorp_predictions,'GovtRelated': GovtRelated_predictions,'USHighYield': USHighYield_predictions})
all_predictions = all_predictions.set_index('Year')
all_predictions = all_predictions.transpose()
predicted_rankings = all_predictions.rank(axis=0, method='dense',ascending=False).astype('int64')
predicted_rankings.to_csv('rf_rankings.csv', sep=',')
all_predictions.to_csv('rf_preds.csv', sep=',')

#Monthly Predictions
mp = pd.DataFrame({'Year':years,'ABS': all_ABS_preds,'MBS': all_MBS_preds,'CMBS': all_CMBS_preds,
'IGCorp': all_IGCorp_preds,'GovtRelated': all_GovtRelated_preds,'USHighYield': all_USHighYield_preds})
monthly_predictions = pd.DataFrame({'Year':np.repeat(tf.Year.values,mp.ABS.str.len()),'ABS': list(chain.from_iterable(mp.ABS)),'MBS': list(chain.from_iterable(mp.MBS)),
'CMBS': list(chain.from_iterable(mp.CMBS)), 'IGCorp': list(chain.from_iterable(mp.IGCorp)), 'GovtRelated': list(chain.from_iterable(mp.GovtRelated)),'USHighYield': list(chain.from_iterable(mp.USHighYield))})
monthly_predictions = monthly_predictions.set_index('Year')
monthly_predictions.to_csv('rf_monthly_preds.csv', sep=',')

#Top Factors
tf = pd.DataFrame({'Year':years,'ABS': list_10_ABS,'MBS': list_10_MBS,'CMBS': list_10_CMBS,
'IGCorp': list_10_IGCorp, 'GovtRelated': list_10_GovtRelated,'USHighYield': list_10_USHighYield})
#Unpack List into Rows
top_factors = pd.DataFrame({'Year':np.repeat(tf.Year.values,tf.ABS.str.len()),'ABS': list(chain.from_iterable(tf.ABS)),'MBS': list(chain.from_iterable(tf.MBS)),
'CMBS': list(chain.from_iterable(tf.CMBS)), 'IGCorp': list(chain.from_iterable(tf.IGCorp)), 'GovtRelated': list(chain.from_iterable(tf.GovtRelated)),'USHighYield': list(chain.from_iterable(tf.USHighYield))})
top_factors = top_factors.set_index('Year')
top_factors.to_csv('RandomForest_topten.csv', sep=',')

#Actuals
all_actuals = pd.DataFrame({'Year':years, 'ABS': ABS_actuals,'MBS': MBS_actuals,'CMBS': CMBS_actuals,
'IGCorp': IGCorp_actuals, 'GovtRelated': GovtRelated_actuals,'USHighYield': USHighYield_actuals})
all_actuals = all_actuals.set_index('Year')
all_actuals = all_actuals.transpose()
actual_rankings = all_actuals.rank(axis=0, ascending=False).astype('int64')
####################

#########################
#Gradient Boosting

window=60
#Establish Models

params = {'n_estimators': 1000, 'max_depth': 4, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls'}
ABS_gbm = ensemble.GradientBoostingRegressor(**params)
MBS_gbm = ensemble.GradientBoostingRegressor(**params)
CMBS_gbm = ensemble.GradientBoostingRegressor(**params)
IGCorp_gbm = ensemble.GradientBoostingRegressor(**params)
GovtRelated_gbm = ensemble.GradientBoostingRegressor(**params)
USHighYield_gbm = ensemble.GradientBoostingRegressor(**params)

#Window to test
window_test = 12

iStart = 0
i = 1

#Always copy and run from this point forward, don't start at the for loop.

#Setting empty lists to hold results
ABS_predictions = []
ABS_actuals = []
MBS_predictions = []
MBS_actuals = []
CMBS_predictions = []
CMBS_actuals = []
IGCorp_predictions = []
IGCorp_actuals = []
GovtRelated_predictions = []
GovtRelated_actuals = []
USHighYield_predictions = []
USHighYield_actuals = []

all_ABS_preds = []
all_MBS_preds = []
all_CMBS_preds = []
all_IGCorp_preds = []
all_GovtRelated_preds = []
all_USHighYield_preds = []
list_10_ABS = []
list_10_MBS = []
list_10_CMBS = []
list_10_IGCorp = []
list_10_GovtRelated = []
list_10_USHighYield = []

#Loop through the dataset fitting a model based on 5 years
#Skip a Year, then test on the following year
#Take the December return of that year
#Store it 
for iStart in range(0, len(all_train),window_test):
    iEnd = iStart+window + 1
    iStart = iStart + 1
    if iEnd == 133:
        break
    print('training:',iStart,iEnd)

    pred_start = iEnd + window_test
    pred_end = pred_start + window_test
    print('test:', pred_start, pred_end)
    
    param_grid = {
        'learning_rate': [.001, .01, .1],
        'max_depth': [4, 7, 9],
        'loss': ['ls'],
        'min_samples_split': [2, 4, 7],
        'n_estimators': [500, 1000, 1600]
    }

    gbm = ensemble.GradientBoostingRegressor(warm_start= True, random_state=50)
    '''

    # Instantiate the grid search model
    ABS_grid_search = GridSearchCV(estimator = gbm, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
    ABS_grid_search.fit(all_train[iStart:iEnd],ABS_train[iStart:iEnd])
    ABS_grid_search.best_params_
    ABS_gbm = ensemble.GradientBoostingRegressor(**ABS_grid_search.best_params_,random_state=50)
    
    MBS_grid_search = GridSearchCV(estimator = gbm, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
    MBS_grid_search.fit(all_train[iStart:iEnd],MBS_train[iStart:iEnd])
    MBS_grid_search.best_params_
    MBS_gbm = ensemble.GradientBoostingRegressor(**MBS_grid_search.best_params_,random_state=50)
    
    CMBS_grid_search = GridSearchCV(estimator = gbm, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
    CMBS_grid_search.fit(all_train[iStart:iEnd],CMBS_train[iStart:iEnd])
    CMBS_grid_search.best_params_
    CMBS_gbm = ensemble.GradientBoostingRegressor(**CMBS_grid_search.best_params_,random_state=50)
    
    IGCorp_grid_search = GridSearchCV(estimator = gbm, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
    IGCorp_grid_search.fit(all_train[iStart:iEnd],IGCorp_train[iStart:iEnd])
    IGCorp_grid_search.best_params_
    IGCorp_gbm = ensemble.GradientBoostingRegressor(**IGCorp_grid_search.best_params_,random_state=50)
    
    GovtRelated_grid_search = GridSearchCV(estimator = gbm, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
    GovtRelated_grid_search.fit(all_train[iStart:iEnd],GovtRelated_train[iStart:iEnd])
    GovtRelated_grid_search.best_params_
    GovtRelated_gbm = ensemble.GradientBoostingRegressor(**GovtRelated_grid_search.best_params_,random_state=50)
    
    USHighYield_grid_search = GridSearchCV(estimator = gbm, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
    USHighYield_grid_search.fit(all_train[iStart:iEnd],USHighYield_train[iStart:iEnd])
    USHighYield_grid_search.best_params_
    USHighYield_gbm = ensemble.GradientBoostingRegressor(**USHighYield_grid_search.best_params_,random_state=50)


    ABS_gbm = ensemble.GradientBoostingRegressor(**ABS_grid_search.best_params_)
    MBS_gbm = ensemble.GradientBoostingRegressor(**MBS_grid_search.best_params_)
    CMBS_gbm = ensemble.GradientBoostingRegressor(**CMBS_grid_search.best_params_)
    IGCorp_gbm = ensemble.GradientBoostingRegressor(**IGCorp_grid_search.best_params_)
    GovtRelated_gbm = ensemble.GradientBoostingRegressor(**GovtRelated_grid_search.best_params_)
    USHighYield_gbm = ensemble.GradientBoostingRegressor(**USHighYield_grid_search.best_params_)

    #ABS_gbm.fit(all_train[iStart:iEnd],ABS_train[iStart:iEnd])
    #print(ABS_gbm.predict(all_train[pred_start:pred_end]))
    '''
    #ABS
    feature_list = (all_train.columns)
    ABS_gbm.fit(all_train[iStart:iEnd],ABS_train[iStart:iEnd])
    importances = list(ABS_gbm.feature_importances_)
    # List of tuples with variable and importance
    feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
    # Sort the feature importances by most important first
    feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
    # Print out the feature and importances 
    #ABS_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
    [print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:10]]

    #Top Ten Factors
    ABS_GBM_top10 = [x[0] for x in feature_importances[0:10]]
    ABS_GBM_top10
    list_10_ABS.append(ABS_GBM_top10)

    #Make Top Ten DF
    ABS_GBM_all_train = all_train[ABS_GBM_top10]
    ABS_GBM_all_train.shape
    
    #Fit Top Ten
    #ABS_gbm.fit(ABS_GBM_all_train[iStart:iEnd],ABS_train[iStart:iEnd])
    
    ABS_preds = ABS_gbm.predict(all_train[pred_start:pred_end])
    #ABS_preds = ABS_gbm.predict(ABS_GBM_all_train[pred_start:pred_end])
    ABS_actual = ABS_train[pred_start:pred_end]
    ABS_return = ABS_actual.values
    ABS_predictions.append(ABS_preds[11])
    ABS_actuals.append(ABS_return[11])
    all_ABS_preds.append(ABS_preds)
    print('ABS gbm preds:',ABS_predictions)
    print('ABS actual:',ABS_actuals)
    print('ABS r2',r2_score(ABS_return,ABS_preds))
    print('ABS gbm RMSE:', np.sqrt(metrics.mean_squared_error(ABS_return,ABS_preds)))
 
    #MBS
    feature_list = (all_train.columns)
    MBS_gbm.fit(all_train[iStart:iEnd],MBS_train[iStart:iEnd])
    importances = list(MBS_gbm.feature_importances_)
    feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
    feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
    #MBS_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
    [print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:10]]
    
    print(MBS_gbm.predict(all_train[pred_start:pred_end]))

    #Top Ten Factors
    MBS_GBM_top10 = [x[0] for x in feature_importances[0:10]]
    MBS_GBM_top10
    list_10_MBS.append(MBS_GBM_top10)

    #Make Top Ten DF
    MBS_GBM_all_train = all_train[MBS_GBM_top10]
    MBS_GBM_all_train.shape
    
    #Fit Top Ten
    #MBS_gbm.fit(MBS_GBM_all_train[iStart:iEnd],MBS_train[iStart:iEnd])
    
    MBS_preds = MBS_gbm.predict(all_train[pred_start:pred_end])
    #MBS_preds = MBS_gbm.predict(MBS_GBM_all_train[pred_start:pred_end])
    MBS_actual = MBS_train[pred_start:pred_end]
    MBS_return = MBS_actual.values
    MBS_predictions.append(MBS_preds[11])
    MBS_actuals.append(MBS_return[11])
    all_MBS_preds.append(MBS_preds)
    print('MBS gbm preds:',MBS_predictions)
    print('MBS actual:',MBS_actuals)
    print('MBS r2',r2_score(MBS_return,MBS_preds))
    print('MBS gbm RMSE:', np.sqrt(metrics.mean_squared_error(MBS_return,MBS_preds)))
    
    #CMBS
    feature_list = (all_train.columns)
    CMBS_gbm.fit(all_train[iStart:iEnd],CMBS_train[iStart:iEnd])
    importances = list(CMBS_gbm.feature_importances_)
    feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
    feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
    #CMBS_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
    [print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:10]]
    print(CMBS_gbm.predict(all_train[pred_start:pred_end]))

    #Top Ten Factors
    CMBS_GBM_top10 = [x[0] for x in feature_importances[0:10]]
    CMBS_GBM_top10
    list_10_CMBS.append(CMBS_GBM_top10)

    #Make Top Ten DF
    CMBS_GBM_all_train = all_train[CMBS_GBM_top10]
    CMBS_GBM_all_train.shape
    
    #Fit Top Ten
    #CMBS_gbm.fit(CMBS_GBM_all_train[iStart:iEnd],CMBS_train[iStart:iEnd])
    
    CMBS_preds = CMBS_gbm.predict(all_train[pred_start:pred_end])
    #CMBS_preds = CMBS_gbm.predict(CMBS_GBM_all_train[pred_start:pred_end])
    CMBS_actual = CMBS_train[pred_start:pred_end]
    CMBS_return = CMBS_actual.values
    CMBS_predictions.append(CMBS_preds[11])
    CMBS_actuals.append(CMBS_return[11])
    all_CMBS_preds.append(CMBS_preds)
    print('CMBS gbm preds:',CMBS_predictions)
    print('CMBS actual:',CMBS_actuals)
    print('CMBS r2',r2_score(CMBS_return,CMBS_preds))
    print('CMBS gbm RMSE:', np.sqrt(metrics.mean_squared_error(CMBS_return,CMBS_preds)))
    
    #IGCorp
    feature_list = (all_train.columns)
    IGCorp_gbm.fit(all_train[iStart:iEnd],IGCorp_train[iStart:iEnd])
    importances = list(IGCorp_gbm.feature_importances_)
    feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
    feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
    #IGCorp_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
    [print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:10]]
    
    print(IGCorp_gbm.predict(all_train[pred_start:pred_end]))

    #Top Ten Factors
    IGCorp_GBM_top10 = [x[0] for x in feature_importances[0:10]]
    IGCorp_GBM_top10
    list_10_IGCorp.append(IGCorp_GBM_top10)

    #Make Top Ten DF
    IGCorp_GBM_all_train = all_train[IGCorp_GBM_top10]
    IGCorp_GBM_all_train.shape
    
    #Fit Top Ten
    #IGCorp_gbm.fit(IGCorp_GBM_all_train[iStart:iEnd],IGCorp_train[iStart:iEnd])
    
    IGCorp_preds = IGCorp_gbm.predict(all_train[pred_start:pred_end])
    #IGCorp_preds = IGCorp_gbm.predict(IGCorp_GBM_all_train[pred_start:pred_end])
    IGCorp_actual = IGCorp_train[pred_start:pred_end]
    IGCorp_return = IGCorp_actual.values
    IGCorp_predictions.append(IGCorp_preds[11])
    IGCorp_actuals.append(IGCorp_return[11])
    all_IGCorp_preds.append(IGCorp_preds)
    print('IGCorp gbm preds:',IGCorp_predictions)
    print('IGCorp actual:',IGCorp_actuals)
    print('IGCorp r2',r2_score(IGCorp_return,IGCorp_preds))
    print('IGCorp gbm RMSE:', np.sqrt(metrics.mean_squared_error(IGCorp_return,IGCorp_preds)))
    
    #GovtRelated
    feature_list = (all_train.columns)
    GovtRelated_gbm.fit(all_train[iStart:iEnd],GovtRelated_train[iStart:iEnd])
    importances = list(GovtRelated_gbm.feature_importances_)
    feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
    feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
    #GovtRelated_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
    [print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:10]]
    print(GovtRelated_gbm.predict(all_train[pred_start:pred_end]))

    #Top Ten Factors
    GovtRelated_GBM_top10 = [x[0] for x in feature_importances[0:10]]
    GovtRelated_GBM_top10
    list_10_GovtRelated.append(GovtRelated_GBM_top10)

    #Make Top Ten DF
    GovtRelated_GBM_all_train = all_train[GovtRelated_GBM_top10]
    GovtRelated_GBM_all_train.shape
    
    #Fit Top Ten
    #GovtRelated_gbm.fit(GovtRelated_GBM_all_train[iStart:iEnd],GovtRelated_train[iStart:iEnd])
    
    GovtRelated_preds = GovtRelated_gbm.predict(all_train[pred_start:pred_end])
    #GovtRelated_preds = GovtRelated_gbm.predict(GovtRelated_GBM_all_train[pred_start:pred_end])
    GovtRelated_actual = GovtRelated_train[pred_start:pred_end]
    GovtRelated_return = GovtRelated_actual.values
    GovtRelated_predictions.append(GovtRelated_preds[11])
    GovtRelated_actuals.append(GovtRelated_return[11])
    all_GovtRelated_preds.append(GovtRelated_preds)
    print('GovtRelated gbm preds:',GovtRelated_predictions)
    print('GovtRelated actual:',GovtRelated_actuals)
    print('GovtRelated r2',r2_score(GovtRelated_return,GovtRelated_preds))
    print('GovtRelated gbm RMSE:', np.sqrt(metrics.mean_squared_error(GovtRelated_return,GovtRelated_preds)))
    
    #USHighYield
    feature_list = (all_train.columns)
    USHighYield_gbm.fit(all_train[iStart:iEnd],USHighYield_train[iStart:iEnd])
    importances = list(USHighYield_gbm.feature_importances_)
    feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
    feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
    #USHighYield_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
    [print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:10]]
    print(USHighYield_gbm.predict(all_train[pred_start:pred_end]))

    #Top Ten Factors
    USHighYield_GBM_top10 = [x[0] for x in feature_importances[0:10]]
    USHighYield_GBM_top10
    list_10_USHighYield.append(USHighYield_GBM_top10)

    #Make Top Ten DF
    USHighYield_GBM_all_train = all_train[USHighYield_GBM_top10]
    USHighYield_GBM_all_train.shape
    
    #Fit Top Ten
    #USHighYield_gbm.fit(USHighYield_GBM_all_train[iStart:iEnd],USHighYield_train[iStart:iEnd])

    USHighYield_preds = USHighYield_gbm.predict(all_train[pred_start:pred_end])
    #USHighYield_preds = USHighYield_gbm.predict(USHighYield_GBM_all_train[pred_start:pred_end])
    USHighYield_actual = USHighYield_train[pred_start:pred_end]
    USHighYield_return = USHighYield_actual.values
    USHighYield_predictions.append(USHighYield_preds[11])
    USHighYield_actuals.append(USHighYield_return[11])
    all_USHighYield_preds.append(USHighYield_preds)
    print('USHighYield gbm preds:',USHighYield_predictions)
    print('USHighYield actual:',USHighYield_actuals)
    print('USHighYield r2',r2_score(USHighYield_return,USHighYield_preds))
    print('USHighYield gbm RMSE:', np.sqrt(metrics.mean_squared_error(USHighYield_return,USHighYield_preds)))
    
#Years with results    
years = [2011,2012,2013,2014,2015,2016]

#Dataframes to hold results
ABS_performance = pd.DataFrame({'Year':years,'ABS_preds': ABS_predictions,'ABS_actuals': ABS_actuals})
MBS_performance = pd.DataFrame({'Year':years,'MBS_preds': MBS_predictions,'MBS_actuals': MBS_actuals})
CMBS_performance = pd.DataFrame({'Year':years,'CMBS_preds': CMBS_predictions,'CMBS_actuals': CMBS_actuals})
IGCorp_performance = pd.DataFrame({'Year':years,'IGCorp_preds': IGCorp_predictions,'IGCorp_actuals': IGCorp_actuals})
GovtRelated_performance = pd.DataFrame({'Year':years,'GovtRelated_preds': GovtRelated_predictions,'GovtRelated_actuals': GovtRelated_actuals})
USHighYield_performance = pd.DataFrame({'Year':years,'USHighYield_preds': USHighYield_predictions,'USHighYield_actuals': USHighYield_actuals})

#Predictions and Actuals
performance = pd.DataFrame({'Year':years,'ABS_preds': ABS_predictions,'ABS_actuals': ABS_actuals,'MBS_preds': MBS_predictions,'MBS_actuals': MBS_actuals,
'CMBS_preds': CMBS_predictions,'CMBS_actuals': CMBS_actuals,'IGCorp_preds': IGCorp_predictions,'IGCorp_actuals': IGCorp_actuals,
'GovtRelated_preds': GovtRelated_predictions,'GovtRelated_actuals': GovtRelated_actuals,'USHighYield_preds': USHighYield_predictions,'USHighYield_actuals': USHighYield_actuals})

#Use the year as an index
performance = performance.set_index('Year')
#Create rankings based on highest return
performance.rank(axis=0, ascending=False)
#Flip the axis
performance.transpose()

#Compare the next two dataframes 
#Predictions
all_predictions = pd.DataFrame({'Year':years,'ABS': ABS_predictions,'MBS': MBS_predictions,'CMBS': CMBS_predictions,
'IGCorp': IGCorp_predictions,'GovtRelated': GovtRelated_predictions,'USHighYield': USHighYield_predictions})
all_predictions = all_predictions.set_index('Year')
all_predictions = all_predictions.transpose()
predicted_rankings = all_predictions.rank(axis=0, method='dense',ascending=False).astype('int64')
predicted_rankings.to_csv('gbm_rankings.csv', sep=',')
all_predictions.to_csv('gbm_preds.csv', sep=',')

#Monthly Predictions
mp = pd.DataFrame({'Year':years,'ABS': all_ABS_preds,'MBS': all_MBS_preds,'CMBS': all_CMBS_preds,
'IGCorp': all_IGCorp_preds,'GovtRelated': all_GovtRelated_preds,'USHighYield': all_USHighYield_preds})
monthly_predictions = pd.DataFrame({'Year':np.repeat(tf.Year.values,mp.ABS.str.len()),'ABS': list(chain.from_iterable(mp.ABS)),'MBS': list(chain.from_iterable(mp.MBS)),
'CMBS': list(chain.from_iterable(mp.CMBS)), 'IGCorp': list(chain.from_iterable(mp.IGCorp)), 'GovtRelated': list(chain.from_iterable(mp.GovtRelated)),'USHighYield': list(chain.from_iterable(mp.USHighYield))})
monthly_predictions = monthly_predictions.set_index('Year')
monthly_predictions.to_csv('gbm_monthly_preds.csv', sep=',')

#Top Factors
tf = pd.DataFrame({'Year':years,'ABS': list_10_ABS,'MBS': list_10_MBS,'CMBS': list_10_CMBS,
'IGCorp': list_10_IGCorp, 'GovtRelated': list_10_GovtRelated,'USHighYield': list_10_USHighYield})
#Unpack List into Rows
top_factors = pd.DataFrame({'Year':np.repeat(tf.Year.values,tf.ABS.str.len()),'ABS': list(chain.from_iterable(tf.ABS)),'MBS': list(chain.from_iterable(tf.MBS)),
'CMBS': list(chain.from_iterable(tf.CMBS)), 'IGCorp': list(chain.from_iterable(tf.IGCorp)), 'GovtRelated': list(chain.from_iterable(tf.GovtRelated)),'USHighYield': list(chain.from_iterable(tf.USHighYield))})
top_factors = top_factors.set_index('Year')
top_factors.to_csv('GradientBoosting_topten.csv', sep=',')

#Actuals
all_actuals = pd.DataFrame({'Year':years, 'ABS': ABS_actuals,'MBS': MBS_actuals,'CMBS': CMBS_actuals,
'IGCorp': IGCorp_actuals, 'GovtRelated': GovtRelated_actuals,'USHighYield': USHighYield_actuals})
all_actuals = all_actuals.set_index('Year')
all_actuals = all_actuals.transpose()
actual_rankings = all_actuals.rank(axis=0, ascending=False).astype('int64')




