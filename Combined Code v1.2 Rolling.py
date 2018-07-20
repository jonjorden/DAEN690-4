# Load Fundamental Libraries
import csv
import numpy as np
import pandas as pd
import sklearn
from datetime import datetime
import seaborn as sns

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


# Time Series Libraries
from pandas.tools.plotting import autocorrelation_plot
from statsmodels.tsa.stattools import acf, pacf
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.arima_model import ARIMA
from statsmodels.tsa.stattools import adfuller


#Read Datafile
df = pd.read_csv('Combined_Dataset_v3.csv', parse_dates= ['Value Date'], index_col= "Value Date")
df_pca = pd.read_csv('Combined_Dataset_v3.csv', parse_dates= ['Value Date'])
df2 = pd.read_csv('Combined_Dataset_v3.csv', parse_dates= ['Value Date'])
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

#Training Set for Features
all_train = df_features[(df_features.index > '2011-01-01') & (df_features.index <= '2014-12-31')]
all_train.info()

#Training Set for Response Variables
ABS_train = Y_ABS[(Y_ABS.index > '2011-01-01') & (Y_ABS.index <= '2014-12-31')]
CMBS_train = Y_CMBS[(Y_CMBS.index > '2011-01-01') & (Y_CMBS.index <= '2014-12-31')]
IGCorp_train = Y_IGCorp[(Y_IGCorp.index > '2011-01-01') & (Y_IGCorp.index <= '2014-12-31')]
GovtRelated_train = Y_GovtRelated[(Y_GovtRelated.index > '2011-01-01') & (Y_GovtRelated.index <= '2014-12-31')]
MBS_train = Y_MBS[(Y_MBS.index > '2011-01-01') & (Y_MBS.index <= '2014-12-31')]
USHighYield_train = Y_USHighYield[(Y_USHighYield.index > '2011-01-01') & (Y_USHighYield.index <= '2014-12-31')]


#Test Set for Features
all_test = df_features[(df_features.index > '2016-01-01') & (df_features.index <= '2016-12-31')]

#Test Set for Response Variables
ABS_test = Y_ABS[(Y_ABS.index > '2016-01-01') & (Y_ABS.index <= '2016-12-31')]
CMBS_test = Y_CMBS[(Y_CMBS.index > '2016-01-01') & (Y_CMBS.index <= '2016-12-31')]
IGCorp_test = Y_IGCorp[(Y_IGCorp.index > '2016-01-01') & (Y_IGCorp.index <= '2016-12-31')]
GovtRelated_test = Y_GovtRelated[(Y_GovtRelated.index > '2016-01-01') & (Y_GovtRelated.index <= '2016-12-31')]
MBS_test = Y_MBS[(Y_MBS.index > '2016-01-01') & (Y_MBS.index <= '2016-12-31')]
USHighYield_test = Y_USHighYield[(Y_USHighYield.index > '2016-01-01') & (Y_USHighYield.index <= '2016-12-31')]

###########################################################################

#My Models

#ABS
#Sort data
all_train = all_train.sort_index()
all_test = all_test.sort_index()
ABS_train = ABS_train.sort_index()
ABS_test = ABS_test.sort_index()

from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import RandomizedSearchCV
from sklearn.model_selection import GridSearchCV


#Random Forest Importance
rf = RandomForestRegressor(random_state=50)
rf.fit(all_train,ABS_train)
feature_list = (all_train.columns)

##### Regular Random Forest
# Get numerical feature importances
importances = list(rf.feature_importances_)
# List of tuples with variable and importance
feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
# Sort the feature importances by most important first
feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
# Print out the feature and importances 
#ABS_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
[print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances]

#Top 10 Important Predictors
ABS_top10 = pd.DataFrame(feature_importances, columns=['Variable','Importance'])
ABS_top10.nlargest(10,'Importance',keep='first')

#Make Top Ten Training and Test dfs
topten_ABS_all_train = all_train[['EXUSUK','DSPIC96','USD3MTD156N','TCU','TTLCONS','EXUSEU','PALLFNFINDEXM','EXCAUS','T10Y2Y','HSN1F']]
topten_ABS_all_train.shape

topten_ABS_all_test = all_test[['EXUSUK','DSPIC96','USD3MTD156N','TCU','TTLCONS','EXUSEU','PALLFNFINDEXM','EXCAUS','T10Y2Y','HSN1F']]
topten_ABS_all_test.shape
###############
#Rolling Section Start

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
df_features = df[df.columns[12:14]]
df_features.head()

#Training Set for Features (2005 through 2016)
all_train = df_features[(df_features.index > '2005-01-01') & (df_features.index <= '2016-12-31')]
all_train.info()

#Training Set for Response Variables
ABS_train = Y_ABS[(Y_ABS.index > '2005-01-01') & (Y_ABS.index <= '2016-12-31')]
CMBS_train = Y_CMBS[(Y_CMBS.index > '2005-01-01') & (Y_CMBS.index <= '2016-12-31')]
IGCorp_train = Y_IGCorp[(Y_IGCorp.index > '2005-01-01') & (Y_IGCorp.index <= '2016-12-31')]
GovtRelated_train = Y_GovtRelated[(Y_GovtRelated.index > '2005-01-01') & (Y_GovtRelated.index <= '2016-12-31')]
MBS_train = Y_MBS[(Y_MBS.index > '2005-01-01') & (Y_MBS.index <= '2016-12-31')]
USHighYield_train = Y_USHighYield[(Y_USHighYield.index > '2005-01-01') & (Y_USHighYield.index <= '2016-12-31')]

import pandas as pd
from sklearn import linear_model

model_ols = linear_model.LinearRegression()
window=60

window_test = 11
iStart = 0

for iStart in range(1, len(all_train),window_test):
    iEnd = iStart+window_test
    print(iStart,iEnd)

    model_ols.fit(all_train[iStart:iEnd], ABS_train[iStart:iEnd])

    #write/store output
    #print(iEnd)
    print(model_ols.coef_)

    #Predict
    pred_nextyr = iEnd + window
    pred_forward = pred_nextyr
    #model_ols.predict(all_train[pred_nextyr:pred_forward])
    
for iStart in range(0, len(all_test)-window_test):        
    iEnd = iStart+window
    lm_preds = model_ols.predict(all_test[iStart:iEnd])
    print(iStart,iEnd)
    print(lm_preds[1])
    


#Rolling Section End
############################

#The below code was ran for each sector

#IGCorp
#Sort data
all_train = all_train.sort_index()
all_test = all_test.sort.index()
IGCorp_train = IGCorp_train.sort_index()
IGCorp_test = IGCorp_test.sort_index()

#Random Forest Importance
rf = RandomForestRegressor(random_state=50)
rf.fit(all_train,IGCorp_train)
feature_list = (all_train.columns)

##### Regular Random Forest
# Get numerical feature importances
importances = list(rf.feature_importances_)
# List of tuples with variable and importance
feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, importances)]
# Sort the feature importances by most important first
feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
# Print out the feature and importances 
#IGCorp_importance = ['Variable: {:20} Importance: {}'.format(*pair) for pair in feature_importances]
[print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances]

#Top 10 Important Predictors
IGCorp_top10 = pd.DataFrame(feature_importances, columns=['Variable','Importance'])
IGCorp_top10 = IGCorp_top10.nlargest(10,'Importance',keep='first')

IGCorptopten_list = list(IGCorp_top10['Variable'])

#Make Top Ten Training and Test dfs
topten_IGCorp_all_train = all_train[IGCorptopten_list]
topten_IGCorp_all_train.shape

topten_IGCorp_all_test = all_test[IGCorptopten_list]
topten_IGCorp_all_test.shape

# Random search of parameters, using 3 fold cross validation, 
# search across 100 different combinations, and use all available cores
rf_random = RandomizedSearchCV(estimator = rf, param_distributions = random_grid, n_iter = 100, cv = 3, verbose=2, random_state=42, n_jobs = -1)

# Fit the random search model
rf_random.fit(topten_IGCorp_all_train, IGCorp_train)

#View Best Params
rf_random.best_params_

# Create the parameter grid based on the results of random search 
param_grid = {
    'bootstrap': [False],
    'max_depth': [30, 70, 80],
    'max_features': ['sqrt'],
    'min_samples_leaf': [1, 2, 3],
    'min_samples_split': [2, 5, 7],
    'n_estimators': [200, 400, 600, 800]
}

rf = RandomForestRegressor()

# Instantiate the grid search model
grid_search = GridSearchCV(estimator = rf, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
grid_search.fit(topten_IGCorp_all_train, IGCorp_train)
grid_search.best_params_

#Make Prediction
rf_grid_search = grid_search.predict(topten_IGCorp_all_test)

date_keys = IGCorp_test.index.values
IGCorp_test = pd.DataFrame(IGCorp_test)
IGCorp_rf_results = pd.DataFrame(rf_grid_search)
IGCorp_rf_results = IGCorp_rf_results.set_index(date_keys)

#Plot Prediction vs Actual
month = [1,2,3,4,5,6,7,8,9,10,11,12]
plt.style.use('seaborn-darkgrid')
plt.plot(month,IGCorp_test['IGCorp_12M'].values, color='blue',linewidth=3)
plt.plot(month, IGCorp_rf_results[0].values, marker='',color='red', linewidth=4)
plt.xlabel('Months in 2016')
plt.ylabel('Excess Returns')
plt.title('IGCorp Random Forest: Preds vs Actual')
plt.legend()
plt.show()
print('MAE:', metrics.mean_absolute_error(IGCorp_test, rf_grid_search))
print('MSE:', metrics.mean_squared_error(IGCorp_test, rf_grid_search))
print('RMSE:', np.sqrt(metrics.mean_squared_error(IGCorp_test, rf_grid_search)))

#SKLearn Linear Model
lm = LinearRegression()
lm_model = lm.fit(all_train,IGCorp_train)

#Make Prediction
lm_predictions = lm.predict(all_test)

date_keys = IGCorp_test.index.values
IGCorp_test = pd.DataFrame(IGCorp_test)
IGCorp_lm_results = pd.DataFrame(lm_predictions)
IGCorp_lm_results = IGCorp_lm_results.set_index(date_keys)

#Plot Prediction vs Actual
month = [1,2,3,4,5,6,7,8,9,10,11,12]
plt.style.use('seaborn-darkgrid')
plt.plot(month,IGCorp_test['IGCorp_12M'].values, color='blue',linewidth=3)
plt.plot(month, IGCorp_lm_results[0].values, marker='',color='red', linewidth=4)
plt.xlabel('Months in 2016')
plt.ylabel('Excess Returns')
plt.title('IGCorp Linear Regression: Preds vs Actual')
plt.legend()
plt.show()

#Model Statistics
print('R2:',r2_score(IGCorp_test,lm_predictions))
print('MAE:', metrics.mean_absolute_error(IGCorp_test, lm_predictions))
print('MSE:', metrics.mean_squared_error(IGCorp_test, lm_predictions))
print('RMSE:', np.sqrt(metrics.mean_squared_error(IGCorp_test, lm_predictions)))

# Fit on training set only
scaler = StandardScaler()
scaler.fit(all_train)

#Apply transform to both the training set and the test set
all_train_scaled = scaler.transform(all_train)
all_test_scaled = scaler.transform(all_test)

#### Recursive Feature Elimination
from sklearn.feature_selection import RFE
from sklearn.linear_model import LogisticRegression
#Create Binary 1 variable which shows positive or negative return
B_IGCorp_Return = df['IGCorp_B']
B_IGCorp_train = B_IGCorp_Return[(B_IGCorp_Return.index > '2011-01-01') & (B_IGCorp_Return.index <= '2014-12-31')]
all_train.shape
#Sort dates
B_IGCorp_train = B_IGCorp_train.sort_index()
B_IGCorp_train.shape

# feature extraction
RFE_model = LogisticRegression()
rfe = RFE(RFE_model, 10)
recursive_fit = rfe.fit(all_train, B_IGCorp_train)
#print(recursive_fit.n_features_)
#print("Selected Features:",recursive_fit.support_)
print("Feature Ranking: ",recursive_fit.ranking_)
RFE_topten_IGCorp = all_train.iloc[:,recursive_fit.ranking_[:10]].columns
RFE_topten_IGCorp

#LASSO Regression
from sklearn.linear_model import Lasso
from sklearn.linear_model import RandomizedLasso
from sklearn.preprocessing import StandardScaler
#A helper method for pretty-printing linear models
def pretty_print_linear(coefs, names = None, sort = False):
    if names == None:
        names = ["X%s" % x for x in range(len(coefs))]
    lst = zip(coefs, names)
    if sort:
        lst = sorted(lst,  key = lambda x:-np.IGCorp(x[0]))
    return " + ".join("%s * %s" % (round(coef, 3), name)
                                   for coef, name in lst)
 

#Gradient Boosting Model
from sklearn import ensemble
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import KFold

params = {'n_estimators': 1000, 'max_depth': 4, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls'}
clf = ensemble.GradientBoostingRegressor(**params)

clf.fit(all_train,IGCorp_train)
mse = mean_squared_error(IGCorp_test, clf.predict(scaled_all_test))
print("MSE: %.4f" % mse)
print(clf.feature_importances_)
clf_feature_importance = clf.feature_importances_
# Get numerical feature importances
gradient_boost_importances = list(clf.feature_importances_)
# List of tuples with variable and importance
gb_feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(feature_list, gradient_boost_importances)]
# Sort the feature importances by most important first
gb_feature_importances = sorted(gb_feature_importances, key = lambda x: x[1], reverse = True)
# Print out the feature and importances 
[print('Variable: {:20} Importance: {}'.format(*pair)) for pair in gb_feature_importances[0:10]]


IGCorp_GBM_top10 = [x[0] for x in gb_feature_importances[0:10]]
IGCorp_GBM_top10

#Use Grid Search to find best parameters
param_grid = dict(n_estimators=np.array([50,100,200,300,400]))
model = ensemble.GradientBoostingRegressor(random_state=21)
kfold = KFold(n_splits=10, random_state=21)
grid = GridSearchCV(estimator=model, param_grid=param_grid, scoring='neg_mean_squared_error', cv=kfold)
grid_result = grid.fit(all_train, IGCorp_train)

means = grid_result.cv_results_['mean_test_score']
stds = grid_result.cv_results_['std_test_score']
params = grid_result.cv_results_['params']
for mean, stdev, param in zip(means, stds, params):
    print("%f (%f) with: %r" % (mean, stdev, param))

print("Best: %f using %s" % (grid_result.best_score_, grid_result.best_params_))
