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

# list of x locations for plotting
x_values = list(range(len(importances)))
# Make a bar chart
plt.bar(x_values, importances, orientation = 'vertical', color = 'r', edgecolor = 'k', linewidth = 1.2)
# Tick labels for x axis
plt.xticks(x_values, feature_list, rotation='vertical')
# Axis labels and title
plt.ylabel('Importance'); plt.xlabel('Variable'); plt.title('Variable Importances');

# List of features sorted from most to least important
sorted_importances = [importance[1] for importance in feature_importances]
sorted_features = [importance[0] for importance in feature_importances]
# Cumulative importances
cumulative_importances = np.cumsum(sorted_importances)
# Make a line graph
plt.plot(x_values, cumulative_importances, 'g-')
# Draw line at 95% of importance retained
plt.hlines(y = 0.95, xmin=0, xmax=len(sorted_importances), color = 'r', linestyles = 'dashed')
# Format x ticks and labels
plt.xticks(x_values, sorted_features, rotation = 'vertical')
# Axis labels and title
plt.xlabel('Variable'); plt.ylabel('Cumulative Importance'); plt.title('Cumulative Importances');


####  Random Forest CV 
# Number of trees in random forest
n_estimators = [int(x) for x in np.linspace(start = 200, stop = 2000, num = 10)]
# Number of features to consider at every split
max_features = ['auto', 'sqrt']
# Maximum number of levels in tree
max_depth = [int(x) for x in np.linspace(10, 110, num = 11)]
max_depth.append(None)
# Minimum number of samples required to split a node
min_samples_split = [2, 5, 10]
# Minimum number of samples required at each leaf node
min_samples_leaf = [1, 2, 4]
# Method of selecting samples for training each tree
bootstrap = [True, False]
# Create the random grid
random_grid = {'n_estimators': n_estimators,
               'max_features': max_features,
               'max_depth': max_depth,
               'min_samples_split': min_samples_split,
               'min_samples_leaf': min_samples_leaf,
               'bootstrap': bootstrap}
print(random_grid)

# Random search of parameters, using 3 fold cross validation, 
# search across 100 different combinations, and use all available cores
rf_random = RandomizedSearchCV(estimator = rf, param_distributions = random_grid, n_iter = 100, cv = 3, verbose=2, random_state=42, n_jobs = -1)

# Fit the random search model
rf_random.fit(topten_ABS_all_train, ABS_train)

#View Best Params
rf_random.best_params_

# Create the parameter grid based on the results of random search 
param_grid = {
    'bootstrap': [True],
    'max_depth': [20, 30, 50, 80, 100],
    'max_features': ['sqrt'],
    'min_samples_leaf': [1, 2, 3],
    'min_samples_split': [2, 4, 7],
    'n_estimators': [100, 120, 200, 400]
}

rf = RandomForestRegressor()

# Instantiate the grid search model
grid_search = GridSearchCV(estimator = rf, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)
grid_search.fit(topten_ABS_all_train, ABS_train)
grid_search.best_params_

#Make Prediction
rf_grid_search = grid_search.predict(topten_ABS_all_test)

date_keys = ABS_test.index.values
ABS_test = pd.DataFrame(ABS_test)
ABS_rf_results = pd.DataFrame(rf_grid_search)
ABS_rf_results = ABS_rf_results.set_index(date_keys)

#Plot Prediction vs Actual
month = [1,2,3,4,5,6,7,8,9,10,11,12]
plt.style.use('seaborn-darkgrid')
plt.plot(month,ABS_test['ABS_12M'].values, color='blue',linewidth=3)
plt.plot(month, ABS_rf_results[0].values, marker='',color='red', linewidth=4)
plt.xlabel('Months in 2016')
plt.ylabel('Excess Returns')
plt.title('ABS Random Forest: Preds vs Actual')
plt.legend()
plt.show()
print('MAE:', metrics.mean_absolute_error(ABS_test, rf_grid_search))
print('MSE:', metrics.mean_squared_error(ABS_test, rf_grid_search))
print('RMSE:', np.sqrt(metrics.mean_squared_error(ABS_test, rf_grid_search)))


#SKLearn Linear Model
lm = LinearRegression()
lm_model = lm.fit(all_train,ABS_train)

#Make Prediction
lm_predictions = lm.predict(all_test)

date_keys = ABS_test.index.values
ABS_test = pd.DataFrame(ABS_test)
ABS_lm_results = pd.DataFrame(lm_predictions)
ABS_lm_results = ABS_lm_results.set_index(date_keys)

#Plot Prediction vs Actual
month = [1,2,3,4,5,6,7,8,9,10,11,12]
plt.style.use('seaborn-darkgrid')
plt.plot(month,ABS_test['ABS_12M'].values, color='blue',linewidth=3)
plt.plot(month, ABS_lm_results[0].values, marker='',color='red', linewidth=4)
plt.xlabel('Months in 2016')
plt.ylabel('Excess Returns')
plt.title('ABS Linear Regression: Preds vs Actual')
plt.legend()
plt.show()

#Model Statistics
print('R2:',r2_score(ABS_test,lm_predictions))
print('MAE:', metrics.mean_absolute_error(ABS_test, lm_predictions))
print('MSE:', metrics.mean_squared_error(ABS_test, lm_predictions))
print('RMSE:', np.sqrt(metrics.mean_squared_error(ABS_test, lm_predictions)))
