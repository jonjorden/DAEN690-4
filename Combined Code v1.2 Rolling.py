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

all_train = all_train.reset_index()
all_train = all_train.drop('Value Date', axis=1)

all_test = all_test.reset_index()
all_test = all_test.drop('Value Date', axis=1)
import pandas as pd
from sklearn import linear_model

model_ols = linear_model.LinearRegression()
window=2

for iStart in range(0, len(all_train)-window):        
    iEnd = iStart+window

    model_ols.fit(all_train[iStart:iEnd], ABS_train[iStart:iEnd])

    #write/store output
    print(iEnd)
    print(model_ols.coef_)

    #Predict
    pred_nextyr = iEnd + window
    pred_forward = pred_nextyr
    #model_ols.predict(all_train[pred_nextyr:pred_forward])
    
for iStart in range(0, len(all_test)-window):        
    iEnd = iStart+window
    lm_preds = model_ols.predict(all_test[iStart:iEnd])
    print(iStart,iEnd)
    print(lm_preds[1])
    
