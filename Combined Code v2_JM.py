# -------------------------------------------------------------------------------
## OBJECTIVES ##

# What are the coefficients?
# What variables are being used for the model? Understand how the model is working.
# How accurate is the model?
# Which sector is going to be the best performing model? 

# We want to be able to show and back-test which sector will best predict the next 12 months.
# Assign an expected return to each of the models.

# Can you rank order these models to the expected return?
# Is there a relationship between the expected and actual return? Yes or No
# Is our model useful in assessing the performance of our returns?

# Calculate the predictions of the future 12-month excess returns for each sector.
# Rank which sectors have the highest predicted 12-month excess returns (1-6).

# All these are independent from linear regression. 
# The output from the regression would be the expected return. Beta coefficient times the X value.
# Compare the expected returns.
# Rank from highest to lowest.
# What is the actual forward return of rank #1, #2, #3, #4, #5, and #6?

# Reset every month and conduct a rolling window of your back test.
# Step forward one year or one month.
# Dependent variable is the new expected return.
# Tie the realized returns to each of the ranks.
# Determine whether the model adds any value.

# Use 5 years as a training 2005-2010; test on 2012
# Use 2006-2011; test on 2013... so on, until 2017

# -------------------------------------------------------------------------------
# Loading Fundamental Libraries
import csv
import numpy as np
import pandas as pd
import sklearn
from datetime import datetime
import seaborn as sns

# Importing Visualization Libraries
import matplotlib.pyplot as plt
sns.set(style="whitegrid", font_scale=1)
%matplotlib inline

# -------------------------------------------------------------------------------
# Read Data File
df = pd.read_csv('Combined_Dataset_v3.csv', parse_dates= ['Value_Date'], index_col= "Value_Date")
df_pca = pd.read_csv('Combined_Dataset_v3.csv', parse_dates= ['Value_Date'])
# df2 = pd.read_csv('Combined_Dataset_v3.csv', parse_dates= ['Value_Date'])
df.head()

# Declare Response Variables
Y_ABS = df['ABS_12M']
Y_CMBS = df['CMBS_12M']
Y_IGCorp = df['IGCorp_12M']
Y_MBS = df['MBS_12M']
Y_USHighYield = df['USHighYield_12M']
Y_GovtRelated = df['GovtRelated_12M']

# Declaring Explanatory Variables
df_features = df[df.columns[12:75]]
df_features.head()

# Training Set for Features (2011 through 2014)
all_train = df_features[(df_features.index > '2011-01-01') & (df_features.index <= '2014-12-31')]
all_train.info()

# Training Set for Response Variables
ABS_train = Y_ABS[(Y_ABS.index > '2011-01-01') & (Y_ABS.index <= '2014-12-31')]
CMBS_train = Y_CMBS[(Y_CMBS.index > '2011-01-01') & (Y_CMBS.index <= '2014-12-31')]
IGCorp_train = Y_IGCorp[(Y_IGCorp.index > '2011-01-01') & (Y_IGCorp.index <= '2014-12-31')]
GovtRelated_train = Y_GovtRelated[(Y_GovtRelated.index > '2011-01-01') & (Y_GovtRelated.index <= '2014-12-31')]
MBS_train = Y_MBS[(Y_MBS.index > '2011-01-01') & (Y_MBS.index <= '2014-12-31')]
USHighYield_train = Y_USHighYield[(Y_USHighYield.index > '2011-01-01') & (Y_USHighYield.index <= '2014-12-31')]

# Test Set for Features
all_test = df_features[(df_features.index > '2016-01-01') & (df_features.index <= '2016-12-31')]

# Test Set for Response Variables (2016)
ABS_test = Y_ABS[(Y_ABS.index > '2016-01-01') & (Y_ABS.index <= '2016-12-31')]
CMBS_test = Y_CMBS[(Y_CMBS.index > '2016-01-01') & (Y_CMBS.index <= '2016-12-31')]
IGCorp_test = Y_IGCorp[(Y_IGCorp.index > '2016-01-01') & (Y_IGCorp.index <= '2016-12-31')]
GovtRelated_test = Y_GovtRelated[(Y_GovtRelated.index > '2016-01-01') & (Y_GovtRelated.index <= '2016-12-31')]
MBS_test = Y_MBS[(Y_MBS.index > '2016-01-01') & (Y_MBS.index <= '2016-12-31')]
USHightYield_test = Y_USHighYield[(Y_USHighYield.index > '2016-01-01') & (Y_USHighYield.index <= '2016-12-31')]

# Show Missing Values (if any)
print(all_train.isnull().sum())

# View Training Set Keys
all_train.keys()

# -------------------------------------------------------------------------------
## PCA - To Gain Insight into High-Dimensional Data's Variance
# -------------------------------------------------------------------------------
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import statsmodels.api as sm
import statsmodels.formula.api as smf

# Make a Copy of all_train for PCA purposes Only (Excludes Objects)
all_train2 = all_train.select_dtypes(include=['float64', 'int64'])

# Scale Features
scaler = StandardScaler()
scaler.fit(all_train2)
scaled_data = scaler.transform(all_train2)

# Fit PCA on Features using 2 Components
pca = PCA(n_components=2)
pca.fit(scaled_data)

# Apply PCA Transformation
x_pca = pca.transform(scaled_data)

# Shape of the Data
scaled_data.shape
x_pca.shape

# Plot the Components on a Scatter Plot
plt.figure(figsize=(8,6))
plt.scatter(x_pca[:,0],x_pca[:,1],c=CMBS_train,cmap='plasma')
plt.xlabel('Principal Component 1')
plt.ylabel('Principal Component 2')

# View the Array of PCA Components
print (pca.components_)

# Print the Explained Variance Ratio
print(pca.explained_variance_ratio_) 

# This shows the variance explained by each dimension.

df_pca_columns = all_train2.columns.values

df_comp = pd.DataFrame(pca.components_,columns=df_pca_columns)

# Plot a Heatmap to Show the Correlation
plt.figure(figsize=(12,6))
sns.heatmap(df_comp,cmap='plasma',)

# Choose the Number of Components
pca_no = PCA().fit(ABS_with_train.ABS_12M)
plt.plot(np.cumsum(pca.explained_variance_ratio_))
plt.xlabel('Number of Components')
plt.ylabel('Cumulative Explained Variance');

# Visualize and Interpret the PCA Results

explained_variance = pca.explained_variance_ratio_
explained_variance

with plt.style.context('ggplot'):
    plt.figure(figsize=(6, 4))

    plt.bar(range(2), explained_variance, alpha=0.5, align='center',
            label='Individual Explained Variance')
    plt.ylabel('Explained Variance Ratio')
    plt.xlabel('Principal Components')
    plt.legend(loc='best')
    plt.tight_layout()
    
# The plot shows that the first component constitutes almost 60% of the variance.
# The second component should be dropped.

# -------------------------------------------------------------------------------
## EDA - View Correlations with 12-Month Excess Returns Per Sector

'''
Add in Jon's code here for the correlation
'''
# -------------------------------------------------------------------------------
# Concatenate Training Set with the ABS Training Set
ABS_with_train = pd.concat([all_train,ABS_train],axis=1)
ABS_with_train

# Correlation Matrix
corr = ABS_with_train.corr()
corr

m = ~(corr.mask(np.eye(len(corr), dtype=bool)).abs() > 0.5).any()
m
raw = corr.loc[m, m]
raw

# Plot the Correlation Matrix
# sns.heatmap(corr, mask=np.zeros_like(corr, dtype=np.bool), cmap=sns.diverging_palette(220, 10, as_cmap=True),
#            square=True)

# -------------------------------------------------------------------------------
## Models
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
## Random Forest Model
'''
Redo and get a postive R-squared and provide a baseline estimate to get a meaningful result
'''
# -------------------------------------------------------------------------------
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
from scipy.stats import spearmanr, pearsonr
from statsmodels.formula.api import ols

# Importing Visualization Libraries
from sklearn.tree import export_graphviz
import pydot

# Preprocess and Convert Data to Arrays
abs_actual = np.array(ABS_test['ABS_12M'])
labels = np.array(features['actual'])

# Remove the labels from the features
features = Y_.drop('ABS_12M', axis = 1)

# Saving Feature Names for Later Use
feature_list = list(features.columns)

# Convert to Numpy Array
features = np.array(features)

# Split the Data into Training and Testing Sets
all_train, all_test, ABS_train, ABS_test  = train_test_split(features, labels, test_size = 0.25, random_state = 42)
# train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0.25, random_state = 42)

print('Training Features Shape:', all_train.shape)
print('Training Labels Shape:', ABS_train.shape)
print('Testing Features Shape:', ABS_train.shape)
print('Testing Labels Shape:', ABS_test.shape)

# The Baseline Predictions are the Historical Averages
baseline_preds = test_features[:, feature_list.index('average')]

    # Baseline Errors, and Display Average Baseline Error
baseline_errors = abs(baseline_preds - test_labels)
print('Average baseline error: ', round(np.mean(baseline_errors), 2))
Average baseline error:  5.06 degrees.

# Instantiate model with 1000 decision trees
rf = RandomForestRegressor(n_estimators = 1000, random_state = 42)

# Train the model on training data
rf.fit(train_features, train_labels);

# Use the forest's predict method on the test data
predictions = rf.predict(test_features)

# Calculate the absolute errors
errors = abs(predictions - test_labels)

# Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2), 'degrees.')
Mean Absolute Error: 3.83 degrees.

# Calculate mean absolute percentage error (MAPE)
mape = 100 * (errors / test_labels)
# Calculate and display accuracy
accuracy = 100 - np.mean(mape)
print('Accuracy:', round(accuracy, 2), '%.')

# Pull out one tree from the forest
tree = rf.estimators_[5]

# Import tools needed for visualization
from sklearn.tree import export_graphviz
import pydot

# Pull out one tree from the forest
tree = rf.estimators_[5]

# Export the image to a dot file
export_graphviz(tree, out_file = 'tree.dot', feature_names = feature_list, rounded = True, precision = 1)

# Use dot file to create a graph
(graph, ) = pydot.graph_from_dot_file('tree.dot')

# Write graph to a png file
graph.write_png('ABS_RF_tree.png')

# Limit depth of tree to 3 levels
rf_small = RandomForestRegressor(n_estimators=10, max_depth = 3)
rf_small.fit(train_features, train_labels)

# Extract the small tree
tree_small = rf_small.estimators_[5]

# Save the tree as a PNG image
export_graphviz(tree_small, out_file = 'small_tree.dot', feature_names = feature_list, rounded = True, precision = 1)
(graph, ) = pydot.graph_from_dot_file('small_tree.dot')
graph.write_png('small_tree.png');

# Get numerical feature importances
importances = list(rf.feature_importances_)
# List of tuples with variable and importance
feature_importances = [(feature, round(importance, 2)) for feature, importance in zip(feature_list, importances)]
# Sort the feature importances by most important first
feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
# Print out the feature and importances 
[print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances];

# New random forest with only the two most important variables
rf_most_important = RandomForestRegressor(n_estimators= 1000, random_state=42)

# Extract the two most important features
important_indices = [feature_list.index('temp_1'), feature_list.index('average')]
train_important = train_features[:, important_indices]
test_important = test_features[:, important_indices]

# Train the random forest
rf_most_important.fit(train_important, train_labels)

# Make predictions and determine the error
predictions = rf_most_important.predict(test_important)
errors = abs(predictions - test_labels)

# Display the performance metrics
print('Mean Absolute Error:', round(np.mean(errors), 2), 'degrees.')
mape = np.mean(100 * (errors / test_labels))
accuracy = 100 - mape
print('Accuracy:', round(accuracy, 2), '%.')

# Set the style
plt.style.use('fivethirtyeight')

# list of x locations for plotting
x_values = list(range(len(importances)))

# Make a bar chart
plt.bar(x_values, importances, orientation = 'vertical')

# Tick labels for x axis
plt.xticks(x_values, feature_list, rotation='vertical')

# Axis labels and title
plt.ylabel('Importance'); plt.xlabel('Variable'); plt.title('Variable Importances');

# ----------------------------------------------------------
# Salim's Code for Random Forest
# ----------------------------------------------------------
'''
# ABS
rf = RandomForestRegressor(n_estimators=100, oob_score=True, random_state=0)
rf.fit(all_train, ABS_train)

predicted_train = rf.predict(all_train)
predicted_test = rf.predict(all_test)
test_score = r2_score(ABS_test, predicted_test)
spearman = spearmanr(ABS_test, predicted_test)
pearson = pearsonr(ABS_test, predicted_test)
test_score
spearman
pearson

Checking for p-values with OLS:

# Check for p-value with OLS

# USHighYield
model = ols("USHighYield_train ~ all_train", data=all_train).fit()
model.summary()

#GovtRelated
model = ols("GovtRelated_train ~ all_train", data=all_train).fit()
model.summary()

# IGCorp
model = ols("IGCorp_train ~ all_train", data=all_train).fit()
model.summary()

# MBS
model = ols("MBS_train ~ all_train", data=all_train).fit()
model.summary()

# CMBS
model = ols("CMBS_train ~ all_train", data=all_train).fit()
model.summary()

# ABS
model = ols("ABS_train ~ all_train", data=all_train).fit()
model.summary()
'''

# -------------------------------------------------------------------------------
## Linear Regression Model

'''
Workflow:
 1. Coefficients Per Sector 
 2. Accuracy Metrics Per Sector; Variance
 2a. Actual Future 12-month Excess Returns (Data)
 2b. Expected Future 12-month Excess Returns (Model Predictions)
 3. List of (Predictor) Variables Used for this Model
 3a. Models Used to Identify Most Important Variables to Use; Feature Selection Methods:
        (1) PCA
        (2) Random Forest
        (3) Gradient Boosting ** If time allows **
 # --------------------------------------
 4a. List all the above for all six sectors in sorted order -> Top performer? Expected excess return?
 4b. Top performer by accuracy metrics and what model was used (as well as what input variables were used)
 4c. Supervised Learning Models Used:
        (1) Linear Regression **
        (2) Rolling Mean (Window)
        (3) ARIMA
        (4) OLS
        
# Challenges:
1. Minimize multicollinearity
2. Resolve the negative R-squared for feature selection and linear regression
        
##  Create a for loop to iterate and change the training and test years 
 
 # Iteration 1
 X = 2005 - 2010 # modify these dates
 Y = 2012 # modify these dates
 
 # Iteration 2
 X = 2006 - 2011
 Y = 2013
 
 ...
 
 X = 2011 - 2015
 Y = 2017

##  Create a for loop for each sector

# ABS
# CMBS
# GovtRelated
# IGCorp
# MBS
# USHighYield
'''

# -------------------------------------------------------------------------------
from sklearn.linear_model import LinearRegression
from sklearn import metrics

# - Train out the regression model on 3 years of training data
# - Test on 1 year out of test data
 
# Establish X and Y arrays
X = df[['EXKOUS', 'VIX1M Index', 'USEPUINDXM', 'PSAVERT', 'RECPROUSM156N']]
y = df['CMBS_12M']

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.4, random_state=101)

lm = LinearRegression()

lm.fit(X_train, y_train)

# Evaluate the Model and Print the Intercept
print(lm.intercept_)

coeff_df = pd.DataFrame(lm.coef_,X.columns,columns=['Coefficient'])
coeff_df

# Predict Off the Test Set
predictions = lm.predict(X_test)
plt.scatter(y_test, predictions)

# Create a Residual Histogram
sns.distplot((y_test-predictions),bins=50)

# Provide the Regression Evaluation Metrics:
# MAE = mean of the absolute value of the errors; average error
# MSE = metric that punishes larger errors; mean of the squared errors
# RMSE = square root of the mean of the squared errors
print('MAE:', metrics.mean_absolute_error(y_test, predictions))
print('MSE:', metrics.mean_squared_error(y_test, predictions))
print('RMSE:', np.sqrt(metrics.mean_squared_error(y_test, predictions)))

'''
# -------------------------------------------------------------------------------
# TIME SERIES ANALYSIS TECHNIQUES
# -------------------------------------------------------------------------------
from pandas.tools.plotting import autocorrelation_plot
from statsmodels.tsa.stattools import acf, pacf
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.arima_model import ARIMA
from statsmodels.tsa.stattools import adfuller

- Estimate a model on 2010-2012 (3 years and buffer 1 year) | Test data set - 2014
- Split on 2011-2013 | Test data set - 2015
- Re-estimate the model and calculate the performance

# -------------------------------------------------------------------------------
# Autocorrelation
# -------------------------------------------------------------------------------

- How correlated a variable is with itself; how related variables earlier in time are with variables later in time
- In other words, autocorrelations are a measure of how much a data point is dependent on previous data points
    - Positive autocorrelation = trend following; momentum
    - Negative autocorrelation = mean reverting
    
- Stocks historically have negative autocorrelation, measured over short horizons (days).  
    - Trading strategy: Buy losers and sell winners.

- Commodities and currencies have historically had positive autocorrelation, measured over longer horizons (months).
    - Trading strategy: Buy winners and sell losers.

# Compute all the autocorrelation for all six sectors
ABS_auto = df['ABS_12M'].autocorr()
CMBS_auto = df['CMBS_12M'].autocorr()
GovtRelated_auto = df['GovtRelated_12M'].autocorr()
IGCorp_auto = df['IGCorp_12M'].autocorr()
MBS_auto = df['MBS_12M'].autocorr()
USHighYield_auto = df['USHighYield_12M'].autocorr()

print("The autocorrelation of ABS is:", ABS_auto)
print("The autocorrelation of CMBS is:", CMBS_auto)
print("The autocorrelation of GovtRelated is:", GovtRelated_auto)
print("The autocorrelation of IGCorp is:", IGCorp_auto)
print("The autocorrelation of MBS is:", MBS_auto)
print("The autocorrelation of US High Yield is:", USHighYield_auto)

# **INSIGHT:** All sectors are highly correlated.

# View the CMBS sector first
autocorrelation_plot(df.CMBS_12M)

# Plot the autocorrelation function (ACF) to measure the correlation between the TS with a lagged version of itself
plot_acf(df.CMBS_12M, lags = 12) # one year lag on CMBS


# **INSIGHT** The later lags of 7-12 months are statistically significant.

# View trends for all six sectors over time with good parameters for visualizations

plt.style.use('ggplot')
fig = plt.figure(figsize=(15,10))
df.plot(kind= "line", y = ['ABS_12M', 'CMBS_12M', 'GovtRelated_12M',
       'IGCorp_12M', 'MBS_12M', 'USHighYield_12M'])


# **INSIGHT:** GovtRelated and MBS = Less correlated sectors

fig = plt.figure(figsize=(12,8))
plt.plot(df['ABS_12M'], color='blue') # Asset-Backed Securities
plt.plot(df['RECPROUSM156N'], color='red') # US Smoothed Recession Probabilities


## Check Stationarity

# Check for stationarity or constant statistical properties over time.  Such as:
# 1. Constant Mean
# 2. Constant Variance
# 3. Autocovariance that does not depend on time
# 
#     - Is the series stationary?
#     - Use adf.test(), ACF, PACF plots to determine order of differencing needed
#     - Look for an overall trend in the data (along with some seasonal variations)
# 

# Plot the CMBS 12 month excess returns
plt.style.use('ggplot')
df['CMBS_12M'].plot(kind = "hist", bins = 30)

df['CMBS_12M'].plot()

# Log transformation to help stabilize the variance observed above
df['CMBS_12M_Log'] = np.log(df.CMBS_12M)
df['CMBS_12M_Log'].head()

df['CMBS_12M_Log'].plot(kind = "hist", bins = 30)

df['CMBS_12M_Log'].plot()


# **RESULT:** The model is stationary.
# - Model in R using 2010-2012 data for training and testing on 2014 data.

# -------------------------------------------------------------------------------
# Random Walk
# -------------------------------------------------------------------------------

# - Regress current mean excess returns on lagged excess returns

df["CMBS_12M_LogShift"] = df.CMBS_12M_Log.shift()
df.CMBS_12M_Log.head()

# df2 = pd.DataFrame(data=df)
# type(df2)
# df.CMBS_12M_Log.plot(kind = "scatter", y = "CMBS_12M_Log", x = "CMBS_12M_LogShift", s = 50)
# df2.head()


# #### Action: Fix this dataframe and remove index to be able to form scatter plot above.

# Lets plot the one-month difference curve
df["CMBS_12M_LogDiff"] = df.CMBS_12M_Log - df.CMBS_12M_LogShift


df.CMBS_12M_LogDiff.plot()

df["CMBSRandom"] = np.exp(df.CMBS_12M_LogShift)


# Convert the Value Date in datetimedelta figure starting from zero
# df["Time_Index"] = df.Value_Date - df.Value_Date.min()

# df.plot(kind = "line", x = "Time_Index", y = ["CMBS_12M","CMBSRandom"])

# -------------------------------------------------------------------------------
# Moving Average (MA)
# -------------------------------------------------------------------------------
# - Good method for predicting spikes and for handling abrupt changes in a system, as this model is always stationary; univariate time series approach
# - MA used prior errors to quickly incorporate changes, allowing to correct sudden changes based on random events
# - Measure of the average can be the mean or median
# - Gives us a local statistic on an average in time
# - Plot the MA to visualize trends and smooth out random fluctuations, remove outliers, and help identify larger trends

fig = plt.figure(figsize=(12,8))
moving_avg = pd.rolling_mean(excess_returns_cmbs, 12)
plt.plot(moving_avg)
plt.plot(roll, color = 'red')

# Create a more stationary plot by removing MA
# Trend line would not be as reliable
# Roll by standard deviation

excess_no_ma = excess_returns_cmbs - moving_avg
roll_std = pd.rolling_std(excess_no_ma, window=(12))

fig = plt.figure(figsize=(12,8))
plt.plot(excess_no_ma)
plt.plot(roll, color = 'red')

# Attempt to decrease trend and seasonality
ts_log_diff = excess_no_ma - excess_no_ma.shift()

roll_log_diff = pd.rolling_std(ts_log_diff, window=(12))
# Series.rolling(window=12,center=False).std()

#fig = plt.figure(figsize=(12,8))plt.plot(ts_log_diff)
plt.plot(roll_log_diff, color = 'red')'

# -------------------------------------------------------------------------------
# Rolling Window
# -------------------------------------------------------------------------------
# - Rolling takes three important parameters
# 1. Window = number of days/weeks/months to include in the average
# 2. Center = whether the window should be centered on the data or use data prior to that data
# 3. Freq = what level to roll up averages to (as used in resample)

# Excess returns for CMBS
excess_returns_cmbs = df['CMBS_12M']

# Create a rolling window of 12 months
roll = pd.rolling_std(excess_returns_cmbs, window=(12))
fig = plt.figure(figsize=(12,8))
plt.plot(excess_returns_cmbs, color ='blue')
plt.plot(roll, color='red')

# Expanding Window
- The expanding method yields the value of the statistic with all the data available up to that point in time.
 
### Autoregressive (AR)
- AR models that are good for predicting long-term trends, where seasonal variation exists
- AR slowly incorporates changes in the system by combining previous values; changes in preferences, tastes, and/or patterns

# Autoregressive and Moving Average (ARMA)
- ARMA (p,q)
- Consists of the weighted sum of past values (autoregressive component) and the weighted sum of past errors (moving average component)

# -------------------------------------------------------------------------------
# ARIMA
# -------------------------------------------------------------------------------

# ARIMA = Auto-Regressive Integrated Moving Average (p, d, q)
# - Good for meeting stationarity assumptions at the cost of introducing model complexity
# - Best to use ARIMA becuase they don't rely on the underlying series being stationary, compared to ARMA
# - Predict the difference of the series (as opposed to the value of the series)

# **Evaluate and Iterate:**
# - Check residuals, which should have no patterns and be normally distributed
# - If there are visible patterns or bias, plot ACF/PACF.  Are any additional order parameters needed?
# - Refit model if needed. Compare model errors and fit criteria such as AIC or BIC.
# - Calculate forecast using the chosen model

# Use ARIMA(p, d, q) to predict the difference of the series
# p = AR = 1
# d = Integrated = 1
# q = MA = 1
model_CMBS = ARIMA(df['CMBS_12M'], (1, 1, 1)).fit() 
model_CMBS.summary()
# BIC = 
# AIC = how well a model fits the data; looking for lowest AIC value

# Inspect residuals
model_CMBS.resid.plot()

# Plot autocorrelation of the residuals
plot_acf(model.resid, lags = 50)


# In[125]:

# Plot predictions for CMBS
model_CMBS.plot_predict(1,50)

'''
