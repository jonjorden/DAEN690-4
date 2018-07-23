# -------------------------------------------------------------------------------
## WORKFLOW ##

'''
    DATAFRAME MANAGEMENT:
    1.  Create a dataframe with both the dependent and independent variables in one row
    2.  Create vectors for the position of the dependent and independent variables in your dataset
    3.  Create a for loop to run the preprocessing steps, feature selection, and models for one sector for all years (2005-2016)
    
>>> PARALLEL PROCESSING:
    4. Re-run feature selection with correlation drill-down and exploratory analysis - Jon
    5. Re-run the same models for different time periods - Jon & Jessica
    6. Evaluate the error metrics; possibly come up with a baseline to compare against; research better way - Jessica (MSE, R-squared)
    
    OUTPUT MANAGEMENT:
    - List all the above for all six sectors in sorted order -> Top performer? Expected excess return?
    - Top performer by accuracy metrics and what model was used (as well as what input variables were used)
        - The output should be a dataframe with the following columns:
            1. Rank
            2. Sector
            3. Tuple - Top 10 List of Explanatory/Independent Variables Used
            4. Start Month/Date (of five-year window)
            5. Predicted Month/Year (one year forward)
            6. Actual 12-Month Excess Return
            7. Model's Predicted 12-Month Excess Return
            8. Error Metrics *
        
        * Consider the following depending on the model ran: beta estimate:
            - Beta (coefficient/slope of the trend between facotr and future excess returns per sector)
            - Standard Error (RMSE, R-squared, MSE); Results Summary metrics available
            - P-value
    
    STEP 1: LEARN ABOUT THE DATA; DOCUMENTATION, STATISTICAL SUMMARIES, TABLE AND GRAPHIC VIEWS
        - Include EDA, descriptive plots, correlation plots, etc. (all pre-processing steps used)
    
    STEP 2: RUN PRE-PROCESSING STEPS AND PERFORM FEATURE SELECTION
        - PCA
        - Random Forest (see RF_Regression R files for each sector)
        - Gradient Boosting 
        
    STEP 3: RUN MODELS AND APPEND RESULTS TO A NEW DATAFRAME FOR SORTING LATER
        - Supervised Learning Models Used:
            (1) Linear Regression ** / OLS
                # Create a df with the expected returns outputted from the linear regression.
                # Compare the expected returns.
                # Rank from highest to lowest.
                # What is the actual forward return of rank #1, #2, #3, #4, #5, and #6? This should be sorted from largest to smallest.
            (2) Rolling Mean (Window)
            (3) ARIMA
            (4) ARIMA
            
    STEP 4: ASSESS THE RANKING CORRELATIONS AND TRY TO IDENTIFY MOST PREDICTIVE FACTORS PER SECTOR
        - After parallel processing, find patterns (similarities > differences); which factors are reliable to increase the model's accuracy in prediction?
'''
# -------------------------------------------------------------------------------

# Overall Action Items / Notes to Self:
# Past Challenges:
# 1. Minimize multicollinearity
# 2. Resolve the negative R-squared for feature selection and linear regression

# -------------------------------------------------------------------------------
# Loading Fundamental Libraries
import csv
import numpy as np
import pandas as pd
import sklearn
from datetime import datetime

# Importing Visualization Libraries
import seaborn as sns
import matplotlib.pyplot as plt
sns.set(style="whitegrid", font_scale=1)
%matplotlib inline

# -------------------------------------------------------------------------------
# Read Data File
df = pd.read_csv('Combined_Dataset_v3.csv', parse_dates= ['Value_Date'], index_col= "Value_Date")
# df2 = pd.read_csv('Combined_Dataset_v3.csv', parse_dates= ['Value_Date'])
df.head()

# Sort Dates in Ascending Order
df = df.sort_index()
df.head()

# View all column names
df.columns

# -------------------------------------------------------------------------------
## ## Linear Regression Model
# -------------------------------------------------------------------------------
from sklearn.linear_model import LinearRegression
from sklearn import linear_model
from sklearn import metrics
from sklearn.metrics import r2_score
import statsmodels.regression.linear_model as sm

# Declare Response Variables (5 x 75)
Y_ABS = df['ABS_12M']
Y_CMBS = df['CMBS_12M']
Y_IGCorp = df['IGCorp_12M']
Y_MBS = df['MBS_12M']
Y_USHighYield = df['USHighYield_12M']
Y_GovtRelated = df['GovtRelated_12M']


# Declaring Explanatory Variables For All Sectors Based on Random Forest Results
df_features_ABS = df[["EXUSUK", "USSLIND", "TOTALSA", "INDPRO", "TCU",
                     "RECPROUSM156N", "POILWTIUSDM", "CCI", "MSACSR", "EXUSEU"]]
df_features_CMBS = df[["EXUSUK", "TCU", "TOTALSA", "EXUSEU", "EXKOUS", 
                      "MSACSR", "MSACSR.1", "CCI", "USSLIND", "INDPRO"]]
df_features_GovtRelated = df[["INDPRO", "EXUSUK", "EXKOUS", "EXMXUS", "TTLCONS", 
                             "USSLIND", "TCU", "PAYEMS", "HQMCB10YR", "PCETRIM12M159SFRBDAL"]]
df_features_IGCorp = df[["EXUSUK", "EXKOUS", "USSLIND", "TTLCONS", "INDPRO",
                         "PAYEMS", "TCU", "EXMXUS", "S&P500", "HSN1F"]]
df_features_MBS = df[["TCU", "EXKOUS", "TOTALSA", "S&P500", "EXCAUS", "USSLIND", 
                      "EXUSUK", "PCETRIM12M159SFRBDAL", "TTLCONS", "IRSTCI01USM156N"]]
df_features_USHighYield = df[["S&P500", "HSN1F", "EXKOUS", "INDPRO", "HQMCB10YR",
                              "PAYEMS", "LIBOR", "USD3MTD156N", "EXCAUS", "USSLIND"]]

# Training Set for Independent Variables (2005 through 2016)
all_train = df_features_ABS[(df_features_ABS.index > '2004-12-01') & (df_features_ABS.index <= '2016-12-31')]
# all_train.info()

    # Check the top and bottom of the dataframe:
    # len(all_train) # 145 rows for 12 years of data (2005 - 2017)
    # all_train.iloc[0] # 12/31/2004
    # all_train.head(5) # S&P500 and FedRates
    # all_train.iloc[60] # 12/31/2009
    # Test set would be the full year of 2012

# Training Set for Response/Dependent Variables
ABS_train = Y_ABS[(Y_ABS.index > '2004-12-01') & (Y_ABS.index <= '2016-12-31')]
CMBS_train = Y_CMBS[(Y_CMBS.index > '2004-12-01') & (Y_CMBS.index <= '2016-12-31')]
IGCorp_train = Y_IGCorp[(Y_IGCorp.index > '2004-012-01') & (Y_IGCorp.index <= '2016-12-31')]
GovtRelated_train = Y_GovtRelated[(Y_GovtRelated.index > '2004-12-01') & (Y_GovtRelated.index <= '2016-12-31')]
MBS_train = Y_MBS[(Y_MBS.index > '2004-12-01') & (Y_MBS.index <= '2016-12-31')]
USHighYield_train = Y_USHighYield[(Y_USHighYield.index > '2004-12-01') & (Y_USHighYield.index <= '2016-12-31')]

# Create a for loop running a linear regression model to predict future 12-month excess returns:

# Create a window for 5 years and test window for 1 year
window_test = 12
window = 60
iStart = 0

# Establish linear models
ABS_model_ols = linear_model.LinearRegression()
MBS_model_ols = linear_model.LinearRegression()
CMBS_model_ols = linear_model.LinearRegression()
IGCorp_model_ols = linear_model.LinearRegression()
GovtRelated_model_ols = linear_model.LinearRegression()
USHighYield_model_ols = linear_model.LinearRegression()

# Note: Always copy and run from this point forward, don't start at the for loop.

# Setting empty lists to hold results
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

# Actions Performed Below:
    # Loop through the dataset fitting a model based on 5 years
    # Skip a year, then test on the following year
    # Take the December return of that year
    # Store the results

for iStart in range(0, len(all_train),window_test):
    iEnd = iStart+window + 1
    iStart = iStart + 1
    if iEnd == 133:
        break
    print('training:',iStart,iEnd)

    pred_start = iEnd + window_test
    pred_end = pred_start + window_test + 1
    print('test:', pred_start, pred_end)
    
# PARALLEL PROCESSING NOTES [PENDING]:
    # RUN A CORRELATION BETWEEN 2005 = 2009 FOR ALL INDEPENDENT AND DEPENDENT VARIABLES FOR EVERY YEAR
    # Subset 10-20, down from the total 63 macroeconomic variables
    # Objective: Discover that actuals correlate with the actuals; does the performance improve with different training sets?
    # Objective: Try to identify patterns with the common most important factors and/or explanatory?
    
    # ABS SECTOR
    ABS_model_ols.fit(all_train[iStart:iEnd], ABS_train[iStart:iEnd]) # Iter 1 = Dec. 2005 - 2009 for both training and test set
    ABS_preds = ABS_model_ols.predict(all_train[pred_start:pred_end]) # Full year of 2011
    
    ABS_actual = ABS_train[pred_start:pred_end] # Full year of 2011
    ABS_return = ABS_actual.values # Every excess return per month of 2011
    
    ABS_predictions.append(ABS_preds[11]) # Take the return of December 2011
    ABS_actuals.append(ABS_return[11]) # Append Dec. 2011 only to the results
    
    print('ABS preds:', ABS_predictions) # Print to screen predicted 12-month future excess returns
    print('ABS actual:', ABS_actuals) # Print to screen actual excess returns
    print('ABS r2',r2_score(ABS_return,ABS_preds)) # R-squared for each year

    # CMBS
    CMBS_model_ols.fit(all_train[iStart:iEnd], CMBS_train[iStart:iEnd])
    CMBS_preds = CMBS_model_ols.predict(all_train[pred_start:pred_end])
    CMBS_actual = CMBS_train[pred_start:pred_end]
    CMBS_return = CMBS_actual.values
    CMBS_predictions.append(CMBS_preds[11])
    CMBS_actuals.append(CMBS_return[11])
    print('CMBS preds:',CMBS_predictions)
    print('CMBS actual:', CMBS_actuals)
    print('CMBS r2',r2_score(CMBS_return,CMBS_preds))
    
    #GovtRelated 
    GovtRelated_model_ols.fit(all_train[iStart:iEnd], GovtRelated_train[iStart:iEnd])
    GovtRelated_preds = GovtRelated_model_ols.predict(all_train[pred_start:pred_end])
    GovtRelated_actual = GovtRelated_train[pred_start:pred_end]
    GovtRelated_return = GovtRelated_actual.values
    GovtRelated_predictions.append(GovtRelated_preds[11])
    GovtRelated_actuals.append(GovtRelated_return[11])
    print('GovtRelated preds:',GovtRelated_predictions)
    print('GovtRelated actual:', GovtRelated_actuals)
    print('GovtRelated r2',r2_score(GovtRelated_return,GovtRelated_preds))
    
    # IGCorp
    IGCorp_model_ols.fit(all_train[iStart:iEnd], IGCorp_train[iStart:iEnd])
    IGCorp_preds = IGCorp_model_ols.predict(all_train[pred_start:pred_end])
    IGCorp_actual = IGCorp_train[pred_start:pred_end]
    IGCorp_return = IGCorp_actual.values
    IGCorp_predictions.append(IGCorp_preds[11])
    IGCorp_actuals.append(IGCorp_return[11])
    print('IGCorp preds:', IGCorp_predictions)
    print('IGCorp actual:', IGCorp_actuals)
    print('IGCorp r2',r2_score(IGCorp_return,IGCorp_preds))
    
    # MBS
    MBS_model_ols.fit(all_train[iStart:iEnd], MBS_train[iStart:iEnd])
    MBS_preds = MBS_model_ols.predict(all_train[pred_start:pred_end])
    MBS_actual = MBS_train[pred_start:pred_end]
    MBS_return = MBS_actual.values
    MBS_predictions.append(MBS_preds[11])
    MBS_actuals.append(MBS_return[11])
    print('MBS preds:', MBS_predictions)
    print('MBS actual:', MBS_actuals)
    print('MBS r2',r2_score(MBS_return,MBS_preds))
    
    #USHighYield
    USHighYield_model_ols.fit(all_train[iStart:iEnd], USHighYield_train[iStart:iEnd])
    USHighYield_preds = USHighYield_model_ols.predict(all_train[pred_start:pred_end])
    USHighYield_actual = USHighYield_train[pred_start:pred_end]
    USHighYield_return = USHighYield_actual.values
    USHighYield_predictions.append(USHighYield_preds[11])
    USHighYield_actuals.append(USHighYield_return[11])
    print('USHighYield preds:', USHighYield_predictions)
    print('USHighYield actual:', USHighYield_actuals)
    print('USHighYield r2',r2_score(USHighYield_return,USHighYield_preds))

# Years with Results    
years = [2011,2012,2013,2014,2015,2016]

# Dataframes to hold results
ABS_performance = pd.DataFrame({'Year':years,'ABS_preds': ABS_predictions,'ABS_actuals': ABS_actuals})
MBS_performance = pd.DataFrame({'Year':years,'MBS_preds': MBS_predictions,'MBS_actuals': MBS_actuals})
CMBS_performance = pd.DataFrame({'Year':years,'CMBS_preds': CMBS_predictions,'CMBS_actuals': CMBS_actuals})
IGCorp_performance = pd.DataFrame({'Year':years,'IGCorp_preds': IGCorp_predictions,'IGCorp_actuals': IGCorp_actuals})
GovtRelated_performance = pd.DataFrame({'Year':years,'GovtRelated_preds': GovtRelated_predictions,'GovtRelated_actuals': GovtRelated_actuals})
USHighYield_performance = pd.DataFrame({'Year':years,'USHighYield_preds': USHighYield_predictions,'USHighYield_actuals': USHighYield_actuals})

# Predictions and Actuals
performance = pd.DataFrame({'Year':years,'ABS_preds': ABS_predictions,'ABS_actuals': ABS_actuals,'MBS_preds': MBS_predictions,'MBS_actuals': MBS_actuals,
'CMBS_preds': CMBS_predictions,'CMBS_actuals': CMBS_actuals,'IGCorp_preds': IGCorp_predictions,'IGCorp_actuals': IGCorp_actuals,
'GovtRelated_preds': GovtRelated_predictions,'GovtRelated_actuals': GovtRelated_actuals,'USHighYield_preds': USHighYield_predictions,'USHighYield_actuals': USHighYield_actuals})

# Use the year as an index
performance = performance.set_index('Year')
# Create rankings based on highest return
performance.rank(axis=0, ascending=False)
# Flip the axis
performance.transpose()

# -------------------------------------------------------------------------------
# # Compare the Next Two DataFrames (Predictions vs. Actuals)
# -------------------------------------------------------------------------------

# Predictions
all_predictions = pd.DataFrame({'Year':years,'ABS': ABS_predictions,'MBS': MBS_predictions,'CMBS': CMBS_predictions,
'IGCorp': IGCorp_predictions,'GovtRelated': GovtRelated_predictions,'USHighYield': USHighYield_predictions})
all_predictions = all_predictions.set_index('Year')
all_predictions = all_predictions.transpose()
predicted_rankings = all_predictions.rank(axis=0, method='dense',ascending=False).astype('int64')

# Actuals
all_actuals = pd.DataFrame({'Year':years, 'ABS': ABS_actuals,'MBS': MBS_actuals,'CMBS': CMBS_actuals,
'IGCorp': IGCorp_actuals, 'GovtRelated': GovtRelated_actuals,'USHighYield': USHighYield_actuals})
all_actuals = all_actuals.set_index('Year')
all_actuals = all_actuals.transpose()
actual_rankings = all_actuals.rank(axis=0, ascending=False).astype('int64')

# Print the DataFrames to CSVs:
predicted_rankings.to_csv('predicted_rankings_v1.csv', index=False, header=False)
print all_predictions
        
actual_rankings.to_csv('actual_rankings_v1.csv', index=False, header=False)
print actual_rankings

# -------------------------------------------------------------------------------
# Archives for Later - In a separate .py file for now; less clutter
# -------------------------------------------------------------------------------



