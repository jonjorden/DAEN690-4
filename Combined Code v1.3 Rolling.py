###############
#Rolling Section Start

import pandas as pd
from sklearn import linear_model

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
df_features = df[df.columns[12:19]]
df_features.head()

#Training Set for Features (2005 through 2016)
all_train = df_features[(df_features.index > '2004-12-01') & (df_features.index <= '2016-12-31')]
all_train.info()

#Training Set for Response Variables
ABS_train = Y_ABS[(Y_ABS.index > '2004-12-01') & (Y_ABS.index <= '2016-12-31')]
CMBS_train = Y_CMBS[(Y_CMBS.index > '2004-12-01') & (Y_CMBS.index <= '2016-12-31')]
IGCorp_train = Y_IGCorp[(Y_IGCorp.index > '2004-012-01') & (Y_IGCorp.index <= '2016-12-31')]
GovtRelated_train = Y_GovtRelated[(Y_GovtRelated.index > '2004-12-01') & (Y_GovtRelated.index <= '2016-12-31')]
MBS_train = Y_MBS[(Y_MBS.index > '2004-12-01') & (Y_MBS.index <= '2016-12-31')]
USHighYield_train = Y_USHighYield[(Y_USHighYield.index > '2004-12-01') & (Y_USHighYield.index <= '2016-12-31')]

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
    print('ABS preds:',predictions)
    print('ABS actual:',actuals)
    print('ABS r2',r2_score(ABS_return,ABS_preds))
    #MBS
    MBS_model_ols.fit(all_train[iStart:iEnd], MBS_train[iStart:iEnd])
    MBS_preds = MBS_model_ols.predict(all_train[pred_start:pred_end])
    MBS_actual = MBS_train[pred_start:pred_end]
    MBS_return = MBS_actual.values
    MBS_predictions.append(MBS_preds[11])
    MBS_actuals.append(MBS_return[11])
    print('MBS preds:',predictions)
    print('MBS actual:',actuals)
    print('MBS r2',r2_score(MBS_return,MBS_preds))
    #CMBS
    CMBS_model_ols.fit(all_train[iStart:iEnd], CMBS_train[iStart:iEnd])
    CMBS_preds = CMBS_model_ols.predict(all_train[pred_start:pred_end])
    CMBS_actual = CMBS_train[pred_start:pred_end]
    CMBS_return = CMBS_actual.values
    CMBS_predictions.append(CMBS_preds[11])
    CMBS_actuals.append(CMBS_return[11])
    print('CMBS preds:',predictions)
    print('CMBS actual:',actuals)
    print('CMBS r2',r2_score(CMBS_return,CMBS_preds))
    #IGCorp
    IGCorp_model_ols.fit(all_train[iStart:iEnd], IGCorp_train[iStart:iEnd])
    IGCorp_preds = IGCorp_model_ols.predict(all_train[pred_start:pred_end])
    IGCorp_actual = IGCorp_train[pred_start:pred_end]
    IGCorp_return = IGCorp_actual.values
    IGCorp_predictions.append(IGCorp_preds[11])
    IGCorp_actuals.append(IGCorp_return[11])
    print('IGCorp preds:',predictions)
    print('IGCorp actual:',actuals)
    print('IGCorp r2',r2_score(IGCorp_return,IGCorp_preds))
    #GovtRelated
    GovtRelated_model_ols.fit(all_train[iStart:iEnd], GovtRelated_train[iStart:iEnd])
    GovtRelated_preds = GovtRelated_model_ols.predict(all_train[pred_start:pred_end])
    GovtRelated_actual = GovtRelated_train[pred_start:pred_end]
    GovtRelated_return = GovtRelated_actual.values
    GovtRelated_predictions.append(GovtRelated_preds[11])
    GovtRelated_actuals.append(GovtRelated_return[11])
    print('GovtRelated preds:',predictions)
    print('GovtRelated actual:',actuals)
    print('GovtRelated r2',r2_score(GovtRelated_return,GovtRelated_preds))
    #USHighYield
    USHighYield_model_ols.fit(all_train[iStart:iEnd], USHighYield_train[iStart:iEnd])
    USHighYield_preds = USHighYield_model_ols.predict(all_train[pred_start:pred_end])
    USHighYield_actual = USHighYield_train[pred_start:pred_end]
    USHighYield_return = USHighYield_actual.values
    USHighYield_predictions.append(USHighYield_preds[11])
    USHighYield_actuals.append(USHighYield_return[11])
    print('USHighYield preds:',predictions)
    print('USHighYield actual:',actuals)
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


#Actuals
all_actuals = pd.DataFrame({'Year':years, 'ABS': ABS_actuals,'MBS': MBS_actuals,'CMBS': CMBS_actuals,
'IGCorp': IGCorp_actuals, 'GovtRelated': GovtRelated_actuals,'USHighYield': USHighYield_actuals})
all_actuals = all_actuals.set_index('Year')
all_actuals = all_actuals.transpose()
actual_rankings = all_actuals.rank(axis=0, ascending=False).astype('int64')


#Rolling Section End
############################