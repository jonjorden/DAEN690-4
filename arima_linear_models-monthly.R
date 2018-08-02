#ARIMA Model

#Packages
library(tseries)
library(MASS)
library(forecast)
library(zoo)
library(ggplot2)

#Read the file and impute with last known value for the response variable
CMBS <-read.csv("CMBS_v2.csv", na.strings = "n/a")
CMBS$X12_Month_Excess_Returns <-  na.locf(CMBS$X12_Month_Excess_Returns, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

#Keep these columns
keep_cols <- c("Value.Date","Excess.Return", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y", "X12_Month_Excess_Returns")
CMBS_mod <- CMBS[ , (names(CMBS) %in% keep_cols)]

#Convert to R friendly date format
CMBS_mod$Value.Date <- as.Date(CMBS_mod$Value.Date, "%m/%d/%Y")

#Add the date back and make it the 1st column
#ABS_cor$Date <- ABSfile$Value.Date
#ABS_lm <- subset(ABS_cor, select = c(26,0:25))

#Add more columns
#ABS_lm[27:37] <- ABS_slimmer[25:35]
#ABS_lm$Date <- as.Date(ABS_lm$Date, "%m/%d/%Y")

#Subset Train Set
CMBS_mod_train <- CMBS_mod[CMBS_mod$Value.Date >="2003-01-01" & CMBS_mod$Value.Date<= "2012-12-31",]

#Subset Test Set
CMBS_mod_test <- CMBS_mod[CMBS_mod$Value.Date >="2014-01-01" & CMBS_mod$Value.Date<= "2014-12-31",]

#Two variable ds
train_data <- CMBS_mod_train[,0:2]
test_data <- CMBS_mod_test[,0:2]

yresponse <- data.frame(CMBS_mod_train$Value.Date, CMBS_mod_test$X12_Month_Excess_Returns)
cols_to_keep <- intersect(colnames(yresponse),colnames(train_data))
yresponse <- yresponse[,cols_to_keep, drop=TRUE]

#Log of price
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)
train_data$Excess.Return = log(train_data$Excess.Return)

#Autocorrelation and Partial Autocorrelation
acf(train_data$Excess.Return, lag.max = 20)

pacf(train_data$Price, lag.max = 20)

#Stationary Model
lnCMBS = train_data

#Fit Model beginning at 2010 with a frequency of 250- roughly one year of data
pricearima <- ts(lnCMBS, start = c(2010,01,01), frequency = 250 )
fitlnCMBS <- auto.arima(pricearima)
fitlnCMBS
plot(pricearima, type='l')

#exp(lnABS)
forecastedvalues_ln = forecast(fitlnCMBS, h=500)
forecastedvalues_ln
plot(forecastedvalues_ln)

forecastedvaluesextracted = as.numeric(forecastedvalues_ln$mean)
finalforecastvalues = exp(forecastedvaluesextracted)
finalforecastvalues
results <- data.frame(Date = test_data$Value.Date, ActualPrice = test_data$Excess.Return, ForecastedPrice = finalforecastvalues[251:500])
col_headings <- c("Actual Price", "Forecasted Price")
names(results) <- col_headings
percentage_error = ((results$`Actual Price`- results$`Forecasted Price`)/(results$`Actual Price`))
percentage_error
mean(percentage_error)

Box.test(fitlnABS$residuals, lag=5, type = "Ljung-Box")
Box.test(fitlnABS$residuals, lag=10, type = "Ljung-Box")
Box.test(fitlnABS$residuals, lag=15, type = "Ljung-Box")

library(ggplot2)
library(reshape2)

#Line Plot
ggplot(results, aes(x = Date)) + 
  geom_line(aes(y = ActualPrice), colour="blue") + 
  geom_line(aes(y = ForecastedPrice), colour = "red") + ggtitle("ARIMA Model")
ylab(label="Price") + 
  xlab("2014 PReds vs Actual")


#-------------------




#---------------

#Linear Models

all_sectors <-read.csv("Combined_Dataset_v3.csv", na.strings = "N/A")

all_sectors$Value.Date <- as.Date(all_sectors$Value.Date, "%m/%d/%Y")

#Subset Train Set
all_train_linear <- all_sectors[all_sectors$Value.Date >="2005-01-01" & all_sectors$Value.Date<= "2009-12-31",]
y_ABS_train <- all_train_linear$ABS_12M

#ABS_tr_linear <- all_train_linear[14:76]
keep_columns <- c('S&P500', 'FedRates', 'LIBOR', 'VIX1M Index', 'VIX3M Index', 'USGG2YR Index', 'USGG10YR Index', 'T10Y2Y', 'T10YIEM', 'PAYEMS', 'IRSTCI01USM156N', 'EXCAUS', 'TCU', 'CCI', 'UMCSENT', 'MCOILBRENTEU', 'CURRCIR', 'USEPUINDXM', 'INDPRO', 'EXJPUS', 'EXKOUS', 'USSLIND', 'M1SL', 'MNFCTRSMNSA', 'MVGFD027MNFRBDAL', 'MVMTD027MNFRBDAL', 'MVPHGFD027MNFRBDAL', 'MSPNHSUS', 'EXMXUS', 'BAA', 'PSAVERT', 'PCU523110523110202', 'CSUSHPINSA', 'RECPROUSM156N', 'TTLCONS', 'TOTALSA', 'USUNEMPLOYMENTRATE', 'USD3MTD156N', 'EXUSUK')
ABS_tr_linear <- all_train_linear[ , (names(all_train_linear) %in% keep_columns)]


#Subset Test Set
all_test_linear <- all_sectors[all_sectors$Value.Date >="2011-01-01" & all_sectors$Value.Date<= "2011-12-31",]
y_ABS_test <- all_test_linear$ABS_12M

#ABS_ts_linear <- all_test_linear[14:76]
ABS_ts_linear <- all_test_linear[ , (names(all_test_linear) %in% keep_columns)]
#Train Linear Model
simple.fit = lm(y_ABS_train ~., data= ABS_tr_linear )

#Model Summary
summary(simple.fit)

#Make Predictions
predictions = predict.lm(simple.fit, newdata =ABS_ts_linear)
#predictions <- log(predictions)
#Compare Prediction with Actuals  
preds_actuals <- data.frame(Date = all_test_linear$Value.Date, Actual = y_ABS_test, Preds = predictions)

RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(simple.fit$residuals)

mae <- function(error) { mean(abs(error)) }
mae(simple.fit$residuals)
#Line Plot
ggplot(preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("ABS: Regression") + 
  ylab(label="Excess Return") + xlab("2014 PReds vs Actual")

library(corrplot)
ABS_matrix <- sapply(reduced_Data, as.numeric)
corrplot(cor(as.matrix(ABS_matrix), use = "complete.obs", method = "pearson"), method = "circle")


library(caret)
df2 = cor(ABS_tr_linear)
hc = findCorrelation(df2, cutoff=0.9) # putt any value as a "cutoff" 
hc = sort(hc)
colnames(hc)
reduced_Data = ABS_tr_linear[,c(hc)]

#--------------------------
CMBS <-read.csv("CMBS_v2.csv", na.strings = "n/a")
CMBS$X12_Month_Excess_Returns <-  na.locf(CMBS$X12_Month_Excess_Returns, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "X12_Month_Excess_Returns")
CMBS_mod <- CMBS[ , (names(CMBS) %in% keep_cols)]

#Convert to R friendly date format
CMBS_mod$Value.Date <- as.Date(CMBS_mod$Value.Date, "%m/%d/%Y")

#ABS2 <-read.csv("ABS_v2.csv", na.strings = "n/a")
#ABS2_slim <- ABS2[,colSums(is.na(ABS2))<nrow(ABS2)]
#extra_cols <- c("Value.Date","Return.Type","Currency","Quality","MTD.Currency.Return", "VIX3M.Index")
#ABS_slimmer <- ABS2_slim[ , !(names(ABS2_slim) %in% extra_cols)]

#ABS_slimmer$Date <- ABSfile$Value.Date
#ABS_linear <- subset(ABS_slimmer, select = c(36,0:35))

#ABS_linear$Date <- as.Date(ABS_lm$Date, "%m/%d/%Y")

#Subset Train Set
CMBS_train_linear <- CMBS_mod[CMBS_mod$Value.Date >="2008-01-01" & CMBS_mod$Value.Date<= "2012-12-31",]
y_CMBS_train <- CMBS_train_linear$X12_Month_Excess_Returns

CMBS_tr_linear <- CMBS_train_linear[,-1]
CMBS_tr_linear <- CMBS_tr_linear[0:9]

#Subset Test Set
CMBS_test_linear <- CMBS_mod[CMBS_mod$Value.Date >="2014-01-01" & CMBS_mod$Value.Date<= "2014-12-31",]
y_CMBS_test <- CMBS_test_linear$X12_Month_Excess_Returns
CMBS_ts_linear <- CMBS_test_linear[,-1]
CMBS_ts_linear <- CMBS_ts_linear[0:9]

#ABS_test_linear <- ABS_linear[ABS_linear$Date >="2014-01-01" & ABS_linear$Date<= "2014-12-31",]
#ABS_ts_linear <- ABS_test_linear[0:34]

#Train Linear Model
simple.fit = lm(y_CMBS_train ~., data= CMBS_tr_linear )

#Model Summary
summary(simple.fit)

#Make Predictions
predictions = predict.lm(simple.fit, newdata = CMBS_ts_linear)
#predictions <- log(predictions)
#Compare Prediction with Actuals  
preds_actuals <- data.frame(Date = CMBS_test_linear$Value.Date, Actual = y_CMBS_test, Preds = predictions)

RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(simple.fit$residuals)

mae <- function(error) { mean(abs(error)) }
mae(simple.fit$residuals)
#Line Plot
ggplot(preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("CMBS: Regression") + 
ylab(label="Excess Return") + xlab("2014 PReds vs Actual")

#Linear Models

#-------------------------------------------------CMBS-----------------------------------------------------

CMBS <-read.csv("CMBS-monthly.csv", na.strings = "n/a")
CMBS$Future.12.Month.Return <-  na.locf(CMBS$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y", "Future.12.Month.Return" )
CMBS_mod <- CMBS[ , (names(CMBS) %in% keep_cols)]

#Convert to R friendly date format
CMBS_mod$Value.Date <- as.Date(CMBS_mod$Value.Date, "%m/%d/%Y")

#Subset Train Set
CMBS_train_linear <- CMBS_mod[CMBS_mod$Value.Date >="2008-01-01" & CMBS_mod$Value.Date<= "2012-12-31",]
y_CMBS_train <- CMBS_train_linear$Future.12.Month.Return

#CMBS_tr_linear <- CMBS_train_linear[,-1]
#CMBS_tr_linear <- CMBS_tr_linear[0:9]
CMBS_tr_linear <- CMBS_train_linear[2:9]

#Subset Test Set
CMBS_test_linear <- CMBS_mod[CMBS_mod$Value.Date >="2014-01-01" & CMBS_mod$Value.Date<= "2014-12-31",]
y_CMBS_test <- CMBS_test_linear$Future.12.Month.Return
CMBS_ts_linear <- CMBS_test_linear[2:9]


#Train Linear Model
CMBS_simple.fit = lm(y_CMBS_train ~., data= CMBS_tr_linear )

#Model Summary
summary(CMBS_simple.fit)
varImp(CMBS_simple.fit)

#Make Predictions
predictions = predict.lm(CMBS_simple.fit, newdata = CMBS_ts_linear)
#predictions <- log(predictions)
#Compare Prediction with Actuals  
CMBS_preds_actuals <- data.frame(Date = CMBS_test_linear$Value.Date, Actual = y_CMBS_test, Preds = predictions)

CMBS_RMSE <- function(error) { sqrt(mean(error^2)) }
CMBS_RMSE(CMBS_simple.fit$residuals)

CMBS_mae <- function(error) { mean(abs(error)) }
CMBS_mae(CMBS_simple.fit$residuals)

#Line Plot
ggplot(CMBS_preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("CMBS: Regression") + 
  ylab(label="Excess Return") + xlab("2014 Predictions vs Actual")

#----------------------------------ABS----------------------------------------------------------------

ABS <-read.csv("ABS-monthly.csv", na.strings = "n/a")
ABS$Future.12.Month.Return <-  na.locf(ABS$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y", "Future.12.Month.Return")
ABS_mod <- ABS[ , (names(ABS) %in% keep_cols)]

#Convert to R friendly date format
ABS_mod$Value.Date <- as.Date(ABS_mod$Value.Date, "%m/%d/%Y")

#Subset Train Set
ABS_train_linear <- ABS_mod[ABS_mod$Value.Date >="2008-01-01" & ABS_mod$Value.Date<= "2012-12-31",]
y_ABS_train <- ABS_train_linear$Future.12.Month.Return

ABS_tr_linear <- ABS_train_linear[2:9]

#Subset Test Set
ABS_test_linear <- ABS_mod[ABS_mod$Value.Date >="2014-01-01" & ABS_mod$Value.Date<= "2014-12-31",]
y_ABS_test <- ABS_test_linear$Future.12.Month.Return
ABS_ts_linear <- ABS_test_linear[2:9]

#Train Linear Model
ABS_simple.fit = lm(y_ABS_train ~., data= ABS_tr_linear )

#Model Summary
summary(ABS_simple.fit)

#Make Predictions
predictions = predict.lm(ABS_simple.fit, newdata = ABS_ts_linear)
#predictions <- log(predictions)
#Compare Prediction with Actuals  
ABS_preds_actuals <- data.frame(Date = ABS_test_linear$Value.Date, Actual = y_ABS_test, Preds = predictions)

ABS_RMSE <- function(error) { sqrt(mean(error^2)) }
ABS_RMSE(ABS_simple.fit$residuals)

ABS_mae <- function(error) { mean(abs(error)) }
ABS_mae(ABS_simple.fit$residuals)

#Line Plot
ggplot(ABS_preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("ABS: Regression") + 
  ylab(label="Excess Return") + xlab("2014 Predictions vs Actual")

varImp(ABS_simple.fit)
#------------------------------------------USHighYield--------------------------------------------------------
USHighYield <-read.csv("USHighYield-monthly.csv", na.strings = "n/a")
USHighYield$Future.12.Month.Return <-  na.locf(USHighYield$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
USHighYield_mod <- USHighYield[ , (names(USHighYield) %in% keep_cols)]

#Convert to R friendly date format
USHighYield_mod$Value.Date <- as.Date(USHighYield_mod$Value.Date, "%m/%d/%Y")

#Subset Train Set
USHighYield_train_linear <- USHighYield_mod[USHighYield_mod$Value.Date >="2008-01-01" & USHighYield_mod$Value.Date<= "2012-12-31",]
y_USHighYield_train <- USHighYield_train_linear$Future.12.Month.Return

USHighYield_tr_linear <- USHighYield_train_linear[2:10]

#Subset Test Set
USHighYield_test_linear <- USHighYield_mod[USHighYield_mod$Value.Date >="2014-01-01" & USHighYield_mod$Value.Date<= "2014-12-31",]
y_USHighYield_test <- USHighYield_test_linear$Future.12.Month.Return
USHighYield_ts_linear <- USHighYield_test_linear[2:10]

#Train Linear Model
USHighYield_simple.fit = lm(y_USHighYield_train ~., data= USHighYield_tr_linear )

#Model Summary
summary(USHighYield_simple.fit)

#Make Predictions
predictions = predict.lm(USHighYield_simple.fit, newdata = USHighYield_ts_linear)
#predictions <- log(predictions)
#Compare Prediction with Actuals  
USHighYield_preds_actuals <- data.frame(Date = USHighYield_test_linear$Value.Date, Actual = y_USHighYield_test, Preds = predictions)

USHighYield_RMSE <- function(error) { sqrt(mean(error^2)) }
USHighYield_RMSE(USHighYield_simple.fit$residuals)

USHighYield_mae <- function(error) { mean(abs(error)) }
USHighYield_mae(USHighYield_simple.fit$residuals)

#Line Plot
ggplot(USHighYield_preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("USHighYield: Regression") + 
  ylab(label="Excess Return") + xlab("2014 Predictions vs Actual")

#-------------------------------------------------MBS-----------------------------------------------------------
MBS <-read.csv("MBS-monthly.csv", na.strings = "n/a")
MBS$Future.12.Month.Return <-  na.locf(MBS$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
MBS_mod <- MBS[ , (names(MBS) %in% keep_cols)]

#Convert to R friendly date format
MBS_mod$Value.Date <- as.Date(MBS_mod$Value.Date, "%m/%d/%Y")

#Subset Train Set
MBS_train_linear <- MBS_mod[MBS_mod$Value.Date >="2008-01-01" & MBS_mod$Value.Date<= "2012-12-31",]
y_MBS_train <- MBS_train_linear$Future.12.Month.Return

MBS_tr_linear <- MBS_train_linear[2:10]

#Subset Test Set
MBS_test_linear <- MBS_mod[MBS_mod$Value.Date >="2014-01-01" & MBS_mod$Value.Date<= "2014-12-31",]
y_MBS_test <- MBS_test_linear$Future.12.Month.Return
MBS_ts_linear <- MBS_test_linear[2:10]

#Train Linear Model
MBS_simple.fit = lm(y_MBS_train ~., data= MBS_tr_linear )

#Model Summary
summary(MBS_simple.fit)
varImp(MBS_simple.fit)
#Make Predictions
predictions = predict.lm(MBS_simple.fit, newdata = MBS_ts_linear)
#predictions <- log(predictions)
#Compare Prediction with Actuals  
MBS_preds_actuals <- data.frame(Date = MBS_test_linear$Value.Date, Actual = y_MBS_test, Preds = predictions)

MBS_RMSE <- function(error) { sqrt(mean(error^2)) }
MBS_RMSE(MBS_simple.fit$residuals)

MBS_mae <- function(error) { mean(abs(error)) }
MBS_mae(MBS_simple.fit$residuals)

#Line Plot
ggplot(MBS_preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("MBS: Regression") + 
  ylab(label="Excess Return") + xlab("2014 Predictions vs Actual")

#------------------------------GovtRelated--------------------------------------------------------------
GovtRelated <-read.csv("GovtRelated-monthly.csv", na.strings = "n/a")
GovtRelated$Future.12.Month.Return <-  na.locf(GovtRelated$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
GovtRelated_mod <- GovtRelated[ , (names(GovtRelated) %in% keep_cols)]

#Convert to R friendly date format
GovtRelated_mod$Value.Date <- as.Date(GovtRelated_mod$Value.Date, "%m/%d/%Y")

#Subset Train Set
GovtRelated_train_linear <- GovtRelated_mod[GovtRelated_mod$Value.Date >="2008-01-01" & GovtRelated_mod$Value.Date<= "2012-12-31",]
y_GovtRelated_train <- GovtRelated_train_linear$Future.12.Month.Return

GovtRelated_tr_linear <- GovtRelated_train_linear[2:10]

#Subset Test Set
GovtRelated_test_linear <- GovtRelated_mod[GovtRelated_mod$Value.Date >="2014-01-01" & GovtRelated_mod$Value.Date<= "2014-12-31",]
y_GovtRelated_test <- GovtRelated_test_linear$Future.12.Month.Return
GovtRelated_ts_linear <- GovtRelated_test_linear[2:10]

#Train Linear Model
GovtRelated_simple.fit = lm(y_GovtRelated_train ~., data= GovtRelated_tr_linear )

#Model Summary
summary(GovtRelated_simple.fit)

varImp(GovtRelated_simple.fit)

#Make Predictions
predictions = predict.lm(GovtRelated_simple.fit, newdata = GovtRelated_ts_linear)
#predictions <- log(predictions)
#Compare Prediction with Actuals  
GovtRelated_preds_actuals <- data.frame(Date = GovtRelated_test_linear$Value.Date, Actual = y_GovtRelated_test, Preds = predictions)

GovtRelated_RMSE <- function(error) { sqrt(mean(error^2)) }
GovtRelated_RMSE(GovtRelated_simple.fit$residuals)

GovtRelated_mae <- function(error) { mean(abs(error)) }
GovtRelated_mae(GovtRelated_simple.fit$residuals)

#Line Plot
ggplot(GovtRelated_preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("GovtRelated: Regression") + 
  ylab(label="Excess Return") + xlab("2014 Predictions vs Actual")

#-----------------------------IGCorp-------------------------------------------------------
IGCorp <-read.csv("IGCorp-monthly.csv", na.strings = "n/a")
IGCorp$Future.12.Month.Return <-  na.locf(IGCorp$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
IGCorp_mod <- IGCorp[ , (names(IGCorp) %in% keep_cols)]

#Convert to R friendly date format
IGCorp_mod$Value.Date <- as.Date(IGCorp_mod$Value.Date, "%m/%d/%Y")

#Subset Train Set
IGCorp_train_linear <- IGCorp_mod[IGCorp_mod$Value.Date >="2008-01-01" & IGCorp_mod$Value.Date<= "2012-12-31",]
y_IGCorp_train <- IGCorp_train_linear$Future.12.Month.Return

IGCorp_tr_linear <- IGCorp_train_linear[2:10]

#Subset Test Set
IGCorp_test_linear <- IGCorp_mod[IGCorp_mod$Value.Date >="2014-01-01" & IGCorp_mod$Value.Date<= "2014-12-31",]
y_IGCorp_test <- IGCorp_test_linear$Future.12.Month.Return
IGCorp_ts_linear <- IGCorp_test_linear[2:10]

#Train Linear Model
IGCorp_simple.fit = lm(y_IGCorp_train ~., data= IGCorp_tr_linear )

#Model Summary
summary(IGCorp_simple.fit)

#Make Predictions
predictions = predict.lm(IGCorp_simple.fit, newdata = IGCorp_ts_linear)
#predictions <- log(predictions)
#Compare Prediction with Actuals  
IGCorp_preds_actuals <- data.frame(Date = IGCorp_test_linear$Value.Date, Actual = y_IGCorp_test, Preds = predictions)

IGCorp_RMSE <- function(error) { sqrt(mean(error^2)) }
IGCorp_RMSE(IGCorp_simple.fit$residuals)

IGCorp_mae <- function(error) { mean(abs(error)) }
IGCorp_mae(IGCorp_simple.fit$residuals)

#Line Plot
ggplot(IGCorp_preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("IGCorp: Regression") + 
  ylab(label="Excess Return") + xlab("2014 Predictions vs Actual")

varImp(IGCorp_simple.fit)

#-----------------------------all variables IGCorp-------------------------------------------------------
IGCorp <-read.csv("IGCorp-monthly.csv", na.strings = "n/a")
IGCorp$Future.12.Month.Return <-  na.locf(IGCorp$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

nums <- unlist(lapply(IGCorp, is.numeric))

#keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
#IGCorp_mod <- IGCorp[ , (names(IGCorp) %in% keep_cols)]

#Convert to R friendly date format
IGCorp$Value.Date <- as.Date(IGCorp$Value.Date, "%m/%d/%Y")

#Subset Train Set
IGCorp_train_linear <- IGCorp[IGCorp$Value.Date >="2008-01-01" & IGCorp$Value.Date<= "2012-12-31",]
y_IGCorp_train <- IGCorp_train_linear$Future.12.Month.Return
IGCorp_train_linear <- IGCorp_train_linear[ , nums]
extra_cols <- c("MTD.Total.Return","MTD.Currency.Return","Excess.Return.12.Month","Duration..Mod..to.Worst.",
                "Coupon","MTD.Price.Return","Yield.to.Worst","Duration..Mod..Adj..","Total.Return.3.Month",
                "Total.Return.12.Month","Daily.Total.Return","MTD.Coupon.Return","Returns.Modified.Duration")
IGCorp_train_linear <- IGCorp_train_linear[ , !(names(IGCorp_train_linear) %in% extra_cols)]
IGCorp_tr_linear <- IGCorp_train_linear[0:22]

#Subset Test Set
IGCorp_test_linear <- IGCorp[IGCorp$Value.Date >="2014-01-01" & IGCorp$Value.Date<= "2014-12-31",]
y_IGCorp_test <- IGCorp_test_linear$Future.12.Month.Return
IGCorp_ts_linear <- IGCorp_test_linear[ , nums]
IGCorp_ts_linear <- IGCorp_ts_linear[ , !(names(IGCorp_ts_linear) %in% extra_cols)]
IGCorp_ts_linear <- IGCorp_ts_linear[0:22]

#Train Linear Model
IGCorp_simple.fit = lm(y_IGCorp_train ~., data= IGCorp_tr_linear )

#Model Summary
summary(IGCorp_simple.fit)

#Make Predictions
predictions = predict.lm(IGCorp_simple.fit, newdata = IGCorp_ts_linear)
#predictions <- log(predictions)
#Compare Prediction with Actuals  
IGCorp_preds_actuals <- data.frame(Date = IGCorp_test_linear$Value.Date, Actual = y_IGCorp_test, Preds = predictions)

IGCorp_RMSE <- function(error) { sqrt(mean(error^2)) }
IGCorp_RMSE(IGCorp_simple.fit$residuals)

IGCorp_mae <- function(error) { mean(abs(error)) }
IGCorp_mae(IGCorp_simple.fit$residuals)

#Line Plot
ggplot(IGCorp_preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("IGCorp: Regression") + 
  ylab(label="Excess Return") + xlab("2014 Predictions vs Actual")

varImp(IGCorp_simple.fit)


#Model Summary
summary(IGCorp_simple.fit)

#-----------------------------all variables CMBS-------------------------------------------------------
CMBS <-read.csv("CMBS-monthly.csv", na.strings = "n/a")
CMBS$Future.12.Month.Return <-  na.locf(CMBS$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

nums <- unlist(lapply(CMBS, is.numeric))

#keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
#CMBS_mod <- CMBS[ , (names(CMBS) %in% keep_cols)]

#Convert to R friendly date format
CMBS$Value.Date <- as.Date(CMBS$Value.Date, "%m/%d/%Y")

#Subset Train Set
CMBS_train_linear <- CMBS[CMBS$Value.Date >="2008-01-01" & CMBS$Value.Date<= "2012-12-31",]
y_CMBS_train <- CMBS_train_linear$Future.12.Month.Return
CMBS_train_linear <- CMBS_train_linear[ , nums]
extra_cols <- c("MTD.Total.Return","MTD.Currency.Return","Excess.Return.12.Month","Duration..Mod..to.Worst.",
                "Coupon","MTD.Price.Return","Yield.to.Worst","Duration..Mod..Adj..","Total.Return.3.Month",
                "Total.Return.12.Month","Daily.Total.Return","MTD.Coupon.Return","Returns.Modified.Duration")
CMBS_train_linear <- CMBS_train_linear[ , !(names(CMBS_train_linear) %in% extra_cols)]
CMBS_tr_linear <- CMBS_train_linear[0:22]

#Subset Test Set
CMBS_test_linear <- CMBS[CMBS$Value.Date >="2014-01-01" & CMBS$Value.Date<= "2014-12-31",]
y_CMBS_test <- CMBS_test_linear$Future.12.Month.Return
CMBS_ts_linear <- CMBS_test_linear[ , nums]
CMBS_ts_linear <- CMBS_ts_linear[ , !(names(CMBS_ts_linear) %in% extra_cols)]
CMBS_ts_linear <- CMBS_ts_linear[0:22]

#Train Linear Model
CMBS_simple.fit = lm(y_CMBS_train ~., data= CMBS_tr_linear )

#Model Summary
summary(CMBS_simple.fit)

#Make Predictions
predictions = predict.lm(CMBS_simple.fit, newdata = CMBS_ts_linear)
#predictions <- log(predictions)
#Compare Prediction with Actuals  
CMBS_preds_actuals <- data.frame(Date = CMBS_test_linear$Value.Date, Actual = y_CMBS_test, Preds = predictions)

CMBS_RMSE <- function(error) { sqrt(mean(error^2)) }
CMBS_RMSE(CMBS_simple.fit$residuals)

CMBS_mae <- function(error) { mean(abs(error)) }
CMBS_mae(CMBS_simple.fit$residuals)

#Line Plot
ggplot(CMBS_preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("CMBS: More Variables Regression") + 
  ylab(label="Excess Return") + xlab("2014 Predictions vs Actual")

varImp(CMBS_simple.fit)

#-----------------------------all variables MBS-------------------------------------------------------
MBS <-read.csv("MBS-monthly.csv", na.strings = "n/a")
MBS$Future.12.Month.Return <-  na.locf(MBS$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

nums <- unlist(lapply(MBS, is.numeric))

#keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
#MBS_mod <- MBS[ , (names(MBS) %in% keep_cols)]

#Convert to R friendly date format
MBS$Value.Date <- as.Date(MBS$Value.Date, "%m/%d/%Y")

#Subset Train Set
MBS_train_linear <- MBS[MBS$Value.Date >="2008-01-01" & MBS$Value.Date<= "2012-12-31",]
y_MBS_train <- MBS_train_linear$Future.12.Month.Return
MBS_train_linear <- MBS_train_linear[ , nums]
extra_cols <- c("MTD.Total.Return","MTD.Currency.Return","Excess.Return.12.Month","Duration..Mod..to.Worst.",
                "Coupon","MTD.Price.Return","Yield.to.Worst","Duration..Mod..Adj..","Total.Return.3.Month",
                "Total.Return.12.Month","Daily.Total.Return","MTD.Coupon.Return","Returns.Modified.Duration")
MBS_train_linear <- MBS_train_linear[ , !(names(MBS_train_linear) %in% extra_cols)]
MBS_tr_linear <- MBS_train_linear[0:22]

#Subset Test Set
MBS_test_linear <- MBS[MBS$Value.Date >="2014-01-01" & MBS$Value.Date<= "2014-12-31",]
y_MBS_test <- MBS_test_linear$Future.12.Month.Return
MBS_ts_linear <- MBS_test_linear[ , nums]
MBS_ts_linear <- MBS_ts_linear[ , !(names(MBS_ts_linear) %in% extra_cols)]
MBS_ts_linear <- MBS_ts_linear[0:22]

#Train Linear Model
MBS_simple.fit = lm(y_MBS_train ~., data= MBS_tr_linear )

#Model Summary
summary(MBS_simple.fit)

#Make Predictions
predictions = predict.lm(MBS_simple.fit, newdata = MBS_ts_linear)
#predictions <- log(predictions)
#Compare Prediction with Actuals  
MBS_preds_actuals <- data.frame(Date = MBS_test_linear$Value.Date, Actual = y_MBS_test, Preds = predictions)

MBS_RMSE <- function(error) { sqrt(mean(error^2)) }
MBS_RMSE(MBS_simple.fit$residuals)

MBS_mae <- function(error) { mean(abs(error)) }
MBS_mae(MBS_simple.fit$residuals)

#Line Plot
ggplot(MBS_preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("MBS More Variables: Regression") + 
  ylab(label="Excess Return") + xlab("2014 Predictions vs Actual")

varImp(MBS_simple.fit)

#-----------------------------all variables ABS-------------------------------------------------------
ABS <-read.csv("ABS-monthly.csv", na.strings = "n/a")
ABS$Future.12.Month.Return <-  na.locf(ABS$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

nums <- unlist(lapply(ABS, is.numeric))

#keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
#ABS_mod <- ABS[ , (names(ABS) %in% keep_cols)]

#Convert to R friendly date format
ABS$Value.Date <- as.Date(ABS$Value.Date, "%m/%d/%Y")

#Subset Train Set
ABS_train_linear <- ABS[ABS$Value.Date >="2008-01-01" & ABS$Value.Date<= "2012-12-31",]
y_ABS_train <- ABS_train_linear$Future.12.Month.Return
ABS_train_linear <- ABS_train_linear[ , nums]
extra_cols <- c("MTD.Total.Return","MTD.Currency.Return","Excess.Return.12.Month","Duration..Mod..to.Worst.",
                "Coupon","MTD.Price.Return","Yield.to.Worst","Duration..Mod..Adj..","Total.Return.3.Month",
                "Total.Return.12.Month","Daily.Total.Return","MTD.Coupon.Return","Returns.Modified.Duration")
ABS_train_linear <- ABS_train_linear[ , !(names(ABS_train_linear) %in% extra_cols)]
ABS_tr_linear <- ABS_train_linear[0:22]

#Subset Test Set
ABS_test_linear <- ABS[ABS$Value.Date >="2014-01-01" & ABS$Value.Date<= "2014-12-31",]
y_ABS_test <- ABS_test_linear$Future.12.Month.Return
ABS_ts_linear <- ABS_test_linear[ , nums]
ABS_ts_linear <- ABS_ts_linear[ , !(names(ABS_ts_linear) %in% extra_cols)]
ABS_ts_linear <- ABS_ts_linear[0:22]

#Train Linear Model
ABS_simple.fit = lm(y_ABS_train ~., data= ABS_tr_linear )

#Model Summary
summary(ABS_simple.fit)

#Make Predictions
predictions = predict.lm(ABS_simple.fit, newdata = ABS_ts_linear)
#predictions <- log(predictions)
#Compare Prediction with Actuals  
ABS_preds_actuals <- data.frame(Date = ABS_test_linear$Value.Date, Actual = y_ABS_test, Preds = predictions)

ABS_RMSE <- function(error) { sqrt(mean(error^2)) }
ABS_RMSE(ABS_simple.fit$residuals)

ABS_mae <- function(error) { mean(abs(error)) }
ABS_mae(ABS_simple.fit$residuals)

#Line Plot
ggplot(ABS_preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("ABS More Variables : Regression") + 
  ylab(label="Excess Return") + xlab("2014 Predictions vs Actual")

varImp(ABS_simple.fit)


#---------------------------------logistic regression for predicting positive return-------------

#------------------------IGCorp------------------------------------------------------------------
IGCorp <-read.csv("IGCorp-monthly.csv", na.strings = "n/a")
IGCorp$Future.12.Month.Return <-  na.locf(IGCorp$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
IGCorp_mod <- IGCorp[ , (names(IGCorp) %in% keep_cols)]

#Convert to R friendly date format
IGCorp_mod$Value.Date <- as.Date(IGCorp_mod$Value.Date, "%m/%d/%Y")
IGCorp_mod$Positive.Return <- ifelse(IGCorp_mod$Future.12.Month.Return > 0,1,0)

#Subset Train Set
IGCorp_train_linear <- IGCorp_mod[IGCorp_mod$Value.Date >="2008-01-01" & IGCorp_mod$Value.Date<= "2012-12-31",]
logit_IGCorp_train <- IGCorp_train_linear$Positive.Return

IGCorp_tr_linear <- IGCorp_train_linear[2:10]

#Subset Test Set
IGCorp_test_linear <- IGCorp_mod[IGCorp_mod$Value.Date >="2014-01-01" & IGCorp_mod$Value.Date<= "2014-12-31",]
logit_IGCorp_test <- IGCorp_test_linear$Positive.Return
IGCorp_ts_linear <- IGCorp_test_linear[2:10]

#logistic regression model
IGCorp_log <- glm(logit_IGCorp_train ~., family=binomial(link='logit'), data= IGCorp_tr_linear)
summary(IGCorp_log)

IGCorp_log <- glm(logit_IGCorp_train ~., family=binomial(link='logit'), data= IGCorp_tr_linear)
anova(IGCorp_log, test="Chisq")

IGCorpfitted.results <- predict(IGCorp_log,newdata=IGCorp_ts_linear,type='response')
IGCorpfitted.results <- ifelse(IGCorpfitted.results > 0.5,1,0)

IGCorpmisClasificError <- mean(IGCorpfitted.results != logit_IGCorp_test)
print(paste('Accuracy',1-IGCorpmisClasificError))

#------------------------GovtRelated------------------------------------------------------------------
GovtRelated <-read.csv("GovtRelated-monthly.csv", na.strings = "n/a")
GovtRelated$Future.12.Month.Return <-  na.locf(GovtRelated$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
GovtRelated_mod <- GovtRelated[ , (names(GovtRelated) %in% keep_cols)]

#Convert to R friendly date format
GovtRelated_mod$Value.Date <- as.Date(GovtRelated_mod$Value.Date, "%m/%d/%Y")
GovtRelated_mod$Positive.Return <- ifelse(GovtRelated_mod$Future.12.Month.Return > 0,1,0)

#Subset Train Set
GovtRelated_train_linear <- GovtRelated_mod[GovtRelated_mod$Value.Date >="2008-01-01" & GovtRelated_mod$Value.Date<= "2012-12-31",]
logit_GovtRelated_train <- GovtRelated_train_linear$Positive.Return

GovtRelated_tr_linear <- GovtRelated_train_linear[2:10]

#Subset Test Set
GovtRelated_test_linear <- GovtRelated_mod[GovtRelated_mod$Value.Date >="2014-01-01" & GovtRelated_mod$Value.Date<= "2014-12-31",]
logit_GovtRelated_test <- GovtRelated_test_linear$Positive.Return
GovtRelated_ts_linear <- GovtRelated_test_linear[2:10]

#logistic regression model
GovtRelated_log <- glm(logit_GovtRelated_train ~., family=binomial, data= GovtRelated_tr_linear)
summary(GovtRelated_log)

GovtRelatedfitted.results <- predict(GovtRelated_log,newdata=GovtRelated_ts_linear,type='response')
GovtRelatedfitted.results <- ifelse(GovtRelatedfitted.results > 0.5,1,0)

GovtRelatedmisClasificError <- mean(GovtRelatedfitted.results != logit_GovtRelated_test)
print(paste('Accuracy',1-GovtRelatedmisClasificError))

#------------------------MBS------------------------------------------------------------------
MBS <-read.csv("MBS-monthly.csv", na.strings = "n/a")
MBS$Future.12.Month.Return <-  na.locf(MBS$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
MBS_mod <- MBS[ , (names(MBS) %in% keep_cols)]

#Convert to R friendly date format
MBS_mod$Value.Date <- as.Date(MBS_mod$Value.Date, "%m/%d/%Y")
MBS_mod$Positive.Return <- ifelse(MBS_mod$Future.12.Month.Return > 0,1,0)

#Subset Train Set
MBS_train_linear <- MBS_mod[MBS_mod$Value.Date >="2008-01-01" & MBS_mod$Value.Date<= "2012-12-31",]
logit_MBS_train <- MBS_train_linear$Positive.Return

MBS_tr_linear <- MBS_train_linear[2:10]

#Subset Test Set
MBS_test_linear <- MBS_mod[MBS_mod$Value.Date >="2014-01-01" & MBS_mod$Value.Date<= "2014-12-31",]
logit_MBS_test <- MBS_test_linear$Positive.Return
MBS_ts_linear <- MBS_test_linear[2:10]

#logistic regression model
MBS_log <- glm(logit_MBS_train ~., family=binomial(link='logit'), data= MBS_tr_linear)
summary(MBS_log)

MBS_log <- glm(logit_MBS_train ~., family=binomial(link='logit'), data= MBS_tr_linear)
anova(MBS_log, test="Chisq")

MBSfitted.results <- predict(MBS_log,newdata=MBS_ts_linear,type='response')
MBSfitted.results <- ifelse(MBSfitted.results > 0.5,1,0)

MBSmisClasificError <- mean(MBSfitted.results != logit_MBS_test)
print(paste('Accuracy',1-MBSmisClasificError))

#------------------------CMBS------------------------------------------------------------------
CMBS <-read.csv("CMBS-monthly.csv", na.strings = "n/a")
CMBS$Future.12.Month.Return <-  na.locf(CMBS$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
CMBS_mod <- CMBS[ , (names(CMBS) %in% keep_cols)]

#Convert to R friendly date format
CMBS_mod$Value.Date <- as.Date(CMBS_mod$Value.Date, "%m/%d/%Y")
CMBS_mod$Positive.Return <- ifelse(CMBS_mod$Future.12.Month.Return > 0,1,0)

#Subset Train Set
CMBS_train_linear <- CMBS_mod[CMBS_mod$Value.Date >="2008-01-01" & CMBS_mod$Value.Date<= "2012-12-31",]
logit_CMBS_train <- CMBS_train_linear$Positive.Return

CMBS_tr_linear <- CMBS_train_linear[2:10]

#Subset Test Set
CMBS_test_linear <- CMBS_mod[CMBS_mod$Value.Date >="2014-01-01" & CMBS_mod$Value.Date<= "2014-12-31",]
logit_CMBS_test <- CMBS_test_linear$Positive.Return
CMBS_ts_linear <- CMBS_test_linear[2:10]

#logistic regression model
CMBS_log <- glm(logit_CMBS_train ~., family=binomial(link='logit'), data= CMBS_tr_linear)
summary(CMBS_log)

CMBS_log <- glm(logit_CMBS_train ~., family=binomial(link='logit'), data= CMBS_tr_linear)
anova(CMBS_log, test="Chisq")

CMBSfitted.results <- predict(CMBS_log,newdata=CMBS_ts_linear,type='response')
CMBSfitted.results <- ifelse(CMBSfitted.results > 0.5,1,0)

CMBSmisClasificError <- mean(CMBSfitted.results != logit_CMBS_test)
print(paste('Accuracy',1-CMBSmisClasificError))

#------------------------ABS------------------------------------------------------------------
ABS <-read.csv("ABS-monthly.csv", na.strings = "n/a")
ABS$Future.12.Month.Return <-  na.locf(ABS$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
ABS_mod <- ABS[ , (names(ABS) %in% keep_cols)]

#Convert to R friendly date format
ABS_mod$Value.Date <- as.Date(ABS_mod$Value.Date, "%m/%d/%Y")
ABS_mod$Positive.Return <- ifelse(ABS_mod$Future.12.Month.Return > 0,1,0)

#Subset Train Set
ABS_train_linear <- ABS_mod[ABS_mod$Value.Date >="2008-01-01" & ABS_mod$Value.Date<= "2012-12-31",]
logit_ABS_train <- ABS_train_linear$Positive.Return

ABS_tr_linear <- ABS_train_linear[2:10]

#Subset Test Set
ABS_test_linear <- ABS_mod[ABS_mod$Value.Date >="2014-01-01" & ABS_mod$Value.Date<= "2014-12-31",]
logit_ABS_test <- ABS_test_linear$Positive.Return
ABS_ts_linear <- ABS_test_linear[2:10]

#logistic regression model
ABS_log <- glm(logit_ABS_train ~., family=binomial(link='logit'), data= ABS_tr_linear)
summary(ABS_log)

ABS_log <- glm(logit_ABS_train ~., family=binomial(link='logit'), data= ABS_tr_linear)
anova(ABS_log, test="Chisq")

ABSfitted.results <- predict(ABS_log,newdata=ABS_ts_linear,type='response')
ABSfitted.results <- ifelse(ABSfitted.results > 0.5,1,0)

ABSmisClasificError <- mean(ABSfitted.results != logit_ABS_test)
print(paste('Accuracy',1-ABSmisClasificError))

#------------------------USHighYield------------------------------------------------------------------
USHighYield <-read.csv("USHighYield-monthly.csv", na.strings = "n/a")
USHighYield$Future.12.Month.Return <-  na.locf(USHighYield$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

keep_cols <- c("Value.Date", "S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y","Excess.Return", "Future.12.Month.Return")
USHighYield_mod <- USHighYield[ , (names(USHighYield) %in% keep_cols)]

#Convert to R friendly date format
USHighYield_mod$Value.Date <- as.Date(USHighYield_mod$Value.Date, "%m/%d/%Y")
USHighYield_mod$Positive.Return <- ifelse(USHighYield_mod$Future.12.Month.Return > 0,1,0)

#Subset Train Set
USHighYield_train_linear <- USHighYield_mod[USHighYield_mod$Value.Date >="2008-01-01" & USHighYield_mod$Value.Date<= "2012-12-31",]
logit_USHighYield_train <- USHighYield_train_linear$Positive.Return

USHighYield_tr_linear <- USHighYield_train_linear[2:10]

#Subset Test Set
USHighYield_test_linear <- USHighYield_mod[USHighYield_mod$Value.Date >="2014-01-01" & USHighYield_mod$Value.Date<= "2014-12-31",]
logit_USHighYield_test <- USHighYield_test_linear$Positive.Return
USHighYield_ts_linear <- USHighYield_test_linear[2:10]

#logistic regression model
USHighYield_log <- glm(logit_USHighYield_train ~., family=binomial(link='logit'), data= USHighYield_tr_linear)
summary(USHighYield_log)

USHighYield_log <- glm(logit_USHighYield_train ~., family=binomial(link='logit'), data= USHighYield_tr_linear)
anova(USHighYield_log, test="Chisq")

USHighYieldfitted.results <- predict(USHighYield_log,newdata=USHighYield_ts_linear,type='response')
USHighYieldfitted.results <- ifelse(USHighYieldfitted.results > 0.5,1,0)

USHighYieldmisClasificError <- mean(USHighYieldfitted.results != logit_USHighYield_test)
print(paste('Accuracy',1-USHighYieldmisClasificError))

#----------v3 data
newABS <-read.csv("ABS-monthly_v3.csv", na.strings = "n/a")

#Remove the 10 columns that all have missing data
newmod_ABS <- newABS[,colSums(is.na(newABS))<nrow(newABS)]


newmod_ABS$Value.Date <- as.Date(newmod_ABS$Value.Date, "%m/%d/%Y")

#train
newABS_train_linear <- newmod_ABS[newmod_ABS$Value.Date >="2008-01-01" & newmod_ABS$Value.Date<= "2012-12-31",]
y_newABS_train <- newABS_train_linear$Future.12.Month.Return

nums <- unlist(lapply(newABS_train_linear, is.numeric))

newABS_tr_linear <- newABS_train_linear[ , nums]
newABS_tr_linear <- newABS_tr_linear[,-which(names(newABS_tr_linear) == "Future.12.Month.Return")] 

#test
newABS_test_linear <- newmod_ABS[newmod_ABS$Value.Date >="2014-01-01" & newmod_ABS$Value.Date <= "2014-12-31",]
y_newABS_test <- newABS_test_linear$Future.12.Month.Return

newABS_ts_linear <- newABS_test_linear[ , nums]
newABS_ts_linear <- newABS_ts_linear[,-which(names(newABS_ts_linear) == "Future.12.Month.Return")] 

#Remove extra columns that aren't numeric or useful
extra_cols <- c("Value.Date","Return.Type","Currency","Quality","MTD.Currency.Return")
new_ABS_cor <- newmod_ABS[ , !(names(newmod_ABS) %in% extra_cols)]


numeric_ABS <- new_ABS_cor[ , nums]

#Train Linear Model
newABS_simple.fit = lm(y_newABS_train ~., data= newABS_tr_linear )

#Model Summary
summary(newABS_simple.fit)

x <- IGCorp_mod[,4:8]
y <- IGCorp_mod[,9]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
