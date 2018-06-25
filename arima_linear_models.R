#ARIMA Model

#Packages
library(tseries)
library(MASS)
library(forecast)

#Add the date back and make it the 1st column
ABS_cor$Date <- ABSfile$Value.Date
ABS_lm <- subset(ABS_cor, select = c(26,0:25))

#Add more columns
ABS_lm[27:37] <- ABS_slimmer[25:35]
ABS_lm$Date <- as.Date(ABS_lm$Date, "%m/%d/%Y")

#Subset Train Set
ABS_lm_train <- ABS_lm[ABS_lm$Date >="2010-01-01" & ABS_lm$Date<= "2012-12-31",]

#Subset Test Set
ABS_lm_test <- ABS_lm[ABS_lm$Date >="2014-01-01" & ABS_lm$Date<= "2014-12-31",]

#Two variable ds
ABS_lm_tr2 <- ABS_lm_train[,c("Date","Price")]
ABS_lm_tst2 <- ABS_lm_test[,c("Date","Price")]

#Log of price
ABS_lm_tr2$Price = log(ABS_lm_tr2$Price)
train_data <- na.omit(ABS_lm_tr2)
test_data <- na.omit(ABS_lm_tst2)
#Autocorrelation and Partial Autocorrelation
acf(train_data$Price, lag.max = 20)

pacf(train_data$Price, lag.max = 20)

#Stationary Model
lnABS = train_data$Price

#Fit Model beginning at 2010 with a frequency of 250- roughly one year of data
pricearima <- ts(lnABS, start = c(2010,01,01), frequency = 250 )
fitlnABS <- auto.arima(pricearima, D=1)
fitlnABS
plot(pricearima, type='l')

exp(lnABS)
forecastedvalues_ln = forecast(fitlnABS, h=500)
forecastedvalues_ln
plot(forecastedvalues_ln)

forecastedvaluesextracted = as.numeric(forecastedvalues_ln$mean)
finalforecastvalues = exp(forecastedvaluesextracted)
finalforecastvalues
results <- data.frame(Date = test_data$Date, ActualPrice = test_data$Price, ForecastedPrice = finalforecastvalues[251:500])
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

#Linear Models
ABS2 <-read.csv("ABS_v2.csv", na.strings = "n/a")
ABS2_slim <- ABS2[,colSums(is.na(ABS2))<nrow(ABS2)]
extra_cols <- c("Value.Date","Return.Type","Currency","Quality","MTD.Currency.Return", "VIX3M.Index")
ABS_slimmer <- ABS2_slim[ , !(names(ABS2_slim) %in% extra_cols)]

ABS_slimmer$Date <- ABSfile$Value.Date
ABS_linear <- subset(ABS_slimmer, select = c(36,0:35))

ABS_linear$Date <- as.Date(ABS_lm$Date, "%m/%d/%Y")

#Subset Train Set
ABS_train_linear <- ABS_linear[ABS_linear$Date >="2010-01-01" & ABS_linear$Date<= "2012-12-31",]
ABS_tr_linear <- ABS_train_linear[0:34]

#Subset Test Set
ABS_test_linear <- ABS_linear[ABS_linear$Date >="2014-01-01" & ABS_linear$Date<= "2014-12-31",]
ABS_ts_linear <- ABS_test_linear[0:34]

#Train Linear Model
simple.fit = lm(ABS_train_linear$Resp_Price ~., data= ABS_tr_linear )

#Model Summary
summary(simple.fit)

#Make Predictions
predictions = predict.lm(simple.fit, newdata = ABS_ts_linear)

#Compare Prediction with Actuals
preds_actuals <- data.frame(Date = ABS_test_linear$Date, Actual = ABS_test_linear$Resp_Price, Preds = predictions)

#Line Plot
ggplot(preds_actuals, aes(x = Date)) + 
  geom_line(aes(y = Actual), colour="blue") + 
  geom_line(aes(y = Preds), colour = "red") + ggtitle("Regression")
ylab(label="Price") + 
  xlab("2014 PReds vs Actual")