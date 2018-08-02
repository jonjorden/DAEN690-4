setwd("C:/Users/jonkennedy/Downloads/GMU")

ABSfile <-read.csv("ABS.csv", na.strings = "n/a") #6803
#count_abs_na <- sapply(ABSfile, function(x) length(which(x=="n/a")))
abs_na <- sapply(ABSfile, function(x) length(which(x=="n/a"))/nrow(ABSfile))

CMBSfile <-read.csv("CMBS.csv", na.strings = "n/a") #4905
cmbs_na <-sapply(CMBSfile, function(x) length(which(x=="n/a"))/nrow(CMBSfile))

Treasuryfile <-read.csv("Treasury.csv", na.strings = "n/a")
treasury_na <- sapply(Treasuryfile, function(x) length(which(x=="n/a"))/nrow(Treasuryfile))

GOVTfile <-read.csv("GovtRelated.csv", na.strings = "n/a")
govt_na <- sapply(GOVTfile, function(x) length(which(x=="n/a"))/nrow(GOVTfile))

IGCorpfile <-read.csv("IGCorp.csv", na.strings = "n/a")
igcorp_na <- sapply(IGCorpfile, function(x) length(which(x=="n/a"))/nrow(IGCorpfile))

MBSfile <-read.csv("MBS.csv", na.strings = "n/a")
mbs_na <- sapply(MBSfile, function(x) length(which(x=="n/a"))/nrow(MBSfile))

USHighYieldfile <-read.csv("USHighYield.csv", na.strings = "n/a")
ushighyield_na <- sapply(USHighYieldfile, function(x) length(which(x=="n/a"))/nrow(USHighYieldfile))

allnas <- cbind(mbs_na,igcorp_na,govt_na,treasury_na,cmbs_na,abs_na,ushighyield_na)
allnas

ABSfile[ABSfile == "n/a"] <- NA
ABS_trimmed <- ABSfile[,colSums(is.na(ABSfile))<nrow(ABSfile)]
extra_cols <- c("Value.Date","Return.Type","Currency","Quality","MTD.Currency.Return")
ABS_cor <- ABS_trimmed[ , !(names(ABS_trimmed) %in% extra_cols)]

ABS2 <-read.csv("ABS_v2.csv", na.strings = "n/a")
ABS2_slim <- ABS2[,colSums(is.na(ABS2))<nrow(ABS2)]
extra_cols <- c("Value.Date","Return.Type","Currency","Quality","MTD.Currency.Return", "VIX3M.Index")
ABS_slimmer <- ABS2_slim[ , !(names(ABS2_slim) %in% extra_cols)]
ABS_no_resp <- ABS_slimmer[0:30]

ABS_matrix_2 <- sapply(ABS_no_resp, as.numeric)
corrplot(cor(as.matrix(ABS_matrix_2), use = "complete.obs", method = "pearson"), method = "circle")

cor(as.matrix(ABS_matrix), use = "complete.obs", method = "pearson")

library(corrplot)
corrplot(cor(as.matrix(ABS_matrix), use = "complete.obs", method = "pearson"), method = "circle")

comp_ABS <- na.omit(ABS_cor)
num_vars <- c("Number.Issues..Returns.", "Returns.Modified.Duration", "Daily.Total.Return", "MTD.Price.Return",
              "MTD.Coupon.Return", "MTD.Paydown.Return", "MTD.Total.Return", "Total.Return.3.Month", "Total.Return.6.Month",
              "YTD.Total.Return","Total.Return.12.Month", "Since.Inception.Total.Return", "Number.Issues..Statistics.",
              "Duration..Mod..Adj..", "Convexity", "Coupon", "Maturity", "Price", "Yield.to.Worst", "Market.Value..MM.",
              "Yield.to.Maturity", "Blended.Spread.Duration", "Excess.Return", "OAS", "Amt.Outstanding..MM.")

library(caret)
library(ggplot2)
library(tidyr)

ggplot(gather(ABS_cor), aes(value)) + geom_histogram(bins = 10) + facet_wrap(~key, scales = 'free_x')

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

ggplot(gather(ABS_matrix_2), aes(value)) + geom_histogram(bins = 10) + facet_wrap(~key, scales = 'free_x')
str(ABS_matrix_2)

#Split into deciles
quantile(ABS_cor$Excess.Return, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5)


ABS_cor$Returns.Modified.Duration<- na.locf(ABS_cor$Returns.Modified.Duration, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)
ABS_cor$decile <- with(ABS_cor, cut(ABS_cor$Excess.Return, breaks=quantile(ABS_cor$Excess.Return, na.rm = TRUE, 
                                                                           prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

library(zoo)
library(ggplot2)

#Read the file and impute with last known value for the response variable
CMBS <-read.csv("CMBS_v2.csv", na.strings = "n/a")
CMBS$X12_Month_Excess_Returns <-  na.locf(CMBS$X12_Month_Excess_Returns, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

#Keep these columns
keep_cols <- c("Value.Date","S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y", "X12_Month_Excess_Returns")
CMBS_mod <- CMBS[ , (names(CMBS) %in% keep_cols)]

#Convert to R friendly date format
CMBS_mod$Value.Date <- as.Date(CMBS_mod$Value.Date, "%m/%d/%Y")

#Remove data that we don't have one year forward data
CMBS_mod <- CMBS_mod[CMBS_mod$Value.Date <"2017-05-12",]
#Shows Deciles
#decile_range <- quantile(CMBS_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5)

#Shows Decile Range for each observation
#CMBS_mod$SP500_decile <- with(CMBS_mod, cut(CMBS_mod$S.P500, breaks=quantile(CMBS_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

#Deciles for each column
CMBS_mod$SP500_decile <- with(CMBS_mod, cut(CMBS_mod$S.P500, labels = 1:10, breaks=quantile(CMBS_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE, ordered_result = TRUE))
CMBS_mod$FedRates_decile <- with(CMBS_mod, cut(CMBS_mod$FedRates, labels = 1:10, breaks=quantile(CMBS_mod$FedRates, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
CMBS_mod$LIBOR_decile <- with(CMBS_mod, cut(CMBS_mod$LIBOR, labels = 1:10, breaks=quantile(CMBS_mod$LIBOR, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
CMBS_mod$VIX1M_decile <- with(CMBS_mod, cut(CMBS_mod$VIX1M.Index, labels = 1:10, breaks=quantile(CMBS_mod$VIX1M.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
CMBS_mod$VIX_VXV_decile <- with(CMBS_mod, cut(CMBS_mod$VIX.VXV, labels = 1:10, breaks=quantile(CMBS_mod$VIX.VXV, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
CMBS_mod$USGG2YR_decile <- with(CMBS_mod, cut(CMBS_mod$USGG2YR.Index, labels = 1:10, breaks=quantile(CMBS_mod$USGG2YR.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
CMBS_mod$USGG10YR_decile <- with(CMBS_mod, cut(CMBS_mod$USGG10YR.Index, labels = 1:10, breaks=quantile(CMBS_mod$USGG10YR.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
CMBS_mod$T10Y2Y_decile <- with(CMBS_mod, cut(CMBS_mod$T10Y2Y, labels = 1:10, breaks=quantile(CMBS_mod$T10Y2Y, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

#Calculate Mean Excess Return for each Decile
CMBS_SP500_Excess_Mean <- aggregate(list(CMBS_SP500_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$SP500_decile), mean)
CMBS_FedRate_Excess_Mean <- aggregate(list(CMBS_FedRate_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$FedRates_decile), mean)
CMBS_LIBOR_Excess_Mean <- aggregate(list(CMBS_LIBOR_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$LIBOR_decile), mean)
CMBS_VIX1M_Excess_Mean <- aggregate(list(CMBS_VIX1M_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$VIX1M_decile), mean)
CMBS_VIX_VXV_Excess_Mean <- aggregate(list(CMBS_VIX_VXV_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$VIX_VXV_decile), mean)
CMBS_USGG2YR_Excess_Mean <- aggregate(list(CMBS_USGG2YR_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$USGG2YR_decile), mean)
CMBS_USGG10YR_Excess_Mean <- aggregate(list(CMBS_USGG10YR_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$USGG10YR_decile), mean)
CMBS_T10Y2Y_Excess_Mean <- aggregate(list(CMBS_T10Y2Y_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$T10Y2Y_decile), mean)


#qplot(CMBS_SP500_Excess_Mean$CMBS_SP500_Mean, geom = "histogram")
#Bar Charts
ggplot(CMBS_SP500_Excess_Mean, aes(x=CMBS_SP500_Excess_Mean$Decile, y=CMBS_SP500_Excess_Mean$CMBS_SP500_Mean)) +
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_FedRate_Excess_Mean, aes(x=CMBS_FedRate_Excess_Mean$Decile, y=CMBS_FedRate_Excess_Mean$CMBS_FedRate_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_LIBOR_Excess_Mean, aes(x=CMBS_LIBOR_Excess_Mean$Decile, y=CMBS_LIBOR_Excess_Mean$CMBS_LIBOR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_VIX1M_Excess_Mean, aes(x=CMBS_VIX1M_Excess_Mean$Decile, y=CMBS_VIX1M_Excess_Mean$CMBS_FedRate_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_VIX_VXV_Excess_Mean, aes(x=CMBS_VIX_VXV_Excess_Mean$Decile, y=CMBS_VIX_VXV_Excess_Mean$CMBS_FedRate_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_USGG2YR_Excess_Mean, aes(x=CMBS_USGG2YR_Excess_Mean$Decile, y=CMBS_USGG2YR_Excess_Mean$CMBS_FedRate_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_USGG10YR_Excess_Mean, aes(x=CMBS_USGG10YR_Excess_Mean$Decile, y=CMBS_USGG10YR_Excess_Mean$CMBS_FedRate_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_T10Y2Y_Excess_Mean, aes(x=CMBS_T10Y2Y_Excess_Mean$Decile, y=CMBS_T10Y2Y_Excess_Mean$CMBS_FedRate_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')

comb_CMBS <- Reduce(function(x, y) merge(x, y, by="Decile", all=TRUE), list(CMBS_SP500_Excess_Mean,CMBS_FedRate_Excess_Mean, CMBS_LIBOR_Excess_Mean, CMBS_VIX1M_Excess_Mean, CMBS_VIX_VXV_Excess_Mean, CMBS_USGG2YR_Excess_Mean, CMBS_USGG10YR_Excess_Mean, CMBS_T10Y2Y_Excess_Mean))
comb_CMBS <- comb_CMBS[ order(as.numeric(comb_CMBS$Decile)), ]