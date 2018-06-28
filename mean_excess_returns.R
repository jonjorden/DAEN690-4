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
CMBS_VIX1M_Excess_Mean <- aggregate(list(CMBS_FedRate_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$VIX1M_decile), mean)
CMBS_VIX_VXV_Excess_Mean <- aggregate(list(CMBS_FedRate_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$VIX_VXV_decile), mean)
CMBS_USGG2YR_Excess_Mean <- aggregate(list(CMBS_FedRate_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$USGG2YR_decile), mean)
CMBS_USGG10YR_Excess_Mean <- aggregate(list(CMBS_FedRate_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$USGG10YR_decile), mean)
CMBS_T10Y2Y_Excess_Mean <- aggregate(list(CMBS_FedRate_Mean=CMBS_mod$X12_Month_Excess_Returns), list(Decile = CMBS_mod$T10Y2Y_decile), mean)


#qplot(CMBS_SP500_Excess_Mean$CMBS_SP500_Mean, geom = "histogram")
#Bar charts
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