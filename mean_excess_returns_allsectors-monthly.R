library(zoo)
library(ggplot2)

# ---------------------------------------------- ABS --------------------------------------------------------------------

#Read the file and impute with last known value for the response variable
ABS <-read.csv("ABS-monthly.csv", na.strings = "n/a")
ABS$Future.12.Month.Return <-  na.locf(ABS$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

#Keep these columns
keep_cols <- c("Value.Date","S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y", "Future.12.Month.Return")
ABS_mod <- ABS[ , (names(ABS) %in% keep_cols)]

#Convert to R friendly date format
ABS_mod$Value.Date <- as.Date(ABS_mod$Value.Date, "%m/%d/%Y")

#Remove data that we don't have one year forward data
ABS_mod <- ABS_mod[ABS_mod$Value.Date <"2017-05-12",]
#Shows Deciles
#decile_range <- quantile(ABS_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5)

#Shows Decile Range for each observation
#ABS_mod$SP500_decile <- with(ABS_mod, cut(ABS_mod$S.P500, breaks=quantile(ABS_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

#Deciles for each column
ABS_mod$SP500_decile <- with(ABS_mod, cut(ABS_mod$S.P500, labels = 1:10, breaks=quantile(ABS_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE, ordered_result = TRUE))
ABS_mod$FedRates_decile <- with(ABS_mod, cut(ABS_mod$FedRates, labels = 1:10, breaks=quantile(ABS_mod$FedRates, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
ABS_mod$LIBOR_decile <- with(ABS_mod, cut(ABS_mod$LIBOR, labels = 1:10, breaks=quantile(ABS_mod$LIBOR, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
ABS_mod$VIX1M_decile <- with(ABS_mod, cut(ABS_mod$VIX1M.Index, labels = 1:10, breaks=quantile(ABS_mod$VIX1M.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
ABS_mod$VIX_VXV_decile <- with(ABS_mod, cut(ABS_mod$VIX.VXV, labels = 1:10, breaks=quantile(ABS_mod$VIX.VXV, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
ABS_mod$USGG2YR_decile <- with(ABS_mod, cut(ABS_mod$USGG2YR.Index, labels = 1:10, breaks=quantile(ABS_mod$USGG2YR.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
ABS_mod$USGG10YR_decile <- with(ABS_mod, cut(ABS_mod$USGG10YR.Index, labels = 1:10, breaks=quantile(ABS_mod$USGG10YR.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
ABS_mod$T10Y2Y_decile <- with(ABS_mod, cut(ABS_mod$T10Y2Y, labels = 1:10, breaks=quantile(ABS_mod$T10Y2Y, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

#Calculate Mean Excess Return for each Decile
ABS_SP500_Excess_Mean <- aggregate(list(ABS_SP500_Mean=ABS_mod$Future.12.Month.Return), list(Decile = ABS_mod$SP500_decile), mean)
ABS_FedRate_Excess_Mean <- aggregate(list(ABS_FedRate_Mean=ABS_mod$Future.12.Month.Return), list(Decile = ABS_mod$FedRates_decile), mean)
ABS_LIBOR_Excess_Mean <- aggregate(list(ABS_LIBOR_Mean=ABS_mod$Future.12.Month.Return), list(Decile = ABS_mod$LIBOR_decile), mean)
ABS_VIX1M_Excess_Mean <- aggregate(list(ABS_VIX1M_Mean=ABS_mod$Future.12.Month.Return), list(Decile = ABS_mod$VIX1M_decile), mean)
ABS_VIX_VXV_Excess_Mean <- aggregate(list(ABS_VIX_VXV_Mean=ABS_mod$Future.12.Month.Return), list(Decile = ABS_mod$VIX_VXV_decile), mean)
ABS_USGG2YR_Excess_Mean <- aggregate(list(ABS_USGG2YR_Mean=ABS_mod$Future.12.Month.Return), list(Decile = ABS_mod$USGG2YR_decile), mean)
ABS_USGG10YR_Excess_Mean <- aggregate(list(ABS_USGG10YR_Mean=ABS_mod$Future.12.Month.Return), list(Decile = ABS_mod$USGG10YR_decile), mean)
ABS_T10Y2Y_Excess_Mean <- aggregate(list(ABS_T10Y2Y_Mean=ABS_mod$Future.12.Month.Return), list(Decile = ABS_mod$T10Y2Y_decile), mean)

comb_ABS <- Reduce(function(x, y) merge(x, y, by="Decile", all=TRUE), list(ABS_SP500_Excess_Mean,ABS_FedRate_Excess_Mean, ABS_LIBOR_Excess_Mean, ABS_VIX1M_Excess_Mean, ABS_VIX_VXV_Excess_Mean, ABS_USGG2YR_Excess_Mean, ABS_USGG10YR_Excess_Mean, ABS_T10Y2Y_Excess_Mean))
comb_ABS <- comb_ABS[ order(as.numeric(comb_ABS$Decile)), ]

#qplot(ABS_SP500_Excess_Mean$ABS_SP500_Mean, geom = "histogram")
#Bar charts
ggplot(ABS_SP500_Excess_Mean, aes(x=ABS_SP500_Excess_Mean$Decile, y=ABS_SP500_Excess_Mean$ABS_SP500_Mean)) +
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(ABS_FedRate_Excess_Mean, aes(x=ABS_FedRate_Excess_Mean$Decile, y=ABS_FedRate_Excess_Mean$ABS_FedRate_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(ABS_LIBOR_Excess_Mean, aes(x=ABS_LIBOR_Excess_Mean$Decile, y=ABS_LIBOR_Excess_Mean$ABS_LIBOR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(ABS_VIX1M_Excess_Mean, aes(x=ABS_VIX1M_Excess_Mean$Decile, y=ABS_VIX1M_Excess_Mean$ABS_VIX1M_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(ABS_VIX_VXV_Excess_Mean, aes(x=ABS_VIX_VXV_Excess_Mean$Decile, y=ABS_VIX_VXV_Excess_Mean$ABS_VIX_VXV_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(ABS_USGG2YR_Excess_Mean, aes(x=ABS_USGG2YR_Excess_Mean$Decile, y=ABS_USGG2YR_Excess_Mean$ABS_USGG2YR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(ABS_USGG10YR_Excess_Mean, aes(x=ABS_USGG10YR_Excess_Mean$Decile, y=ABS_USGG10YR_Excess_Mean$ABS_USGG10YR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(ABS_T10Y2Y_Excess_Mean, aes(x=ABS_T10Y2Y_Excess_Mean$Decile, y=ABS_T10Y2Y_Excess_Mean$ABS_T10Y2Y_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')


# ---------------------------------------------- CMBS --------------------------------------------------------------------

#Read the file and impute with last known value for the response variable
CMBS <-read.csv("CMBS-monthly.csv", na.strings = "n/a")
CMBS$Future.12.Month.Return <-  na.locf(CMBS$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

#Keep these columns
keep_cols <- c("Value.Date","S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y", "Future.12.Month.Return")
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
CMBS_SP500_Excess_Mean <- aggregate(list(CMBS_SP500_Mean=CMBS_mod$Future.12.Month.Return), list(Decile = CMBS_mod$SP500_decile), mean)
CMBS_FedRate_Excess_Mean <- aggregate(list(CMBS_FedRate_Mean=CMBS_mod$Future.12.Month.Return), list(Decile = CMBS_mod$FedRates_decile), mean)
CMBS_LIBOR_Excess_Mean <- aggregate(list(CMBS_LIBOR_Mean=CMBS_mod$Future.12.Month.Return), list(Decile = CMBS_mod$LIBOR_decile), mean)
CMBS_VIX1M_Excess_Mean <- aggregate(list(CMBS_VIX1M_Mean=CMBS_mod$Future.12.Month.Return), list(Decile = CMBS_mod$VIX1M_decile), mean)
CMBS_VIX_VXV_Excess_Mean <- aggregate(list(CMBS_VIX_VXV_Mean=CMBS_mod$Future.12.Month.Return), list(Decile = CMBS_mod$VIX_VXV_decile), mean)
CMBS_USGG2YR_Excess_Mean <- aggregate(list(CMBS_USGG2YR_Mean=CMBS_mod$Future.12.Month.Return), list(Decile = CMBS_mod$USGG2YR_decile), mean)
CMBS_USGG10YR_Excess_Mean <- aggregate(list(CMBS_USGG10YR_Mean=CMBS_mod$Future.12.Month.Return), list(Decile = CMBS_mod$USGG10YR_decile), mean)
CMBS_T10Y2Y_Excess_Mean <- aggregate(list(CMBS_T10Y2Y_Mean=CMBS_mod$Future.12.Month.Return), list(Decile = CMBS_mod$T10Y2Y_decile), mean)

comb_CMBS <- Reduce(function(x, y) merge(x, y, by="Decile", all=TRUE), list(CMBS_SP500_Excess_Mean,CMBS_FedRate_Excess_Mean, CMBS_LIBOR_Excess_Mean, CMBS_VIX1M_Excess_Mean, CMBS_VIX_VXV_Excess_Mean, CMBS_USGG2YR_Excess_Mean, CMBS_USGG10YR_Excess_Mean, CMBS_T10Y2Y_Excess_Mean))
comb_CMBS <- comb_CMBS[ order(as.numeric(comb_CMBS$Decile)), ]

#qplot(CMBS_SP500_Excess_Mean$CMBS_SP500_Mean, geom = "histogram")
#Bar charts
ggplot(CMBS_SP500_Excess_Mean, aes(x=CMBS_SP500_Excess_Mean$Decile, y=CMBS_SP500_Excess_Mean$CMBS_SP500_Mean)) +
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_FedRate_Excess_Mean, aes(x=CMBS_FedRate_Excess_Mean$Decile, y=CMBS_FedRate_Excess_Mean$CMBS_FedRate_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_LIBOR_Excess_Mean, aes(x=CMBS_LIBOR_Excess_Mean$Decile, y=CMBS_LIBOR_Excess_Mean$CMBS_LIBOR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_VIX1M_Excess_Mean, aes(x=CMBS_VIX1M_Excess_Mean$Decile, y=CMBS_VIX1M_Excess_Mean$CMBS_VIX1M_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_VIX_VXV_Excess_Mean, aes(x=CMBS_VIX_VXV_Excess_Mean$Decile, y=CMBS_VIX_VXV_Excess_Mean$CMBS_VIX_VXV_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_USGG2YR_Excess_Mean, aes(x=CMBS_USGG2YR_Excess_Mean$Decile, y=CMBS_USGG2YR_Excess_Mean$CMBS_USGG2YR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_USGG10YR_Excess_Mean, aes(x=CMBS_USGG10YR_Excess_Mean$Decile, y=CMBS_USGG10YR_Excess_Mean$CMBS_USGG10YR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(CMBS_T10Y2Y_Excess_Mean, aes(x=CMBS_T10Y2Y_Excess_Mean$Decile, y=CMBS_T10Y2Y_Excess_Mean$CMBS_T10Y2Y_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')

# ---------------------------------------------- GovtRelated --------------------------------------------------------------------

#Read the file and impute with last known value for the response variable
GovtRelated <-read.csv("GovtRelated-monthly.csv", na.strings = "n/a")
GovtRelated$Future.12.Month.Return <-  na.locf(GovtRelated$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

#Keep these columns
keep_cols <- c("Value.Date","S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y", "Future.12.Month.Return")
GovtRelated_mod <- GovtRelated[ , (names(GovtRelated) %in% keep_cols)]

#Convert to R friendly date format
GovtRelated_mod$Value.Date <- as.Date(GovtRelated_mod$Value.Date, "%m/%d/%Y")

#Remove data that we don't have one year forward data
GovtRelated_mod <- GovtRelated_mod[GovtRelated_mod$Value.Date <"2017-05-12",]
#Shows Deciles
#decile_range <- quantile(GovtRelated_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5)

#Shows Decile Range for each observation
#GovtRelated_mod$SP500_decile <- with(GovtRelated_mod, cut(GovtRelated_mod$S.P500, breaks=quantile(GovtRelated_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

#Deciles for each column
GovtRelated_mod$SP500_decile <- with(GovtRelated_mod, cut(GovtRelated_mod$S.P500, labels = 1:10, breaks=quantile(GovtRelated_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE, ordered_result = TRUE))
GovtRelated_mod$FedRates_decile <- with(GovtRelated_mod, cut(GovtRelated_mod$FedRates, labels = 1:10, breaks=quantile(GovtRelated_mod$FedRates, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
GovtRelated_mod$LIBOR_decile <- with(GovtRelated_mod, cut(GovtRelated_mod$LIBOR, labels = 1:10, breaks=quantile(GovtRelated_mod$LIBOR, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
GovtRelated_mod$VIX1M_decile <- with(GovtRelated_mod, cut(GovtRelated_mod$VIX1M.Index, labels = 1:10, breaks=quantile(GovtRelated_mod$VIX1M.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
GovtRelated_mod$VIX_VXV_decile <- with(GovtRelated_mod, cut(GovtRelated_mod$VIX.VXV, labels = 1:10, breaks=quantile(GovtRelated_mod$VIX.VXV, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
GovtRelated_mod$USGG2YR_decile <- with(GovtRelated_mod, cut(GovtRelated_mod$USGG2YR.Index, labels = 1:10, breaks=quantile(GovtRelated_mod$USGG2YR.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
GovtRelated_mod$USGG10YR_decile <- with(GovtRelated_mod, cut(GovtRelated_mod$USGG10YR.Index, labels = 1:10, breaks=quantile(GovtRelated_mod$USGG10YR.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
GovtRelated_mod$T10Y2Y_decile <- with(GovtRelated_mod, cut(GovtRelated_mod$T10Y2Y, labels = 1:10, breaks=quantile(GovtRelated_mod$T10Y2Y, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

#Calculate Mean Excess Return for each Decile
GovtRelated_SP500_Excess_Mean <- aggregate(list(GovtRelated_SP500_Mean=GovtRelated_mod$Future.12.Month.Return), list(Decile = GovtRelated_mod$SP500_decile), mean)
GovtRelated_FedRate_Excess_Mean <- aggregate(list(GovtRelated_FedRate_Mean=GovtRelated_mod$Future.12.Month.Return), list(Decile = GovtRelated_mod$FedRates_decile), mean)
GovtRelated_LIBOR_Excess_Mean <- aggregate(list(GovtRelated_LIBOR_Mean=GovtRelated_mod$Future.12.Month.Return), list(Decile = GovtRelated_mod$LIBOR_decile), mean)
GovtRelated_VIX1M_Excess_Mean <- aggregate(list(GovtRelated_VIX1M_Mean=GovtRelated_mod$Future.12.Month.Return), list(Decile = GovtRelated_mod$VIX1M_decile), mean)
GovtRelated_VIX_VXV_Excess_Mean <- aggregate(list(GovtRelated_VIX_VXV_Mean=GovtRelated_mod$Future.12.Month.Return), list(Decile = GovtRelated_mod$VIX_VXV_decile), mean)
GovtRelated_USGG2YR_Excess_Mean <- aggregate(list(GovtRelated_USGG2YR_Mean=GovtRelated_mod$Future.12.Month.Return), list(Decile = GovtRelated_mod$USGG2YR_decile), mean)
GovtRelated_USGG10YR_Excess_Mean <- aggregate(list(GovtRelated_USGG10YR_Mean=GovtRelated_mod$Future.12.Month.Return), list(Decile = GovtRelated_mod$USGG10YR_decile), mean)
GovtRelated_T10Y2Y_Excess_Mean <- aggregate(list(GovtRelated_T10Y2Y_Mean=GovtRelated_mod$Future.12.Month.Return), list(Decile = GovtRelated_mod$T10Y2Y_decile), mean)

comb_GovtRelated <- Reduce(function(x, y) merge(x, y, by="Decile", all=TRUE), list(GovtRelated_SP500_Excess_Mean,GovtRelated_FedRate_Excess_Mean, GovtRelated_LIBOR_Excess_Mean, GovtRelated_VIX1M_Excess_Mean, GovtRelated_VIX_VXV_Excess_Mean, GovtRelated_USGG2YR_Excess_Mean, GovtRelated_USGG10YR_Excess_Mean, GovtRelated_T10Y2Y_Excess_Mean))
comb_GovtRelated <- comb_GovtRelated[ order(as.numeric(comb_GovtRelated$Decile)), ]

#qplot(GovtRelated_SP500_Excess_Mean$GovtRelated_SP500_Mean, geom = "histogram")
#Bar charts
ggplot(GovtRelated_SP500_Excess_Mean, aes(x=GovtRelated_SP500_Excess_Mean$Decile, y=GovtRelated_SP500_Excess_Mean$GovtRelated_SP500_Mean)) +
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(GovtRelated_FedRate_Excess_Mean, aes(x=GovtRelated_FedRate_Excess_Mean$Decile, y=GovtRelated_FedRate_Excess_Mean$GovtRelated_FedRate_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(GovtRelated_LIBOR_Excess_Mean, aes(x=GovtRelated_LIBOR_Excess_Mean$Decile, y=GovtRelated_LIBOR_Excess_Mean$GovtRelated_LIBOR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(GovtRelated_VIX1M_Excess_Mean, aes(x=GovtRelated_VIX1M_Excess_Mean$Decile, y=GovtRelated_VIX1M_Excess_Mean$GovtRelated_VIX1M_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(GovtRelated_VIX_VXV_Excess_Mean, aes(x=GovtRelated_VIX_VXV_Excess_Mean$Decile, y=GovtRelated_VIX_VXV_Excess_Mean$GovtRelated_VIX_VXV_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(GovtRelated_USGG2YR_Excess_Mean, aes(x=GovtRelated_USGG2YR_Excess_Mean$Decile, y=GovtRelated_USGG2YR_Excess_Mean$GovtRelated_USGG2YR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(GovtRelated_USGG10YR_Excess_Mean, aes(x=GovtRelated_USGG10YR_Excess_Mean$Decile, y=GovtRelated_USGG10YR_Excess_Mean$GovtRelated_USGG10YR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(GovtRelated_T10Y2Y_Excess_Mean, aes(x=GovtRelated_T10Y2Y_Excess_Mean$Decile, y=GovtRelated_T10Y2Y_Excess_Mean$GovtRelated_T10Y2Y_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')

# ---------------------------------------------- IGCorp --------------------------------------------------------------------

#Read the file and impute with last known value for the response variable
IGCorp <-read.csv("IGCorp-monthly.csv", na.strings = "n/a")
IGCorp$Future.12.Month.Return <-  na.locf(IGCorp$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

#Keep these columns
keep_cols <- c("Value.Date","S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y", "Future.12.Month.Return")
IGCorp_mod <- IGCorp[ , (names(IGCorp) %in% keep_cols)]

#Convert to R friendly date format
IGCorp_mod$Value.Date <- as.Date(IGCorp_mod$Value.Date, "%m/%d/%Y")

#Remove data that we don't have one year forward data
IGCorp_mod <- IGCorp_mod[IGCorp_mod$Value.Date <"2017-05-12",]
#Shows Deciles
#decile_range <- quantile(IGCorp_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5)

#Shows Decile Range for each observation
#IGCorp_mod$SP500_decile <- with(IGCorp_mod, cut(IGCorp_mod$S.P500, breaks=quantile(IGCorp_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

#Deciles for each column
IGCorp_mod$SP500_decile <- with(IGCorp_mod, cut(IGCorp_mod$S.P500, labels = 1:10, breaks=quantile(IGCorp_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE, ordered_result = TRUE))
IGCorp_mod$FedRates_decile <- with(IGCorp_mod, cut(IGCorp_mod$FedRates, labels = 1:10, breaks=quantile(IGCorp_mod$FedRates, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
IGCorp_mod$LIBOR_decile <- with(IGCorp_mod, cut(IGCorp_mod$LIBOR, labels = 1:10, breaks=quantile(IGCorp_mod$LIBOR, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
IGCorp_mod$VIX1M_decile <- with(IGCorp_mod, cut(IGCorp_mod$VIX1M.Index, labels = 1:10, breaks=quantile(IGCorp_mod$VIX1M.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
IGCorp_mod$VIX_VXV_decile <- with(IGCorp_mod, cut(IGCorp_mod$VIX.VXV, labels = 1:10, breaks=quantile(IGCorp_mod$VIX.VXV, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
IGCorp_mod$USGG2YR_decile <- with(IGCorp_mod, cut(IGCorp_mod$USGG2YR.Index, labels = 1:10, breaks=quantile(IGCorp_mod$USGG2YR.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
IGCorp_mod$USGG10YR_decile <- with(IGCorp_mod, cut(IGCorp_mod$USGG10YR.Index, labels = 1:10, breaks=quantile(IGCorp_mod$USGG10YR.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
IGCorp_mod$T10Y2Y_decile <- with(IGCorp_mod, cut(IGCorp_mod$T10Y2Y, labels = 1:10, breaks=quantile(IGCorp_mod$T10Y2Y, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

#Calculate Mean Excess Return for each Decile
IGCorp_SP500_Excess_Mean <- aggregate(list(IGCorp_SP500_Mean=IGCorp_mod$Future.12.Month.Return), list(Decile = IGCorp_mod$SP500_decile), mean)
IGCorp_FedRate_Excess_Mean <- aggregate(list(IGCorp_FedRate_Mean=IGCorp_mod$Future.12.Month.Return), list(Decile = IGCorp_mod$FedRates_decile), mean)
IGCorp_LIBOR_Excess_Mean <- aggregate(list(IGCorp_LIBOR_Mean=IGCorp_mod$Future.12.Month.Return), list(Decile = IGCorp_mod$LIBOR_decile), mean)
IGCorp_VIX1M_Excess_Mean <- aggregate(list(IGCorp_VIX1M_Mean=IGCorp_mod$Future.12.Month.Return), list(Decile = IGCorp_mod$VIX1M_decile), mean)
IGCorp_VIX_VXV_Excess_Mean <- aggregate(list(IGCorp_VIX_VXV_Mean=IGCorp_mod$Future.12.Month.Return), list(Decile = IGCorp_mod$VIX_VXV_decile), mean)
IGCorp_USGG2YR_Excess_Mean <- aggregate(list(IGCorp_USGG2YR_Mean=IGCorp_mod$Future.12.Month.Return), list(Decile = IGCorp_mod$USGG2YR_decile), mean)
IGCorp_USGG10YR_Excess_Mean <- aggregate(list(IGCorp_USGG10YR_Mean=IGCorp_mod$Future.12.Month.Return), list(Decile = IGCorp_mod$USGG10YR_decile), mean)
IGCorp_T10Y2Y_Excess_Mean <- aggregate(list(IGCorp_T10Y2Y_Mean=IGCorp_mod$Future.12.Month.Return), list(Decile = IGCorp_mod$T10Y2Y_decile), mean)

comb_IGCorp <- Reduce(function(x, y) merge(x, y, by="Decile", all=TRUE), list(IGCorp_SP500_Excess_Mean,IGCorp_FedRate_Excess_Mean, IGCorp_LIBOR_Excess_Mean, IGCorp_VIX1M_Excess_Mean, IGCorp_VIX_VXV_Excess_Mean, IGCorp_USGG2YR_Excess_Mean, IGCorp_USGG10YR_Excess_Mean, IGCorp_T10Y2Y_Excess_Mean))
comb_IGCorp <- comb_IGCorp[ order(as.numeric(comb_IGCorp$Decile)), ]

#qplot(IGCorp_SP500_Excess_Mean$IGCorp_SP500_Mean, geom = "histogram")
#Bar charts
ggplot(IGCorp_SP500_Excess_Mean, aes(x=IGCorp_SP500_Excess_Mean$Decile, y=IGCorp_SP500_Excess_Mean$IGCorp_SP500_Mean)) +
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(IGCorp_FedRate_Excess_Mean, aes(x=IGCorp_FedRate_Excess_Mean$Decile, y=IGCorp_FedRate_Excess_Mean$IGCorp_FedRate_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(IGCorp_LIBOR_Excess_Mean, aes(x=IGCorp_LIBOR_Excess_Mean$Decile, y=IGCorp_LIBOR_Excess_Mean$IGCorp_LIBOR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(IGCorp_VIX1M_Excess_Mean, aes(x=IGCorp_VIX1M_Excess_Mean$Decile, y=IGCorp_VIX1M_Excess_Mean$IGCorp_VIX1M_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(IGCorp_VIX_VXV_Excess_Mean, aes(x=IGCorp_VIX_VXV_Excess_Mean$Decile, y=IGCorp_VIX_VXV_Excess_Mean$IGCorp_VIX_VXV_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(IGCorp_USGG2YR_Excess_Mean, aes(x=IGCorp_USGG2YR_Excess_Mean$Decile, y=IGCorp_USGG2YR_Excess_Mean$IGCorp_USGG2YR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(IGCorp_USGG10YR_Excess_Mean, aes(x=IGCorp_USGG10YR_Excess_Mean$Decile, y=IGCorp_USGG10YR_Excess_Mean$IGCorp_USGG10YR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(IGCorp_T10Y2Y_Excess_Mean, aes(x=IGCorp_T10Y2Y_Excess_Mean$Decile, y=IGCorp_T10Y2Y_Excess_Mean$IGCorp_T10Y2Y_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')

# ---------------------------------------------- MBS --------------------------------------------------------------------

#Read the file and impute with last known value for the response variable
MBS <-read.csv("MBS-monthly.csv", na.strings = "n/a")
MBS$Future.12.Month.Return <-  na.locf(MBS$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

#Keep these columns
keep_cols <- c("Value.Date","S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y", "Future.12.Month.Return")
MBS_mod <- MBS[ , (names(MBS) %in% keep_cols)]

#Convert to R friendly date format
MBS_mod$Value.Date <- as.Date(MBS_mod$Value.Date, "%m/%d/%Y")

#Remove data that we don't have one year forward data
MBS_mod <- MBS_mod[MBS_mod$Value.Date <"2017-05-12",]
#Shows Deciles
#decile_range <- quantile(MBS_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5)

#Shows Decile Range for each observation
#MBS_mod$SP500_decile <- with(MBS_mod, cut(MBS_mod$S.P500, breaks=quantile(MBS_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

#Deciles for each column
MBS_mod$SP500_decile <- with(MBS_mod, cut(MBS_mod$S.P500, labels = 1:10, breaks=quantile(MBS_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE, ordered_result = TRUE))
MBS_mod$FedRates_decile <- with(MBS_mod, cut(MBS_mod$FedRates, labels = 1:10, breaks=quantile(MBS_mod$FedRates, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
MBS_mod$LIBOR_decile <- with(MBS_mod, cut(MBS_mod$LIBOR, labels = 1:10, breaks=quantile(MBS_mod$LIBOR, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
MBS_mod$VIX1M_decile <- with(MBS_mod, cut(MBS_mod$VIX1M.Index, labels = 1:10, breaks=quantile(MBS_mod$VIX1M.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
MBS_mod$VIX_VXV_decile <- with(MBS_mod, cut(MBS_mod$VIX.VXV, labels = 1:10, breaks=quantile(MBS_mod$VIX.VXV, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
MBS_mod$USGG2YR_decile <- with(MBS_mod, cut(MBS_mod$USGG2YR.Index, labels = 1:10, breaks=quantile(MBS_mod$USGG2YR.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
MBS_mod$USGG10YR_decile <- with(MBS_mod, cut(MBS_mod$USGG10YR.Index, labels = 1:10, breaks=quantile(MBS_mod$USGG10YR.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
MBS_mod$T10Y2Y_decile <- with(MBS_mod, cut(MBS_mod$T10Y2Y, labels = 1:10, breaks=quantile(MBS_mod$T10Y2Y, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

#Calculate Mean Excess Return for each Decile
MBS_SP500_Excess_Mean <- aggregate(list(MBS_SP500_Mean=MBS_mod$Future.12.Month.Return), list(Decile = MBS_mod$SP500_decile), mean)
MBS_FedRate_Excess_Mean <- aggregate(list(MBS_FedRate_Mean=MBS_mod$Future.12.Month.Return), list(Decile = MBS_mod$FedRates_decile), mean)
MBS_LIBOR_Excess_Mean <- aggregate(list(MBS_LIBOR_Mean=MBS_mod$Future.12.Month.Return), list(Decile = MBS_mod$LIBOR_decile), mean)
MBS_VIX1M_Excess_Mean <- aggregate(list(MBS_VIX1M_Mean=MBS_mod$Future.12.Month.Return), list(Decile = MBS_mod$VIX1M_decile), mean)
MBS_VIX_VXV_Excess_Mean <- aggregate(list(MBS_VIX_VXV_Mean=MBS_mod$Future.12.Month.Return), list(Decile = MBS_mod$VIX_VXV_decile), mean)
MBS_USGG2YR_Excess_Mean <- aggregate(list(MBS_USGG2YR_Mean=MBS_mod$Future.12.Month.Return), list(Decile = MBS_mod$USGG2YR_decile), mean)
MBS_USGG10YR_Excess_Mean <- aggregate(list(MBS_USGG10YR_Mean=MBS_mod$Future.12.Month.Return), list(Decile = MBS_mod$USGG10YR_decile), mean)
MBS_T10Y2Y_Excess_Mean <- aggregate(list(MBS_T10Y2Y_Mean=MBS_mod$Future.12.Month.Return), list(Decile = MBS_mod$T10Y2Y_decile), mean)

comb_MBS <- Reduce(function(x, y) merge(x, y, by="Decile", all=TRUE), list(MBS_SP500_Excess_Mean,MBS_FedRate_Excess_Mean, MBS_LIBOR_Excess_Mean, MBS_VIX1M_Excess_Mean, MBS_VIX_VXV_Excess_Mean, MBS_USGG2YR_Excess_Mean, MBS_USGG10YR_Excess_Mean, MBS_T10Y2Y_Excess_Mean))
comb_MBS <- comb_MBS[ order(as.numeric(comb_MBS$Decile)), ]

#qplot(MBS_SP500_Excess_Mean$MBS_SP500_Mean, geom = "histogram")
#Bar charts
ggplot(MBS_SP500_Excess_Mean, aes(x=MBS_SP500_Excess_Mean$Decile, y=MBS_SP500_Excess_Mean$MBS_SP500_Mean)) +
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(MBS_FedRate_Excess_Mean, aes(x=MBS_FedRate_Excess_Mean$Decile, y=MBS_FedRate_Excess_Mean$MBS_FedRate_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(MBS_LIBOR_Excess_Mean, aes(x=MBS_LIBOR_Excess_Mean$Decile, y=MBS_LIBOR_Excess_Mean$MBS_LIBOR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(MBS_VIX1M_Excess_Mean, aes(x=MBS_VIX1M_Excess_Mean$Decile, y=MBS_VIX1M_Excess_Mean$MBS_VIX1M_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(MBS_VIX_VXV_Excess_Mean, aes(x=MBS_VIX_VXV_Excess_Mean$Decile, y=MBS_VIX_VXV_Excess_Mean$MBS_VIX_VXV_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(MBS_USGG2YR_Excess_Mean, aes(x=MBS_USGG2YR_Excess_Mean$Decile, y=MBS_USGG2YR_Excess_Mean$MBS_USGG2YR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(MBS_USGG10YR_Excess_Mean, aes(x=MBS_USGG10YR_Excess_Mean$Decile, y=MBS_USGG10YR_Excess_Mean$MBS_USGG10YR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(MBS_T10Y2Y_Excess_Mean, aes(x=MBS_T10Y2Y_Excess_Mean$Decile, y=MBS_T10Y2Y_Excess_Mean$MBS_T10Y2Y_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')

# ---------------------------------------------- USHighYield --------------------------------------------------------------------

#Read the file and impute with last known value for the response variable
USHighYield <-read.csv("USHighYield-monthly.csv", na.strings = "n/a")
USHighYield$Future.12.Month.Return <-  na.locf(USHighYield$Future.12.Month.Return, maxgap = Inf, na.rm = FALSE,fromLast = FALSE)

#Keep these columns
keep_cols <- c("Value.Date","S.P500","FedRates","LIBOR", "VIX1M.Index","VIX.VXV","USGG2YR.Index","USGG10YR.Index", "T10Y2Y", "Future.12.Month.Return")
USHighYield_mod <- USHighYield[ , (names(USHighYield) %in% keep_cols)]

#Convert to R friendly date format
USHighYield_mod$Value.Date <- as.Date(USHighYield_mod$Value.Date, "%m/%d/%Y")

#Remove data that we don't have one year forward data
USHighYield_mod <- USHighYield_mod[USHighYield_mod$Value.Date <"2017-05-12",]
#Shows Deciles
#decile_range <- quantile(USHighYield_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5)

#Shows Decile Range for each observation
#USHighYield_mod$SP500_decile <- with(USHighYield_mod, cut(USHighYield_mod$S.P500, breaks=quantile(USHighYield_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

#Deciles for each column
USHighYield_mod$SP500_decile <- with(USHighYield_mod, cut(USHighYield_mod$S.P500, labels = 1:10, breaks=quantile(USHighYield_mod$S.P500, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE, ordered_result = TRUE))
USHighYield_mod$FedRates_decile <- with(USHighYield_mod, cut(USHighYield_mod$FedRates, labels = 1:10, breaks=quantile(USHighYield_mod$FedRates, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
USHighYield_mod$LIBOR_decile <- with(USHighYield_mod, cut(USHighYield_mod$LIBOR, labels = 1:10, breaks=quantile(USHighYield_mod$LIBOR, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
USHighYield_mod$VIX1M_decile <- with(USHighYield_mod, cut(USHighYield_mod$VIX1M.Index, labels = 1:10, breaks=quantile(USHighYield_mod$VIX1M.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
USHighYield_mod$VIX_VXV_decile <- with(USHighYield_mod, cut(USHighYield_mod$VIX.VXV, labels = 1:10, breaks=quantile(USHighYield_mod$VIX.VXV, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
USHighYield_mod$USGG2YR_decile <- with(USHighYield_mod, cut(USHighYield_mod$USGG2YR.Index, labels = 1:10, breaks=quantile(USHighYield_mod$USGG2YR.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
USHighYield_mod$USGG10YR_decile <- with(USHighYield_mod, cut(USHighYield_mod$USGG10YR.Index, labels = 1:10, breaks=quantile(USHighYield_mod$USGG10YR.Index, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))
USHighYield_mod$T10Y2Y_decile <- with(USHighYield_mod, cut(USHighYield_mod$T10Y2Y, labels = 1:10, breaks=quantile(USHighYield_mod$T10Y2Y, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5), include.lowest=TRUE))

#Calculate Mean Excess Return for each Decile
USHighYield_SP500_Excess_Mean <- aggregate(list(USHighYield_SP500_Mean=USHighYield_mod$Future.12.Month.Return), list(Decile = USHighYield_mod$SP500_decile), mean)
USHighYield_FedRate_Excess_Mean <- aggregate(list(USHighYield_FedRate_Mean=USHighYield_mod$Future.12.Month.Return), list(Decile = USHighYield_mod$FedRates_decile), mean)
USHighYield_LIBOR_Excess_Mean <- aggregate(list(USHighYield_LIBOR_Mean=USHighYield_mod$Future.12.Month.Return), list(Decile = USHighYield_mod$LIBOR_decile), mean)
USHighYield_VIX1M_Excess_Mean <- aggregate(list(USHighYield_VIX1M_Mean=USHighYield_mod$Future.12.Month.Return), list(Decile = USHighYield_mod$VIX1M_decile), mean)
USHighYield_VIX_VXV_Excess_Mean <- aggregate(list(USHighYield_VIX_VXV_Mean=USHighYield_mod$Future.12.Month.Return), list(Decile = USHighYield_mod$VIX_VXV_decile), mean)
USHighYield_USGG2YR_Excess_Mean <- aggregate(list(USHighYield_USGG2YR_Mean=USHighYield_mod$Future.12.Month.Return), list(Decile = USHighYield_mod$USGG2YR_decile), mean)
USHighYield_USGG10YR_Excess_Mean <- aggregate(list(USHighYield_USGG10YR_Mean=USHighYield_mod$Future.12.Month.Return), list(Decile = USHighYield_mod$USGG10YR_decile), mean)
USHighYield_T10Y2Y_Excess_Mean <- aggregate(list(USHighYield_T10Y2Y_Mean=USHighYield_mod$Future.12.Month.Return), list(Decile = USHighYield_mod$T10Y2Y_decile), mean)

comb_USHighYield <- Reduce(function(x, y) merge(x, y, by="Decile", all=TRUE), list(USHighYield_SP500_Excess_Mean,USHighYield_FedRate_Excess_Mean, USHighYield_LIBOR_Excess_Mean, USHighYield_VIX1M_Excess_Mean, USHighYield_VIX_VXV_Excess_Mean, USHighYield_USGG2YR_Excess_Mean, USHighYield_USGG10YR_Excess_Mean, USHighYield_T10Y2Y_Excess_Mean))
comb_USHighYield <- comb_USHighYield[ order(as.numeric(comb_USHighYield$Decile)), ]

#qplot(USHighYield_SP500_Excess_Mean$USHighYield_SP500_Mean, geom = "histogram")
#Bar charts
ggplot(USHighYield_SP500_Excess_Mean, aes(x=USHighYield_SP500_Excess_Mean$Decile, y=USHighYield_SP500_Excess_Mean$USHighYield_SP500_Mean)) +
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(USHighYield_FedRate_Excess_Mean, aes(x=USHighYield_FedRate_Excess_Mean$Decile, y=USHighYield_FedRate_Excess_Mean$USHighYield_FedRate_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(USHighYield_LIBOR_Excess_Mean, aes(x=USHighYield_LIBOR_Excess_Mean$Decile, y=USHighYield_LIBOR_Excess_Mean$USHighYield_LIBOR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(USHighYield_VIX1M_Excess_Mean, aes(x=USHighYield_VIX1M_Excess_Mean$Decile, y=USHighYield_VIX1M_Excess_Mean$USHighYield_VIX1M_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(USHighYield_VIX_VXV_Excess_Mean, aes(x=USHighYield_VIX_VXV_Excess_Mean$Decile, y=USHighYield_VIX_VXV_Excess_Mean$USHighYield_VIX_VXV_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(USHighYield_USGG2YR_Excess_Mean, aes(x=USHighYield_USGG2YR_Excess_Mean$Decile, y=USHighYield_USGG2YR_Excess_Mean$USHighYield_USGG2YR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(USHighYield_USGG10YR_Excess_Mean, aes(x=USHighYield_USGG10YR_Excess_Mean$Decile, y=USHighYield_USGG10YR_Excess_Mean$USHighYield_USGG10YR_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')
ggplot(USHighYield_T10Y2Y_Excess_Mean, aes(x=USHighYield_T10Y2Y_Excess_Mean$Decile, y=USHighYield_T10Y2Y_Excess_Mean$USHighYield_T10Y2Y_Mean)) + 
  geom_bar(fill = "#0073C2FF", stat = 'identity')