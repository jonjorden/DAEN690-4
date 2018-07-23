# -------------------------------------------------------------------------------------- 
# Random Forests for ABS
# Variable Importance Plots
# Splom (scatterplot matrix) of the most important variables
# -------------------------------------------------------------------------------------- 

# Install and Load Libraries

#install.packages("randomForeset", dependencies = True)
#install.packages("lattice", dependencies = True)
#install.packages("hexbin", dependencies = True)
#install.packages("ggplot2", dependencies = True)
#install.packages("missForest", dependencies = True)

library(MASS) # Boston Housing Data
library(randomForest)
library(lattice)
library(hexbin)
library(ggplot2)

# Read in the data
sector_data <-read.csv('Combined_Dataset_v3.csv')

# Check for missing values
sum(is.na(sector_data))

# Look at all column names to keep based on dtype
colnames(sector_data)

# ------------------------------------------- Random Forest -----------------------------------------------------------
# Random forests assesses a variable's importance by randomizing the positions a variable's values in the vector. 
# How much does the model's MSE increase?


# -------------------------------------------- ABS Sector ------------------------------------------------------------- 

# Keep these columns (omit the factor data types)
keep_cols_ABS <- c("ABS_12M", "S.P500","FedRates","LIBOR","USGG2YR.Index","USGG10YR.Index", 
               "HQMCB10YR", "PAYEMS", "IRSTCI01USM156M", "EXCAUS", "TCU", "EXCHUS",
               "IRSTCI01USM156N", "EXCHUS", "CCI", "CPALTT01USM661S", "UMCSENT", "MCOILBRENTEU", 
               "USEPUINDXM","MTSDS133FMS", "POILWTIUSDM", "MSACSR", "INDPRO", "EXJPUS", "EXKOUS",
               "USSLIND", "M1SL", "M2SL", "MABMM301USM189S", "MNFCTRSMNSA", "MVGFD027MNFRBDAL",
               "MVPHGFD027MNFRBDAL", "MSPNHSUS", "EXMXUS", "MSACSR.1", "BAA", "HSN1F", 
               "PCE", "PSAVERT", "PCU2122212122210", "DSPIC96", "PCEC96", "CSUSHPINSA", 
               "RECPROUSM156N", "TLAACBM027SBOG", "TTLCONS", "TOTALSL", "TOTALSA", "TWEXMMTH", "PCETRIM12M159SFRBDAL", 
               "USD3MTD156N", "EXUSEU", "EXUSUK")
sector_ABS <- sector_data[ , (names(sector_data) %in% keep_cols_ABS)]
head(sector_ABS,5)

# View the ABS data in table format
View(sector_ABS)

# Create a random forest model to identify the most important variables
rf_ABS <- randomForest(sector_ABS$ABS_12M ~ ., data=sector_ABS, importance=TRUE, proximity=TRUE)

# View RF model summary
rf_ABS       
        
        # Type of random forest: regression
        # Number of trees: 500
        # No. of variables tried at each split: 16
        
        # Mean of squared residuals: 0.0001788385
        # % Var explained: 93.16
        
# Show variable importance plot
varImpPlot(rf_ABS)

# Show a list of all variables with index numbers
varNum <- function(x){
  num <- 1:ncol(x)
  names(num) <- names(x)
  return(num)}

varNum(sector_ABS)

# Remove the last important variables to leave the top 10 + response variable
keep_top10 <- c("ABS_12M", "EXUSUK", "USSLIND", "TOTALSA", "INDPRO", "TCU",
                "RECPROUSM156N", "POILWTIUSDM", "CCI", "MSACSR", "EXUSEU")
              # Index: 1,51,24,46,21,11,42,19,13,33,50 
ABS_top10 <- sector_data[ , (names(sector_data) %in% keep_top10_ABS]
names(ABS_top10)

# Plot a splom with the top five most important variables
offDiag <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.hexbinplot(x,y,xbins=12,...,border=gray(.7),
                   trans=function(x)x^1)
  #   panel.loess(x , y, ..., lwd=2,col='purple')
}
onDiag <- 
  function(x, ...){
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm=TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
    panel.lines(d,col=rgb(.83,.66,1),lwd=2)
    diag.panel.splom(x, ...)
}
windows(width=9,height=9) 
splom(ABS_top10[,c(1,2,3,4,5)],
  xlab='',main="ABS: Selected Variables",
  pscale=0, varname.cex=0.8,axis.text.cex=0.6,
  axis.text.col="purple",axis.text.font=2,
  axis.line.tck=.5,
  panel=offDiag,
  diag.panel = onDiag
)

# -------------------------------------------- CMBS Sector ------------------------------------------------------------- 

# Keep these columns (omit the factor data types)
keep_cols_CMBS <- c("CMBS_12M", "S.P500","FedRates","LIBOR","USGG2YR.Index","USGG10YR.Index", 
               "HQMCB10YR", "PAYEMS", "IRSTCI01USM156M", "EXCAUS", "TCU", "EXCHUS",
               "IRSTCI01USM156N", "EXCHUS", "CCI", "CPALTT01USM661S", "UMCSENT", "MCOILBRENTEU", 
               "USEPUINDXM","MTSDS133FMS", "POILWTIUSDM", "MSACSR", "INDPRO", "EXJPUS", "EXKOUS",
               "USSLIND", "M1SL", "M2SL", "MABMM301USM189S", "MNFCTRSMNSA", "MVGFD027MNFRBDAL",
               "MVPHGFD027MNFRBDAL", "MSPNHSUS", "EXMXUS", "MSACSR.1", "BAA", "HSN1F", 
               "PCE", "PSAVERT", "PCU2122212122210", "DSPIC96", "PCEC96", "CSUSHPINSA", 
               "RECPROUSM156N", "TLAACBM027SBOG", "TTLCONS", "TOTALSL", "TOTALSA", "TWEXMMTH", "PCETRIM12M159SFRBDAL", 
               "USD3MTD156N", "EXUSEU", "EXUSUK")
sector_CMBS <- sector_data[ , (names(sector_data) %in% keep_cols_CMBS)]
head(sector_CMBS,5)

# View the ABS data in table format
# View(sector_CMBS)

# Create a random forest model to identify the most important variables
rf_CMBS <- randomForest(sector_CMBS$CMBS_12M ~ ., data=sector_CMBS, importance=TRUE, proximity=TRUE)

# View RF model summary
rf_CMBS       

# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 16

# Mean of squared residuals: 0.0009516441
# % Var explained: 89.6

# Show variable importance plot
varImpPlot(rf_CMBS)

# Show a list of all variables with index numbers
varNum(sector_CMBS)

# Remove the last important variables to leave the top 10 + response variable
keep_top10_CMBS <- c("EXUSUK", "TCU", "TOTALSA", "EXUSEU", "EXKOUS", 
                      "MSACSR", "MSACSR.1", "CCI", "USSLIND", "INDPRO")

CMBS_top10 <- sector_data[ , (names(sector_data) %in% keep_top10_CMBS)]
names(CMBS_top10)

# Plot a splom with the top five most important variables
offDiag <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.hexbinplot(x,y,xbins=12,...,border=gray(.7),
                   trans=function(x)x^1)
  #   panel.loess(x , y, ..., lwd=2,col='purple')
}
onDiag <- 
  function(x, ...){
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm=TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
    panel.lines(d,col=rgb(.83,.66,1),lwd=2)
    diag.panel.splom(x, ...)
  }
windows(width=9,height=9) 
splom(CMBS_top10[,c(1,2,3,4,5)],
      xlab='',main="CMBS: Selected Variables",
      pscale=0, varname.cex=0.8,axis.text.cex=0.6,
      axis.text.col="purple",axis.text.font=2,
      axis.line.tck=.5,
      panel=offDiag,
      diag.panel = onDiag
)

# -------------------------------------------- GovtRelated Sector ------------------------------------------------------------- 

# Keep these columns (omit the factor data types)
keep_cols_GovtRelated <- c("GovtRelated_12M", "S.P500","FedRates","LIBOR","USGG2YR.Index","USGG10YR.Index", 
                           "HQMCB10YR", "PAYEMS", "IRSTCI01USM156M", "EXCAUS", "TCU", "EXCHUS",
                           "IRSTCI01USM156N", "EXCHUS", "CCI", "CPALTT01USM661S", "UMCSENT", "MCOILBRENTEU", 
                           "USEPUINDXM","MTSDS133FMS", "POILWTIUSDM", "MSACSR", "INDPRO", "EXJPUS", "EXKOUS",
                           "USSLIND", "M1SL", "M2SL", "MABMM301USM189S", "MNFCTRSMNSA", "MVGFD027MNFRBDAL",
                           "MVPHGFD027MNFRBDAL", "MSPNHSUS", "EXMXUS", "MSACSR.1", "BAA", "HSN1F", 
                           "PCE", "PSAVERT", "PCU2122212122210", "DSPIC96", "PCEC96", "CSUSHPINSA", 
                           "RECPROUSM156N", "TLAACBM027SBOG", "TTLCONS", "TOTALSL", "TOTALSA", "TWEXMMTH", "PCETRIM12M159SFRBDAL", 
                           "USD3MTD156N", "EXUSEU", "EXUSUK")
sector_GovtRelated <- sector_data[ , (names(sector_data) %in% keep_cols_GovtRelated)]
head(sector_GovtRelated,5)

# View the ABS data in table format
# View(sector_GovtRelated)

# Create a random forest model to identify the most important variables
rf_GovtRelated <- randomForest(sector_GovtRelated$GovtRelated_12M ~ ., data=sector_GovtRelated, importance=TRUE, proximity=TRUE)

# View RF model summary
rf_GovtRelated       

# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 16

# Mean of squared residuals: 2.419992e-05
# % Var explained: 86.06

# Show variable importance plot
varImpPlot(rf_GovtRelated)

# Show a list of all variables with index numbers
varNum(sector_GovtRelated)

# Remove the last important variables to leave the top 10 + response variable
keep_top10_GovtRelated <- c("INDPRO", "EXUSUK", "EXKOUS", "EXMXUS",
                            "TTLCONS", "USSLIND", "TCU", "PAYEMS", "HQMCB10YR", "PCETRIM12M159SFRBDAL")

GovtRelated_top10 <- sector_data[ , (names(sector_data) %in% keep_top10_GovtRelated)]
names(GovtRelated_top10)

# Plot a splom with the top five most important variables
offDiag <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.hexbinplot(x,y,xbins=12,...,border=gray(.7),
                   trans=function(x)x^1)
  #   panel.loess(x , y, ..., lwd=2,col='purple')
}
onDiag <- 
  function(x, ...){
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm=TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
    panel.lines(d,col=rgb(.83,.66,1),lwd=2)
    diag.panel.splom(x, ...)
  }
windows(width=9,height=9) 
splom(GovtRelated_top10[,c(1,2,3,4,5)],
      xlab='',main="GovtRelated: Selected Variables",
      pscale=0, varname.cex=0.8,axis.text.cex=0.6,
      axis.text.col="purple",axis.text.font=2,
      axis.line.tck=.5,
      panel=offDiag,
      diag.panel = onDiag
)

# -------------------------------------------- IGCorp Sector ------------------------------------------------------------- 

# Keep these columns (omit the factor data types)
keep_cols_IGCorp <- c("IGCorp_12M", "S.P500","FedRates","LIBOR","USGG2YR.Index","USGG10YR.Index", 
                      "HQMCB10YR", "PAYEMS", "IRSTCI01USM156M", "EXCAUS", "TCU", "EXCHUS",
                      "IRSTCI01USM156N", "EXCHUS", "CCI", "CPALTT01USM661S", "UMCSENT", "MCOILBRENTEU", 
                      "USEPUINDXM","MTSDS133FMS", "POILWTIUSDM", "MSACSR", "INDPRO", "EXJPUS", "EXKOUS",
                      "USSLIND", "M1SL", "M2SL", "MABMM301USM189S", "MNFCTRSMNSA", "MVGFD027MNFRBDAL",
                      "MVPHGFD027MNFRBDAL", "MSPNHSUS", "EXMXUS", "MSACSR.1", "BAA", "HSN1F", 
                      "PCE", "PSAVERT", "PCU2122212122210", "DSPIC96", "PCEC96", "CSUSHPINSA", 
                      "RECPROUSM156N", "TLAACBM027SBOG", "TTLCONS", "TOTALSL", "TOTALSA", "TWEXMMTH", "PCETRIM12M159SFRBDAL", 
                      "USD3MTD156N", "EXUSEU", "EXUSUK")
sector_IGCorp <- sector_data[ , (names(sector_data) %in% keep_cols_IGCorp)]
head(sector_IGCorp,5)

# View the ABS data in table format
# View(sector_IGCorp)

# Create a random forest model to identify the most important variables
rf_IGCorp <- randomForest(sector_IGCorp$IGCorp_12M ~ ., data=sector_IGCorp, importance=TRUE, proximity=TRUE)

# View RF model summary
rf_IGCorp       

# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 16

# Mean of squared residuals: 0.0003063885
# % Var explained: 91.53

# Show variable importance plot
varImpPlot(rf_IGCorp)

# Show a list of all variables with index numbers
varNum(sector_IGCorp)

# Remove the last important variables to leave the top 10 + response variable
keep_top10_IGCorp <- c("EXUSUK", "EXKOUS", "USSLIND", "TTLCONS", "INDPRO",
                       "PAYEMS", "TCU", "EXMXUS", "S.P500", "HSN1F")

IGCorp_top10 <- sector_data[ , (names(sector_data) %in% keep_top10_IGCorp)]
names(IGCorp_top10)

# Plot a splom with the top five most important variables
offDiag <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.hexbinplot(x,y,xbins=12,...,border=gray(.7),
                   trans=function(x)x^1)
  #   panel.loess(x , y, ..., lwd=2,col='purple')
}
onDiag <- 
  function(x, ...){
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm=TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
    panel.lines(d,col=rgb(.83,.66,1),lwd=2)
    diag.panel.splom(x, ...)
  }
windows(width=9,height=9) 
splom(IGCorp_top10[,c(1,2,3,4,5)],
      xlab='',main="IGCorp: Selected Variables",
      pscale=0, varname.cex=0.8,axis.text.cex=0.6,
      axis.text.col="purple",axis.text.font=2,
      axis.line.tck=.5,
      panel=offDiag,
      diag.panel = onDiag
)
# -------------------------------------------- MBS Sector ------------------------------------------------------------- 

# Keep these columns (omit the factor data types)
keep_cols_MBS <- c("MBS_12M", "S.P500","FedRates","LIBOR","USGG2YR.Index","USGG10YR.Index", 
                   "HQMCB10YR", "PAYEMS", "IRSTCI01USM156M", "EXCAUS", "TCU", "EXCHUS",
                   "IRSTCI01USM156N", "EXCHUS", "CCI", "CPALTT01USM661S", "UMCSENT", "MCOILBRENTEU", 
                   "USEPUINDXM","MTSDS133FMS", "POILWTIUSDM", "MSACSR", "INDPRO", "EXJPUS", "EXKOUS",
                   "USSLIND", "M1SL", "M2SL", "MABMM301USM189S", "MNFCTRSMNSA", "MVGFD027MNFRBDAL",
                   "MVPHGFD027MNFRBDAL", "MSPNHSUS", "EXMXUS", "MSACSR.1", "BAA", "HSN1F", 
                   "PCE", "PSAVERT", "PCU2122212122210", "DSPIC96", "PCEC96", "CSUSHPINSA", 
                   "RECPROUSM156N", "TLAACBM027SBOG", "TTLCONS", "TOTALSL", "TOTALSA", "TWEXMMTH", "PCETRIM12M159SFRBDAL", 
                   "USD3MTD156N", "EXUSEU", "EXUSUK")
sector_MBS <- sector_data[ , (names(sector_data) %in% keep_cols_MBS)]
head(sector_MBS,5)

# View the ABS data in table format
# View(sector_MBS)

# Create a random forest model to identify the most important variables
rf_MBS <- randomForest(sector_MBS$MBS_12M ~ ., data=sector_MBS, importance=TRUE, proximity=TRUE)

# View RF model summary
rf_MBS       

# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 16

# Mean of squared residuals: 2.191452e-05
# % Var explained: 84.85

# Show variable importance plot
varImpPlot(rf_MBS)

# Show a list of all variables with index numbers
varNum(sector_MBS)

# Remove the last important variables to leave the top 10 + response variable
keep_top10_MBS <- c("TCU", "EXKOUS", "TOTALSA", "S.P500", "EXCAUS",
                    "USSLIND", "EXUSUK", "PCETRIM12M159SFRBDAL", "TTLCONS", "IRSTCI01USM156M")

MBS_top10 <- sector_data[ , (names(sector_data) %in% keep_top10_MBS)]
names(MBS_top10)

# Plot a splom with the top five most important variables
offDiag <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.hexbinplot(x,y,xbins=12,...,border=gray(.7),
                   trans=function(x)x^1)
  #   panel.loess(x , y, ..., lwd=2,col='purple')
}
onDiag <- 
  function(x, ...){
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm=TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
    panel.lines(d,col=rgb(.83,.66,1),lwd=2)
    diag.panel.splom(x, ...)
  }
windows(width=9,height=9) 
splom(MBS_top10[,c(1,2,3,4,5)],
      xlab='',main="MBS: Selected Variables",
      pscale=0, varname.cex=0.8,axis.text.cex=0.6,
      axis.text.col="purple",axis.text.font=2,
      axis.line.tck=.5,
      panel=offDiag,
      diag.panel = onDiag
)
# -------------------------------------------- USHighYield Sector ------------------------------------------------------------- 

# Keep these columns (omit the factor data types)
keep_cols_USHighYield <- c("USHighYield_12M", "S.P500","FedRates","LIBOR","USGG2YR.Index","USGG10YR.Index", 
                           "HQMCB10YR", "PAYEMS", "IRSTCI01USM156M", "EXCAUS", "TCU", "EXCHUS",
                           "IRSTCI01USM156N", "EXCHUS", "CCI", "CPALTT01USM661S", "UMCSENT", "MCOILBRENTEU", 
                           "USEPUINDXM","MTSDS133FMS", "POILWTIUSDM", "MSACSR", "INDPRO", "EXJPUS", "EXKOUS",
                           "USSLIND", "M1SL", "M2SL", "MABMM301USM189S", "MNFCTRSMNSA", "MVGFD027MNFRBDAL",
                           "MVPHGFD027MNFRBDAL", "MSPNHSUS", "EXMXUS", "MSACSR.1", "BAA", "HSN1F", 
                           "PCE", "PSAVERT", "PCU2122212122210", "DSPIC96", "PCEC96", "CSUSHPINSA", 
                           "RECPROUSM156N", "TLAACBM027SBOG", "TTLCONS", "TOTALSL", "TOTALSA", "TWEXMMTH", "PCETRIM12M159SFRBDAL", 
                           "USD3MTD156N", "EXUSEU", "EXUSUK")
sector_USHighYield <- sector_data[ , (names(sector_data) %in% keep_cols_USHighYield)]
head(sector_USHighYield,5)

# View the ABS data in table format
# View(sector_USHighYield)

# Create a random forest model to identify the most important variables
rf_USHighYield <- randomForest(sector_USHighYield$USHighYield_12M ~ ., data=sector_USHighYield, importance=TRUE, proximity=TRUE)

# View RF model summary
rf_USHighYield       

# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 16

# Mean of squared residuals: 0.001520584
# % Var explained: 92.42

# Show variable importance plot
varImpPlot(rf_USHighYield)

# Show a list of all variables with index numbers
varNum(sector_USHighYield)

# Remove the last important variables to leave the top 10 + response variable
keep_top10_USHighYield <- c("S.P500", "HSN1F", "EXKOUS", "INDPRO", "HQMCB10YR",
                            "PAYEMS", "LIBOR", "USD3MTD156N", "EXCAUS", "USSLIND")

USHighYield_top10 <- sector_data[ , (names(sector_data) %in% keep_top10_USHighYield)]
names(USHighYield_top10)

# Plot a splom with the top five most important variables
offDiag <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.hexbinplot(x,y,xbins=12,...,border=gray(.7),
                   trans=function(x)x^1)
  #   panel.loess(x , y, ..., lwd=2,col='purple')
}
onDiag <- 
  function(x, ...){
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm=TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
    panel.lines(d,col=rgb(.83,.66,1),lwd=2)
    diag.panel.splom(x, ...)
  }
windows(width=9,height=9) 
splom(USHighYield_top10[,c(1,2,3,4,5)],
      xlab='',main="USHighYield: Selected Variables",
      pscale=0, varname.cex=0.8,axis.text.cex=0.6,
      axis.text.col="purple",axis.text.font=2,
      axis.line.tck=.5,
      panel=offDiag,
      diag.panel = onDiag
)