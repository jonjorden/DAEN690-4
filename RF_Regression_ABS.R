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
library(missForest)

# Read in the data
# sector_data <-read.csv('~/Documents/4_GMU MS/DAEN690/Team 4_Sector Allocation/Data Analysis/Combined_Dataset_v3.csv')
sector_data <-read.csv('Combined_Dataset_v3.csv')

# Check for missing values
sum(is.na(sector_data))

# Look at all column names to keep based on dtype
colnames(sector_data)

# Keep these columns
keep_cols <- c("ABS_12M", "S.P500","FedRates","LIBOR","USGG2YR.Index","USGG10YR.Index", 
               "HQMCB10YR", "PAYEMS", "IRSTCI01USM156M", "EXCAUS", "TCU", "EXCHUS",
               "IRSTCI01USM156N", "EXCHUS", "CCI", "CPALTT01USM661S", "UMCSENT", "MCOILBRENTEU", 
               "USEPUINDXM","MTSDS133FMS", "POILWTIUSDM", "MSACSR", "INDPRO", "EXJPUS", "EXKOUS",
               "USSLIND", "M1SL", "M2SL", "MABMM301USM189S", "MNFCTRSMNSA", "MVGFD027MNFRBDAL",
               "MVPHGFD027MNFRBDAL", "MSPNHSUS", "EXMXUS", "MSACSR.1", "BAA", "HSN1F", 
               "PCE", "PSAVERT", "PCU2122212122210", "DSPIC96", "PCEC96", "CSUSHPINSA", 
               "RECPROUSM156N", "TLAACBM027SBOG", "TTLCONS", "TOTALSL", "TOTALSA", "TWEXMMTH", "PCETRIM12M159SFRBDAL", 
               "USD3MTD156N", "EXUSEU", "EXUSUK")
sector_ABS <- sector_data[ , (names(sector_data) %in% keep_cols)]
head(sector_ABS,5)

# View the ABS data in table format
View(sector_ABS)

# ------------------------------------------- Random Forest -----------------------------------------------------------
# Random forests assesses a variable's importance by randomizing the positions a variable's values in the vector. 
# How much does the model's MSE increase?

# Create a random forest model to identify the most important variables
rf <- randomForest(sector_ABS$ABS_12M ~ ., data=sector_ABS, importance=TRUE, proximity=TRUE)

# View RF model summary
rf            

# Show variable importance plot
varImpPlot(rf)

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
ABS_top10 <- sector_data[ , (names(sector_data) %in% keep_top10)]
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
