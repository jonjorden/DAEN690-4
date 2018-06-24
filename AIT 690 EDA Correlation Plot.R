#Read ABS file
ABSfile <-read.csv("ABS.csv", na.strings = "n/a")

#Remove the 10 columns that all have missing data
ABS_trimmed <- ABSfile[,colSums(is.na(ABSfile))<nrow(ABSfile)]

#Remove extra columns that aren't numeric or useful
extra_cols <- c("Value.Date","Return.Type","Currency","Quality","MTD.Currency.Return")
ABS_cor <- ABS_trimmed[ , !(names(ABS_trimmed) %in% extra_cols)]

#Create matrix for Correlation
ABS_matrix <- sapply(ABS_cor, as.numeric)
cor(as.matrix(ABS_matrix), use = "complete.obs", method = "pearson")

#Correlation Plot
library(corrplot)
corrplot(cor(as.matrix(ABS_matrix), use = "complete.obs", method = "pearson"), method = "square")

library(caret)
library(ggplot2)
library(tidyr)

ggplot(gather(ABS_cor), aes(value)) + 
  +     geom_histogram(bins = 10) + 
  +     facet_wrap(~key, scales = 'free_x')
