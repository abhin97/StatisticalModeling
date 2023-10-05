#===================================

#     R_Script Module 5 (Week5)

#===================================

install.packages('ggplot2')
install.packages('tidyverse')
install.packages('dplyr')
install.packages('psych')
install.packages("ggpubr")
install.packages("Hmisc")
install.packages("jtools")
install.packages("corrplot")
install.packages("PerformanceAnalytics")

library(ggplot2)
library(broom)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(psych)
library("Hmisc")
library(fs)
library(lubridate)
library(jtools)
library(vtable)
library(corrplot)
library(PerformanceAnalytics)


df<- read.csv("Diesel.csv", header = TRUE, na.strings = c("","", "NA"))
df

view(df)    #view the dataset variable 
str(df)     # view structure 
describe(df)#descriptive analysis (mean, median , mode)
nrow(df)    #number of rows 
ncol(df)    #number of columns
st(df)      #summary of statistics gives better understanding the describe ()
head(df)
tail(df)
class(df)
colnames(df)
summary(df)

#Pre-Processing for correlation test 

#Apply gsub() to convert range.km data type into numeric
df1<- c(df)
x<-gsub(",","",df1$RANGE.km)
df1<-as.numeric(x)
df1
df$RANGE.km<-df1
df

#covert CYL data type to numeric
df$CYL <- as.numeric(df$CYL)
df

#covert FUEL.TANK.L data type to numeric
df$FUEL.TANK.L<- as.numeric(df$FUEL.TANK.L)
df

#Build Dataframe for correlation matrix
df1<- df[,c("CYL","ENGINE.L","FUEL.TANK.L","CONS..L.100km","RANGE.km")]
str(df1)
describe(df1)
ncol(df1)
nrow(df1)
head(df1)
tail(df1)
st(df1)

#correlation matrix using cor() and round to 2 decimals
df1_cor <- cor(df1)
round(df1_cor,2)

#rcorr() to get n and p-value
df1_rcorr <- rcorr(as.matrix(df1))
df1_rcorr

#p-value
df1_pvalue<-df1_rcorr$P
round(df1_pvalue,2)

#Histogram for correlation matrix
chart.Correlation(df1, histogram= TRUE, pch=19)


#Heatmap for correlation matrix
col<- colorRampPalette(c("Green", "yellow", "red"))(20)
heatmap(x = df1_cor, col = col, symm = TRUE)

#Methods for Correlation with significance level

cor(df1$CYL, df1$RANGE.km, method = "pearson")
cor(df1$CYL, df1$RANGE.km, method = "kendall")
cor(df1$CYL, df1$RANGE.km, method = "spearman")

#Applying Linear Regression model
df1_r <- as.data.frame(df1)
fit <- lm(RANGE.km ~ FUEL.TANK.L+ CYL+ ENGINE.L+ CONS..L.100km, data = df1_r)
summ(fit)
 

#=======================================Finish===============================






