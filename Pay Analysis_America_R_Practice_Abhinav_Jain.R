#******************#
#*                *#
#*                *#
#*   R_ Practise  *#
#*   Module 3     *#
#*   t-test       *#
#*                *#
#******************#

# loading required method

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("plyr")
install.packages("ggpubr")


# loading required library

library(ggplot2)
library(tidyverse)
library(dplyr)
library(plyr)
library(ggpubr)
library(psych)
library(skimr)
library("car")


Med_Incm <-read.csv("C:\\Abhinav _ NEU BOSTON\\ALY6010 Probability Theory & Intro Statistics\\Module 3\\median_income_by_state_2010_20220308.csv",header = TRUE,colClasses = "character")
Med_Incm

str(Med_Incm)

# data cleaning
Med_Incm_new <- c(Med_Incm)
x <- gsub(",","",Med_Incm_new$X2010)
Med_Incm_new <- as.numeric(x)
Med_Incm_new
Med_Incm$X2010 <- Med_Incm_new
Med_Incm


Southern_Incm <- Med_Incm[c(2,5,9,11,12,19,20,22,26,35,38,42,44,45,48,50), ]
Southern_Incm
summary(Southern_Incm)
str(Southern_Incm)


#histogram
ggplot(Southern_Incm,aes(x=X2010))+
  geom_histogram(color= "darkblue", fill = "lightblue", bins = 5)+
  labs(title = "Histogram of Median Income in Southern States")

#boxplot
ggplot(Southern_Incm,aes(y=X2010))+
  geom_boxplot(color= "black", fill = "orange")+
  labs(title = "Boxplot of Median Income in Southern States")

#Statistics data
describe(Southern_Incm$X2010)

#Mean to calculate t-test
mean(Southern_Incm$X2010)


#Null Hypothesis
t1<-t.test(Southern_Incm$X2010, mu = 50935, conf.level = 0.95 )
t1

#Range
r_t<-range(t1)
r_t

###Alternative hypothesis t-test-1
#greater
t2<-t.test(Southern_Incm$X2010, mu = 50935, alternative = "greater", conf.level = 0.95)
t2
#less
t3<-t.test(Southern_Incm$X2010, mu = 50935, alternative = "less", conf.level = 0.95 )
t3



points = seq(-2.1314,2.1314,length=200)
points
plot(points,dt(points,df=15),col='red',type='l')














