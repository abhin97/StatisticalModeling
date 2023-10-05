install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("modeest")
install.packages("tableone")
install.packages("plyr")
install.packages("epiDisplay")
install.packages("gmodels")
install.packages("gridExtra")



library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyverse)
library(dplyr)
library(plyr)
library(psych)
library(epiDisplay)
library(gmodels)
library(modeest)
library(tableone)
library(DataExplorer)
library(gridExtra)

FB_data <-read.csv("C:\\Users\\abhin\\Downloads\\FB_conversion_data.csv")
FB_data

str(FB_data)
summary(FB_data)
glimpse(FB_data)


unique(FB_data$age)

FB_clean <- FB_data

FB_clean$age [FB_clean$age =="30-34"] <- 32
FB_clean$age [FB_clean$age =="35-39"] <- 37
FB_clean$age [FB_clean$age =="40-44"] <- 42
FB_clean$age [FB_clean$age =="45-49"] <- 47

FB_clean$age <- as.integer(FB_clean$age)

unique(FB_clean$age)

summary(FB_clean)

FB_clean$gender[FB_clean$gender == 'M'] <- 0
FB_clean$gender[FB_clean$gender == 'F'] <- 1

FB_clean$gender<- as.integer(FB_clean$gender)

unique(FB_clean$gender)
str(FB_clean)

options(scipen = 9)
FB_clean


describe_all <- describe(FB_clean)

view(describe_all)

##### Scatterplot


ggplot(FB_clean)
ggp1 <-ggplot(FB_clean, aes(x= Impressions, y = Clicks, color = Spent))+
  geom_point()

ggp2<-ggplot(FB_clean, aes(x= Impressions, y = Clicks, color = Spent))+
  geom_point()+
  geom_smooth()

ggp3<-ggplot(FB_clean, aes(x= Impressions, y = Clicks, color = Spent))+
  geom_point()+
  geom_abline(color = "red")


ggp4<-ggplot(FB_clean, aes(x= Impressions, y = Clicks, color = Spent))+
  geom_point()

#grid.arrange(ggp1, ggp2, ggp3, ggp4, ncol =2, nrow =2)





# Comparision between Interest and Spent using geom_jitter()
ggplot(FB_clean)
ggp5 <- ggplot(FB_clean, aes(x= interest, y = Clicks, color = Spent))+
  geom_jitter()+
  labs(title = "JItter Plot: Spent vs Interest")

ggp6 <- ggplot(FB_clean, aes(x= interest, y = Clicks, color = Spent))+
  geom_jitter()+
  geom_smooth()+
  labs(title = "JItter Plot: Spent vs Interest with smooth()")

ggp7<-ggplot(FB_clean, aes(x= interest, y = Clicks, color = Spent))+
  geom_jitter()+
  geom_abline(color = "Red")
  labs(title = "JItter Plot: Spent vs Interest with abline()")

ggp8<-ggplot(FB_clean, aes(x= interest, y = Clicks, color = Spent))+
  geom_jitter()
  geom_count()
  
  

grid.arrange(ggp1, ggp2, ggp3, ggp4, ncol =2, nrow =2)



ggp9 <- ggplot( FB_clean, aes(as.factor(xyz_campaign_id), Spent, main = "xyz_Campaign vs Spent"))+
  geom_boxplot(color = "Blue")+
  geom_jitter(color = 'red')
  
ggp10 <- ggplot( FB_clean, aes(as.factor(xyz_campaign_id), Spent, main = "xyz_Campaign vs Spent"))+
  geom_boxplot(color = "Blue")+
  geom_jitter(color = 'red')+
  coord_cartesian(ylim =quantile(FB_clean$Spent, c(0.1,0.9)))+
  labs(title = "Box Plot: Spent without outliers")

