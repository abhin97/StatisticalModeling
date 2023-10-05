#============================
# R_Practise Module 2 Week 4
#============================

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("plyr")
install.packages("vtable")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(plyr)
library(vtable)
library(hrbrthemes)
library(ggExtra)
library(epiDisplay)
library(gridExtra)
df <- read.csv("C:\\Abhinav _ NEU BOSTON\\ALY6010 Probability Theory & Intro Statistics\\Module 4\\avocado_prices_20220315.csv")
df

str(df)
st(df)
psych::describe(df)

#Type by average price group_by function
df1<-df %>%
  group_by(type) %>%
  summarise_at(vars(AveragePrice), list( Mean = mean, Median = median, SD = sd, Min = min, Max = max))
df1

#yearly average price
df2<-df %>%
  group_by(year) %>%
  summarise_at(vars(AveragePrice), list( Mean = mean, Median = median, SD = sd, Min = min, Max = max))
df2

#Type by average price group_by function
df1<-df %>%
  group_by(type) %>%
  summarise_at(vars(AveragePrice), list( AveragePrice = mean ))
df1
df3<-df %>%
  group_by(type) %>%
  summarise_at(vars(AveragePrice), list( AveragePrice = sd ))
df3


#yearly average price
df4<-df %>%
  group_by(year)%>%
  summarise_at(vars(AveragePrice), list( AveragePrice = mean)) %>%
  filter(AveragePrice == max(AveragePrice))
df4

#Maximum
df5<-df %>%
  group_by(year)%>%
  summarise_at(vars(AveragePrice), list( AveragePrice = max))
df5

#Minimum
df6<-df %>%
  group_by(year)%>%
  summarise_at(vars(AveragePrice), list( AveragePrice = min))
df6

# t-test by group
df_t_test <- t.test(df6$year, df6$AveragePrice, var.equal = TRUE.conf.interval=0.95)
df_t_test

#BoxPlot
ggplot(df, aes(AveragePrice))+
  geom_boxplot()
ggplot(df, aes(year))+
  geom_boxplot()+

#Scatter plot
ggplot1<-ggplot(df, aes(AveragePrice, Total.Volume, color =type))+
  geom_point()+
  geom_abline()


#Scatter Plot with abline
ggplot2<-ggplot(df, aes(Small.Bags, Large.Bags, color = type))+
  geom_point()+
  geom_abline()

#Scatter Plot with type
ggplot3<-ggplot(df, aes(X4046, X4770, size = type, color = type))+
  geom_point()+
  geom_abline()

#Scatter Plot with year

ggplot4<-ggplot(df, aes(Total.Volume, Total.Bags, color = type))+
  geom_point()+
  geom_abline()
grid.arrange(ggplot1, ggplot2, ggplot3, ggplot4, ncol =2, nrow =2, top = 'Comparision between type of Avocado')


#Jitterplot
ggplot5<-ggplot(df, aes(Small.Bags, Large.Bags, size = year, color = AveragePrice))+
  geom_jitter()+
  geom_abline()
ggplot5

ggplot6<-ggplot(df, aes(Small.Bags, XLarge.Bags, size = year, color = AveragePrice))+
  geom_jitter()+
  geom_abline()
ggplot6

ggplot7<-ggplot(df, aes(Large.Bags, XLarge.Bags, size = year, color = AveragePrice))+
  geom_jitter()+
  geom_abline()
ggplot7

ggplot8<-ggplot(df, aes(Total.Bags, Total.Volume, size = year, color = AveragePrice))+
  geom_jitter()+
  geom_abline()
ggplot8

grid.arrange(ggplot5, ggplot6, ggplot7, ggplot8, ncol =2, nrow =2, top = "Comparision Between Number of Bags in Years")
#t-test
df_t_test2 <- t.test(df$Total.Volume, df$AveragePrice, var.equal = FALSE, conf.level = .95)
df_t_test2

#t-test
df_t_test2 <- t.test(df$Total.Volume, df$AveragePrice, var.equal = TRUE, conf.level = .95)
df_t_test2






