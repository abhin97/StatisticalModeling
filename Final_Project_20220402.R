install.packages("BSDA")
install.packages("dplyr")

library(ggplot2)
library(dplyr)
library(vtable)
library("Hmisc")
library(fs)
library(corrplot)
library(PerformanceAnalytics)
library(jtools)
library(car)
library(BSDA)

df<- read.csv("Airports2.1.csv")
df


unique(df$Fly_date)
summary(df)
st(df)
df1<- plot(df$Passengers,df$Seats,main = "Scatterplot between Passangers and Seats")

boxplot(df$Passengers,df$Seats, main ="Box Plot of Passangers and Seats")


#make the qqplot 

qqnorm(df$Seats)
qqline(df$Seats)
sd(df$Seats)


#One-Sample z-Test Passengers
z.test(x=df$Passengers, mu= 2536.33, sigma.x=4808)

#As we can see the value of p is greater than 0.05,
#which signify that we do not have the enough of sufficient evidence
#to reject the null hypothesis

z.test(x=df$Seats, mu= 3363, sigma.x=6131)

#Two Sample Test
z.test(x=df$Passengers, sigma.x=4808.92, y=df$Origin_population, sigma.y=7845570.21)

#As the value of two sample test z-test is equal to -7.7, 
#however the value of p is 1.76. As we can see the p-value is
#not less then 0.05, we can reject the null hypotheisis
#because we do not have enough evidence.





#Correlation
df$Passengers<- as.numeric(df$Passengers)
df$Seats<- as.numeric(df$Seats)
df$Flights<- as.numeric(df$Flights)
df$Distance<- as.numeric(df$Distance)
df$Origin_population<- as.numeric(df$Origin_population)
df$Destination_population<- as.numeric(df$Destination_population)

df_cor <- df[,c("Passengers","Seats","Flights","Distance","Origin_population","Destination_population")]


df_corre<- cor(df_cor)
round(df_corre,2)
ggplot(df_cor, aes(Passengers,Seats, color = Flights))+
  geom_point()+
  geom_abline()+
  labs(title = "Correlation Between Passengers and Seats")
chart.Correlation(df_corre, histogram= TRUE, pch=19)
# 
df_corre<- cor(df_cor)
round(df_corre,2)
ggplot(df_cor, aes(Passengers,Distance, color=Seats))+
  geom_point()+
  geom_abline()+
  labs(title = "Correlation Between Distance and Passengers")


chart.Correlation(df_corre, histogram= TRUE, pch=19)

#Regression Testing

df1_r <- as.data.frame(df_cor)
fit <- lm(Flights ~ Seats+ Distance+ Passengers, data = df1_r)
summ(fit)



#==========================Finish====================================#








