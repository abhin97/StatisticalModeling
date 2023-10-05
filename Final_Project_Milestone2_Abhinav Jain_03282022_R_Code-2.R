#================================
#==========Milestone 1===========
#================================

install.packages("tidyverse")
install.packages("dplyr")
install.packages("plyr")
install.packages("epiDisplay")
install.packages("gmodels")
install.packages("ggplot")
install.packages("janitor")
install.packages("imputeTS")

library(datasets)
library(tidyverse)
library(dplyr)
library(plyr)
library(psych)
library(epiDisplay)
library(skimr)
library(gmodels)
library(ggplot2)
library(vtable)
library(janitor)
library(imputeTS)



wine_data <- read.csv("Wine_tasting_20220222.csv")
wine_data
wine_new <- wine_data
wine_new[wine_new == ""] <- NA
wine_new


# Rename 
wine_aly_rename1 <- rename_with(wine_new, toupper)
wine_aly_rename1

#Drop
wine_aly_drop <- subset(wine_aly_rename1, select = -c (DESCRIPTION, TITLE, TASTER_NAME, TASTER_TWITTER_HANDLE, WINERY, REGION_1, REGION_2))
wine_aly_drop

wine_aly_1 <- wine_aly_drop
wine_aly_1

#Structure of the dataset
str(wine_aly_1)

#Summary of the dataset
summary(wine_aly_1)

#Glimpse of the dataset
glimpse(wine_aly_1)
plot(wine_aly_1)

# BOX PLOT
boxplot(wine_aly_1$PRICE, main = "Price")
boxplot(wine_aly_1$POINTS, main = "Points")


#Plot a frequency barchart of a price of wine
wine_aly_1 <- table(wine_aly_1$PRICE)
bar_graph1 <- barplot(wine_aly_1, main = "Price of Wine", ylim = c(0, 60), ylab = "Frequency",
                      xlab = "PRICE", col = heat.colors(6), cex.axis = 0.7, cex.names = 0.7)
text(y = wine_aly_1, bar_graph1, wine_aly_1, cex = 0.5, pos = 3)

#Plot a frequency barchart for points of wine
wine_aly_2 <- wine_aly_drop
wine_aly_2
wine_aly_2 <- table(wine_aly_2$POINTS)
bar_graph2 <- barplot(wine_aly_2, main = "Point of Wine", ylim = c(0, 250), ylab = "Frequency",
                      xlab = "POINT", col = heat.colors(6), cex.axis = 0.7, cex.names = 0.7)
text(y = wine_aly_2, bar_graph2, wine_aly_2, cex = 0.8, pos = 3)

# Through ggplot find highest price of provices
wine_aly_3 <- wine_aly_drop[!(is.na(wine_aly_drop$PROVINCE) | wine_aly_drop$PROVINCE==""), ]
wine_aly_3

avg_price <-  wine_aly_3 %>% group_by(PROVINCE) %>% drop_na() %>% summarize(mean_price = mean(PRICE))
arrange(avg_price, -mean_price)

province_reviews_5 <- wine_aly_3 %>% group_by(PROVINCE) %>% filter(n() >= 5)

avg_price_5 <- province_reviews_5 %>% group_by(PROVINCE) %>% drop_na() %>% summarize(mean_price = mean(PRICE))

arrange(avg_price_5, -mean_price)

ggplot(data = wine_aly_3) + geom_point(mapping=aes(x=POINTS,y=PRICE),color="Green") +
  geom_smooth(mapping=aes(x=POINTS,y=PRICE),color="Red") +
  labs(title="Wine Ratings: Price vs. Points",
       subtitle="Wine ratings and their relationship with the price of wine", caption="Data published by WineEnthusiast")


#==================================================*
#===================MileStone-2====================*
#==================================================*


#Hypothesis Testing performing t-test

#two-sample test from the wine tasting dataset

#defining two variables price and point

POINTS= rnorm(10)
PRICE = rnorm(10)

#performing t.test()

t.test(POINTS,PRICE, var.equal = TRUE)
t.test(POINTS,PRICE, var.equal = FALSE)

#One- Sample t.test from the wine-tasting dataset
t.test(PRICE, mu = 37)
t.test(PRICE, mu=37, alternative = 'greater')
t.test(PRICE, mu=37, alternative = 'less')

#====================================================

#

avg_pointprovinces<- wine_aly_3 %>%
  group_by(PROVINCE) %>%
  drop_na()%>%
  summarise(averagepoint = mean(POINTS))

arrange(avg_pointprovinces, averagepoint)
unique(wine_aly_3$POINTS)
top_province<- c(c("Südburgenland", "Madeira", "Mittelrhein","Tejo","Tokaji", "Puente Alto", "Wachau", "England", "Santa Cruz", "Leithaberg", "Kamptal", "Traisental"))

#Province by Points 

wine_aly_3 %>%
  filter(PROVINCE %in% top_province) %>%
  ggplot(aes(x = PROVINCE,y = POINTS, fill=VARIETY, color = COUNTRY)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 0)) +
  labs(title="Top Provinces by Total Points for Wine")

#Provience by Price
wine_aly_3 %>%
  filter(PROVINCE %in% top_province) %>%
  ggplot(aes(x = PROVINCE,y = PRICE, fill=VARIETY, color = COUNTRY)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 0)) +
  labs(title="Top Provinces by Price for Wine")























