library(ggplot2)
library(tidyverse)
theme_set(theme_bw(base_size=16))


df<- read.csv("world_happiness_report_20220329_csv.csv")
df

summary(df)
str(df)
st(df)
#Created Scatterplot with multiple regression line
df%>%
  ggplot(aes(x=Healthy.life.expectancy,y=Freedom.to.make.life.choices, color = Regional.indicator))+
  geom_point()

df%>%
  ggplot(aes(x=Healthy.life.expectancy,y=Freedom.to.make.life.choices, color = Regional.indicator))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)
sc_plot+
  geom_smooth(method = "lm")

 # Created Dummy Varibale for subset the dataset
df1<-ifelse(df$Regional.indicator=="Western Europe",1,0)
df2<-ifelse(df$Regional.indicator=="North America and ANZ",1,0)
df3<-ifelse(df$Regional.indicator=="Middle East and North Africa",1,0)
df4<-ifelse(df$Regional.indicator=="Latin America and Caribbean",1,0)
df5<-ifelse(df$Regional.indicator=="Central and Eastern Europe",1,0)
df6<-ifelse(df$Regional.indicator=="East Asia",1,0)
df7<-ifelse(df$Regional.indicator=="Commonwealth of Independent States",1,0)
df8<-ifelse(df$Regional.indicator=="Sub-Saharan Africa",1,0)
df9<-ifelse(df$Regional.indicator=="South Asia",1,0)
df_dummy <- data.frame(Healthy.life.expectancy=df$Healthy.life.expectancy,
                       Freedom.to.make.life.choices=df$Freedom.to.make.life.choices,
                       upperwhisker=df$upperwhisker,
                       Logged.GDP.per.capita=df$Logged.GDP.per.capita,
                       lowerwhisker=df$lowerwhisker,
                       df1=df1, df2=df2,df3=df3,df4=df4,df5=df5,df6=df6,df7=df7,df8=df8,df9=df9)



# Subset-1Health life Expe. and Freedom choice in Western Europe
sc_plot<- df_dummy%>%
  ggplot(aes(x= Healthy.life.expectancy , y= Freedom.to.make.life.choices, color=df1 ))+
  geom_point()
sc_plot+
  geom_smooth(method = "lm")+
  labs( title = "Health life Expe. and Freedom choice in Western Europe")

#Subset-2: Health life Expe. and Freedom choice in South Asia
sc_plot<- df_dummy%>%
  ggplot(aes(x= Healthy.life.expectancy , y= Freedom.to.make.life.choices, color=df9 ))+
  geom_point()
sc_plot+
  geom_smooth(method = "lm")+
  labs( title = "Health life Expe. and Freedom choice in South Asia")


#Subset-3: Health life Expe. and Logged GDP per capita in Western Europe
sc_plot<- df_dummy%>%
  ggplot(aes(x= Healthy.life.expectancy , y= Logged.GDP.per.capita, color=df1 ))+
  geom_point()
sc_plot+
  geom_smooth(method = "lm")+
  labs( title = "Health life Expe. and Logged GDP per capita in Western Europe")

#Subset-4: Health life Expe. and Logged GDP per capita in South Asia
sc_plot<- df_dummy%>%
  ggplot(aes(x= Healthy.life.expectancy , y= Logged.GDP.per.capita, color=df9 ))+
  geom_point()
sc_plot+
  geom_smooth(method = "lm")+
  labs( title = "Health life Expe. and Logged GDP per capita in South Asia")





#=============================Finish============================================





