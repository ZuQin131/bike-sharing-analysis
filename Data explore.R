library(data.table) 
library(ggplot2)
library(patchwork)
library(reshape2)
library(tidyverse)
library(corrplot)

library(magrittr)

library(dplyr)
library(tidyr)

library(ggthemes)
library(purrr)
library(caret)

library(ggfortify)
library(cluster)

hour <- read.csv('./data/hour.csv')
day <- read.csv("./data/day.csv")

summary(day)
table(is.na(day))
str(day)

# 0.data processing 
# 0.1 change all int to numeric
day2 <- as.data.frame(day)

day2$yr=as.numeric(day2$yr)
day2$mnth=as.numeric(day2$mnth)
day2$season=as.numeric(day2$season)
day2$holiday=as.numeric(day2$holiday)
day2$weekday=as.numeric(day2$weekday)
day2$workingday=as.numeric(day2$workingday)
day2$weathersit=as.numeric(day2$weathersit)
day2$casual=as.numeric(day2$casual)
day2$registered=as.numeric(day2$registered)
day2$cnt=as.numeric(day2$cnt)
str(day2)

# 0.2 adjust time format : 2010-01-01 => "2010-01-01"
# :: important for time series plot(1.5)
day2$dteday <- as.Date(day2$dteday,"%Y-%m-%d")

# 1.data exploration
# 1.1 histogram
op <- par(mfrow=c(4,3))
par(mar=rep(3,4))
hist(day2$season)
hist(day2$holiday)
hist(day2$weekday)
hist(day2$workingday)
hist(day2$weathersit)
hist(day2$temp)
hist(day2$atemp)
hist(day2$hum)
hist(day2$windspeed)
hist(day2$casual)
hist(day2$registered)
hist(day2$cnt)
par(op)

# 1.2 the correlation between variables
day3=day2[,-c(1,2,15,14)]
corrplot(cor(day3))


# 1.3 box plot
p1 <- ggplot(data = day2, mapping = aes(x = season, y = cnt,fill = factor(season))) +
  geom_boxplot()

p2 <- ggplot(data = day2, mapping = aes(x = weathersit, y = cnt,fill = factor(weathersit))) +
  geom_boxplot()

p3 <- ggplot(data = day2, mapping = aes(x = mnth, y = cnt,fill = factor(mnth))) +
  geom_boxplot()

p4 <- ggplot(data = day2, mapping = aes(x = workingday, y = cnt,fill = factor(workingday))) +
  geom_boxplot()

p5 <- ggplot(data = day2, mapping = aes(x = weekday, y = cnt,fill = factor(weekday))) +
  geom_boxplot()

# :: use patchwork
p1/p2/p3/p4/p5

# 1.4 compare histogram

df1 <- data.frame(day2$registered, day2$casual, day2$weekday)
df2 <- melt(df1, id.vars='day2.weekday')
h1 <- ggplot(df2, aes(x=day2.weekday, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

df3 <- data.frame(day2$registered, day2$casual, day2$holiday)
df4 <- melt(df3, id.vars='day2.holiday')
h2 <- ggplot(df4, aes(x=day2.holiday, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

df5 <- data.frame(day2$registered, day2$casual, day2$mnth)
df6 <- melt(df5, id.vars='day2.mnth')
h3 <- ggplot(df6, aes(x=day2.mnth, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

h1/h2/h3

# 1.5 time series plot 

# 1.5.1 time series plot : cnt, registered, casual 
ggplot() +
  geom_line(data = day2,aes(dteday,cnt,colour="cnt"),size=1)+
  geom_smooth(data = day2, method = 'loess',formula="y~x",color='black',aes(dteday,cnt,color='black'))+
  geom_line(data = day2,aes(dteday,registered,colour="registered"),size=1) +
  geom_smooth(data = day2, method = 'loess',formula="y~x",color='black',aes(dteday,registered,color='black'))+
  geom_line(data = day2,aes(dteday,casual,colour="casual"),size=1)+
  geom_smooth(data = day2, method = 'loess',formula="y~x",color='black',aes(dteday,casual,color='black'))+
  xlab("Day")+ylab("Count")


day2.s <- day2 %>%
  select(dteday,cnt, registered, casual) %>%
  gather(var,value,cnt:casual)


ggplot(day2.s)+
  geom_line(aes(x=dteday,y=value,col=var),alpha=0.4)+
  geom_smooth(method = 'loess', formula = 'y~x', aes(x = dteday, y = value, col = var), alpha = 1)+
  scale_y_continuous(limits = c(0,10000), breaks = c(0,2500,5000,7500,10000))+
  facet_grid(var~.)+
  scale_color_tableau()+
  theme_hc()


# 1.5.2 time series plot : different variable : temp, hum, windspeed, cnt

# define a function for normalize (cnt.norm)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

day2$cnt.norm <- normalize(day2$cnt)


day2.c <- day2 %>%
  select(dteday,temp,hum:windspeed,cnt.norm,season:weathersit) %>%
  gather(var,value,temp:cnt.norm)


ggplot(day2.c)+
  geom_line(aes(x = dteday, y = value, col = var), alpha = .4)+
  geom_smooth(method = 'loess', formula = 'y~x', aes(x = dteday, y = value, col = var), alpha = 1)+
  scale_y_continuous(limits = c(0,1), breaks = c(0,.25,.5,.75,1))+
  facet_grid(var~.)+
  scale_color_tableau()+
  theme_hc()

# 1.6 hour distribution 

pl <- ggplot(filter(hour,workingday==1),aes(hr,cnt)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

pl <- ggplot(filter(hour,workingday==0),aes(hr,cnt)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

pl1 <- ggplot(filter(hour,workingday==1),aes(hr,registered)) 
pl1 <- pl1 + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl1 <- pl1 + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
h1<- pl1 + theme_bw()

pl2 <- ggplot(filter(hour,workingday==0),aes(hr,registered)) 
pl2 <- pl2 + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
pl2 <- pl2 + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
h2<- pl2 + theme_bw()

pl3 <- ggplot(filter(hour,workingday==1),aes(hr,casual)) 
pl3 <- pl3 + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl3 <- pl3 + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))h3
h3<- pl3 + theme_bw()

pl4 <- ggplot(filter(hour,workingday==0),aes(hr,casual)) 
pl4 <- pl4 + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
pl4 <- pl4 + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
h4 <- pl4 + theme_bw()

(h1+h2)/(h3+h4)


#1.7 clustering 
day4 <- day3[,c(9,10,11)]
set.seed(1)
c1 <- autoplot(kmeans(day4,4),data=day4,label =T,label.size =3)
c1
c2 <- autoplot(kmeans(day4,5),data=day4,label =T,label.size =3)
c3 <- autoplot(kmeans(day4,10),data=day4,label =T,label.size =3)
c1/c2/c3

k = 3
set.seed(1)
day2$cluster <- kmeans(day4,k)$cluster
for (i in 1:k){
  print(mean(subset(day2,cluster==i)$cnt))
}
#:: 均值存在差异，即天气情况影响使用量


day2$cluster3 <- kmeans(day4,3)$cluster
day2$cluster5 <- kmeans(day4,5)$cluster
day2$cluster10 <- kmeans(day4,10)$cluster

attach(day2)
p3.manova <- manova(cbind(atemp,hum,windspeed)~cluster3)
summary(p3.manova,test='Wilks')
#:: 0.00205

p5.manova <- manova(cbind(atemp,hum,windspeed)~cluster5)
summary(p5.manova,test='Wilks')
#:: 

p10.manova <- manova(cbind(atemp,hum,windspeed)~cluster10)
summary(p10.manova,test='Wilks')
#:: 0.0091

