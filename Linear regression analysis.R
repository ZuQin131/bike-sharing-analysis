library(boot)
library(glmnet)
library(ggplot2)


###########################################
### I. regression - day 
### data processing 
### 
###########################################

day <- read.csv("./data/day.csv")

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
day2$dteday <- as.Date(day2$dteday,"%Y-%m-%d")
str(day2)

day3=day2[,-c(1,2,15,14)]
str(day3)

#VIF Check
day4 <- day3[,c(11,10,9)]
day.vif <- cor(day4)
kappa(day.vif,exact = T)
#:: 265

day4 <- day3[,c(11,10,9,8)]
day.vif <- cor(day4)
kappa(day.vif,exact = T)
#:: 1.86

#########################################
### I. regression - day
########### 2.1  regression analysis: 
#####   Generalized linear regression
###
#########################################


model <- lm(data = day3, cnt~mnth+atemp+hum+windspeed)
summary(model)

model2 <- glm(data = day3, cnt~as.factor(mnth)+as.factor(weathersit)+atemp+hum+windspeed)
summary(model2)
1-(1201819598/2739535392)

model3 <- glm(data = day3, cnt~as.factor(season)+as.factor(mnth)+as.factor(weathersit)+atemp+hum+windspeed)
summary(model3)
1-(1147923776/2739535392)

model4 <- glm(data = day3, cnt~as.factor(season)+as.factor(yr)+as.factor(holiday)+as.factor(weekday)+as.factor(workingday)+as.factor(mnth)+as.factor(weathersit)+atemp+hum+windspeed)
summary(model4)
1-(417869064/2739535392)

# :: strong relation between temp and atemp ，so get rid of temp
model5 <- glm(data = day3, cnt~as.factor(yr)+as.factor(weekday)+as.factor(workingday)+as.factor(mnth)+as.factor(weathersit)+atemp+hum+windspeed)
summary(model5)
1-(467112813/2739535392)

#normality Test , same variance 
library(car)
res4 <- residuals(model4)
plot(density(res4))
shapiro.test(res4) #:: 0.00758


res5 <- residuals(model5)
plot(density(res5))
shapiro.test(res5) #:: 0.00000166305


##########################################
### I. regression - day 
######### linear regression : dummy 
###
##########################################

library(dummies)
df11 <- day3
df11 <- cbind(df11, dummy(df11$season, sep = "season"))
df11 <- cbind(df11, dummy(df11$yr, sep = "yr"))
df11 <- cbind(df11, dummy(df11$mnth, sep = "mnth"))
df11 <- cbind(df11, dummy(df11$holiday, sep = "holiday"))
df11 <- cbind(df11, dummy(df11$weekday, sep = "weekday"))
df11 <- cbind(df11, dummy(df11$workingday, sep = "workingday"))
df11 <- cbind(df11, dummy(df11$weathersit, sep = "weathersit"))
df11 <- df11[,-c(1:8)]

model6 <- lm(data = df11, cnt~.)
summary(model6)

# the two Tests
res6 <- residuals(model6)
shapiro.test(res6) #:: 0.00530779229
ncvTest(model6) #::0.018

op <- par(mfrow=c(2,2))
plot(model6)
par(op)

###########################################
### I. regression - day 
###########  validation K-Fold: K=10
### 
###########################################
library(boot)
library(DAAG)

set.seed(123)

cv.glm(day3,model5,K=10)$delta[1] #:: 700190.3

cv.glm(day3,model4,K=10)$delta[1] #:: 639885.6

cv.lm(df11,model6,m=10)$delta[1] #:: 636300 

###########################################
### I. regression - day 
##########  lasso: poisson regression
### 
###########################################
library(glmnet)
library(tidyverse)

##### lasso input #####
##### one-hot encoding #####

day3 <- day3[,-8]
day3$yr <- factor(day3$yr)
day3$season <- factor(day3$season)
day3$mnth <- factor(day3$mnth)
day3$weekday <- factor(day3$weekday)
day3$holiday <- factor(day3$holiday)
day3$workingday <- factor(day3$workingday)
day3$weathersit <- factor(day3$weathersit)

x <- model.matrix(cnt~.,day3)[,-12]
y<-day3$cnt
# lam <- c(0,5,10,15,20)

###### lasso regression model ######
lasso.fit <- glmnet(x,y,family = "poisson", alpha = 1,nlambda = 50) #  auto test lambda 
lasso.fit
plot(lasso.fit,xvar = "lambda",label = T)
coef(lasso.fit)


###### find optimal lambda ######
cv.fit<-cv.glmnet(x,y,family="poisson",alpha = 1,type.measure = "mse")  # cross-validation nfold = 10
lam.opt<-cv.fit$lambda.1se
cv.fit$lambda.1se
coef(cv.fit,lam.opt) 
plot(cv.fit)

### optimal lassso model
lasso.opt <- glmnet(x,y,family = "poisson",alpha = 1,lambda = 35.61)
lasso.opt
#:: Df 21, %Dev  81.25, Lambda 35.61 


###########################################
### I. regression - day 
######### 2.3 optimal regression model visualizing 
### 
###########################################

predict1 <- predict(model4,day3)

day2$predict1 <- predict1

ggplot(day2)+
  geom_line(aes(dteday,cnt,colour='cnt'),size=0.4)+
  geom_line(aes(dteday,predict,colour='predict1'),size=1)


library(stats)

predict2 <- predict.lm(model6,df11)
day2$predict2 <- predict2

ggplot(day2)+
  geom_line(aes(dteday,cnt,colour='cnt'),size=0.4)+
  geom_line(aes(dteday,predict2,colour='predict2'),size=1)


predict3 <- predict(lasso.opt,newx=x ,s=35.61,type="response")
day2$predict3 <- predict3

ggplot(day2)+
  geom_line(aes(dteday,cnt,colour='cnt'),size=0.4)+
  geom_line(aes(dteday,predict3,colour='predict3'),size=1)


#############################################################################
### II. regression - hour
######## data processing    
### 
#############################################################################
library(corrplot)

hour <- read.csv("./data/hour.csv")

hour2 <- as.data.frame(hour)
hour2$yr=as.numeric(hour2$yr)
hour2$mnth=as.numeric(hour2$mnth)
hour2$hr=as.numeric(hour2$hr)
hour2$season=as.numeric(hour2$season)
hour2$holiday=as.numeric(hour2$holiday)
hour2$weekday=as.numeric(hour2$weekday)
hour2$workingday=as.numeric(hour2$workingday)
hour2$weathersit=as.numeric(hour2$weathersit)
hour2$casual=as.numeric(hour2$casual)
hour2$registered=as.numeric(hour2$registered)
hour2$cnt=as.numeric(hour2$cnt)
hour2$dteday <- as.Date(hour2$dteday,"%Y-%m-%d")
str(hour2)

hour3=hour2[,-c(1,2,15,16)]
corrplot(cor(hour3))



# Linear Regression
h.model <- lm(data = hour3, cnt~mnth+hr+temp+atemp+hum+windspeed)
summary(model)

# Generalized Linear Regression
h.model2 <- glm(data = hour3, cnt~as.factor(mnth)+as.factor(hr)+as.factor(weathersit)+temp+atemp+hum+windspeed)
summary(model2)
1-(213525876/571761591)

h.model3 <- glm(data = hour3, cnt~as.factor(season)+as.factor(mnth)+as.factor(hr)+as.factor(weathersit)+temp+hum+windspeed)
summary(model3)
1-(211126755/571761591)

h.model4 <- glm(data = hour3, cnt~as.factor(season)+as.factor(yr)+as.factor(holiday)+as.factor(weekday)+as.factor(workingday)+as.factor(mnth)+as.factor(hr)+as.factor(weathersit)+temp+atemp+hum+windspeed)
summary(model4)
1-(179328746/571761591)

h.model5 <- glm(data = hour3, cnt~as.factor(season)+as.factor(yr)+as.factor(weekday)+as.factor(workingday)+as.factor(mnth)+as.factor(hr)+as.factor(weathersit)+temp+atemp+hum+windspeed)
summary(model5)
1-(179328746/571761591)

h.model6 <- glm(data = hour3, cnt~as.factor(hr)+as.factor(yr)+as.factor(weekday)+as.factor(workingday)+as.factor(mnth)+as.factor(weathersit)+temp+atemp+hum+windspeed)
summary(model6)
1-(181727213/571761591)



library(dummies)
df11 <- hour3
df11 <- cbind(df11, dummy(df11$season, sep = "season"))
df11 <- cbind(df11, dummy(df11$yr, sep = "yr"))
df11 <- cbind(df11, dummy(df11$mnth, sep = "mnth"))
df11 <- cbind(df11, dummy(df11$hr, sep = "hr"))
df11 <- cbind(df11, dummy(df11$holiday, sep = "holiday"))
df11 <- cbind(df11, dummy(df11$weekday, sep = "weekday"))
df11 <- cbind(df11, dummy(df11$workingday, sep = "workingday"))
df11 <- cbind(df11, dummy(df11$weathersit, sep = "weathersit"))
df11 <- df11[,-c(1:8)]

h.model7 <- lm(data = df11, cnt~.)
summary(h.model7)

h.res7 <- residuals(h.model7)
shapiro.test(h.res7) #:: 
ncvTest(h.model7) #::

par(mfrow=c(2,2))
plot(h.model7)

###########################################
### II. regression - hour 
##########  lasso: variables selection
### 
###########################################
library(glmnet)
library(tidyverse)

##### lasso input #####
##### one-hot encoding #####

hour3$yr <- factor(hour3$yr)
hour3$season <- factor(hour3$season)
hour3$mnth <- factor(hour3$mnth)
hour3$weekday <- factor(hour3$weekday)
hour3$holiday <- factor(hour3$holiday)
hour3$workingday <- factor(hour3$workingday)
hour3$weathersit <- factor(hour3$weathersit)
hour3$hr <- factor(hour3$hr)
x2 <- model.matrix(cnt~.,hour3)[,-13]
model.matrix(cnt~.,hour3)
y2 <- hour3$cnt
# lam <- c(0,5,10,15,20)

###### lasso regression model ######
lasso.hour.fit <- glmnet(x2,y2,family = "poisson", alpha = 1,nlambda = 50) #  auto test lambda 
lasso.hour.fit
plot(lasso.hour.fit,xvar = "lambda",label = T)
#coef(lasso.fit)[,c(1,2,3,4,5)]


###### find optimal lambda ######
cv.hour.fit<-cv.glmnet(x2,y2,family="poisson",alpha = 1,type.measure = "mse")  # cross-validation nfold = 10
lam.hour.opt<-cv.hour.fit$lambda.1se
cv.hour.fit$lambda.1se
coef(cv.hour.fit,lam.hour.opt)
plot(cv.hour.fit)

### optimal lassso model
lasso.hour.opt <- glmnet(x2,y2,family = "poisson",alpha = 1,lambda = 0.7008157)
lasso.hour.opt

#:: Df 45, %Dev  79.52, 0.7008157 

#visualize

hour.predict3 <- predict(lasso.hour.opt,newx=x2 ,s=0.7008157,type="response")

hour2$predict3 <- hour.predict3


# overlay version
ggplot(hour2)+
  geom_point(aes(dteday,cnt,colour='cnt'),size=0.4)+
  geom_point(aes(dteday,predict3,colour='cnt'),size=1)


# rainbow version 
library(colorspace)
colors <- rainbow_hcl(24)

colours <- c("aquamarine",'coral1','azure3','cornflowerblue','forestgreen','cyan1','darkgoldenrod1','blue4','green','darkmagenta','darkorange',
             'cadetblue1','deeppink3','gray77','ivory2','lavender','lightblue','lightcoral','navy','yellowgreen','tomato3','firebrick','grey0')

r1 <- ggplot(hour2,aes(dteday,cnt)) +
  geom_point(position=position_jitter(w=1, h=0),aes(color=hr),alpha=0.8)+
  scale_color_gradientn(colours =colours)+
  scale_y_continuous(breaks = seq(0,1000,100))+
  theme_bw()

r2 <- ggplot(hour2,aes(dteday,predict3)) +
  geom_point(position=position_jitter(w=1, h=0),aes(color=hr),alpha=0.8)+
  scale_color_gradientn(colours =colours)+
  scale_y_continuous(breaks = seq(0,1000,100))+
  theme_bw()

r1/r2





