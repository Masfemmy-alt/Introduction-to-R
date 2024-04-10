# Introduction-to-R
This is R Code for Beginners
### Get Directory
setwd(dir="C:/Users/user/Documents/ASSFBI573")
#### Import the Dataset
HD <-read.csv("HD.csv")
HD
attach(HD)
names(HD)

set.seed(500)

index <- sample(1:nrow(HD), round(0.75*nrow(HD)))  ### 75% fitting

HDfit <- HD[index,]  # fitting data

HDval <- HD[-index,] #validating data

write.csv(HDfit, "HDfit.csv")

write.csv(HDfit, "HDval.csv")

HD1 <- nls(Height~1.3+a*(1-exp(-b*Dbh)),  data = HD,  start = list(a=10, b=0.7855))
summary(HD1)

HD2 <- nls(Height~1.3+a/(1+b*exp(-c*Dbh)), data = HD,  start = list(a=10, b=2, c=0.7855))
summary(HD2)

HD3 <-nls(Height~1.3+a*exp(-b*exp(-c*Dbh)),  data = HD, start = list (a=13, b=0.5, c=-0.5))
summary(HD3)


plot(HD1)
predict(HD1)
resid(HD1)
hist(resid(HD1), main =  "Histogram of Meyer Model")

library(car)
qqPlot(resid(HD1))
plot(fitted(HD1), resid(HD1))
abline(0,0)

plot(HD2)
predict(HD2)
resid(HD2)
hist(resid(HD2), main =  "Histogram of Logistic model")

library(car)
qqPlot(resid(HD2))
plot(fitted(HD2), resid(HD2))
abline(0,0)

plot(HD3)
predict(HD3)
resid(HD3)
hist(resid(HD3), main =  "Histogram of Gompertz model")

library(car)
qqPlot(resid(HD3))
plot(fitted(HD3), resid(HD3))
abline(0,0)

AIC(HD1)
AIC(HD2)
AIC(HD3)

####Refit the HD1 because it have low AIC among the three H-D model of 7752.32

####with the HDfit for validation

HD1 <- nls(Height~1.3+a*(1-exp(-b*Dbh)),  data = HDfit,  start = list(a=10, b=0.7855))
summary(HD1)

pred <-predict(HD1, HDval)

HDval$predHeight <-pred
HDval

shapiro.test(HDval$Height)

shapiro.test(HDval$predH)

sd(HD$Height)
 
library(DescTools)
ZTest(x=HDval$H, y=pred, paired=FALSE, mu=0, sd_pop=5.288785, conf.level=0.95) #when the P-value is not significant it means the model is ideal 
 

STAND <- read.csv("Stand.csv")
STAND
attach(STAND)
names(STAND)

set.seed(500)

index <- sample(1:nrow(STAND), round(0.75*nrow(STAND)))  ### 75% fitting

STANDfit <- STAND[index,]  # fitting data

STANDval <- STAND[-index,] #validating data

write.csv(STANDfit, "STANDfit.csv")

write.csv(STANDfit, "STANDval.csv")


STAND1 <- nls((Dq~a*N^b*Hd^c), data=STAND, start= list(a=10, b=0.5, c=-0.5))
STAND1

STAND2 <- nls(G~a*N^b*exp(c/Age+d*Hd), data=STAND, start= list( a=0.5 ,b=2.22, c=-0.5, d=0.065))
STAND2

STAND3 <- nls(V~a*Dq^b*Hd^c*N^d, data=STAND, start= list( a=2 ,b=1, c=2, d=1))
STAND3

plot(STAND1)
predict(STAND1)
resid(STAND1)
hist(resid(STAND1), main =  "Histogram of Model 1")

library(car)
qqPlot(resid(STAND1))
plot(fitted(STAND1), resid(STAND1))
abline(0,0)

plot(STAND2)
predict(STAND2)
resid(STAND2)
hist(resid(STAND2), main =  "Histogram of Model 2")

library(car)
qqPlot(resid(STAND2))
plot(fitted(STAND2), resid(STAND2))
abline(0,0)

plot(STAND3)
predict(STAND3)
resid(STAND3)
hist(resid(STAND3), main =  "Histogram of Model 3")

library(car)
qqPlot(resid(STAND3))
plot(fitted(STAND3), resid(STAND3))
abline(0,0)
AIC(STAND1)
AIC(STAND2)
AIC(STAND3)

####Refit the STAND1 because it have low AIC among the three STAND model of 41719.01

####with the STANDfit for validation


STAND1 <- nls((Dq~a*N^b*Hd^c), data=STANDfit, start= list(a=10, b=0.5, c=-0.5))
STAND1

pred <-predict(STAND1, STANDval)

STANDval$predDq <-pred
STANDval

shapiro.test(STANDval$Dq)

shapiro.test(STANDval$predDq)

sd(STAND$Dq)

library(DescTools)
ZTest(x=STANDval$Dq, y=pred, paired=FALSE, mu=0, sd_pop=6.760101, conf.level=0.95) #when the P-value is not significant it means the model is ideal 

RAINFOREST <- read.csv("Rainforest.csv")
RAINFOREST

attach(RAINFOREST)
names(RAINFOREST)

MODEL <-lm(d~.,     data=RAINFOREST)
summary(MODEL)
conflicts(MODEL)
plot(MODEL)


library(caret)
library(leaps)
library(MASS)  
library(randomForest)
library(tidyverse)
library(olsrr)
Model2 <-stepAIC(MODEL, direction = "forward")
summary(Model2)

test<-ols_step_all_possible(MODEL)
test
plot(test)


library(ggplot2)

