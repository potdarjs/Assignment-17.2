getwd()
setwd("D:/Data Analytics with Excel R and Tabaleu/sessions/R session 1")
gmdat<-read.csv("GMdata.csv",header = T)
class(gmdat)
names(gmdat)
View(gmdat) 
str(gmdat)   # understanding the variables
pairs(gmdat) # multicolinearity

# multiple regression model1
gmmod1<-lm(Price~.,data=gmdat)#MLR to forecast Price
gmmod1
summary(gmmod1)
gmmod1$residuals
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(gmmod1)

# eliminated below for model2
# cruise as t value is smaller than P value
gmmod2<-lm(Price~ Mileage+ Make + Liter + Cylinder + Sound + Leather, data = gmdat)
gmmod2
summary(gmmod2)
plot(gmmod2)


# eliminated below for model3
# Sound & Leather as coeeficients are not strong

gmmod3<-lm(Price~ Mileage+ Make + Liter + Cylinder, data = gmdat)
gmmod3
summary(gmmod3)
plot(gmmod3)


# eliminated below for model4
# Cylinder

gmmod4<-lm(Price~ Mileage+ Make + Liter, data = gmdat)
gmmod4
summary(gmmod4)
plot(gmmod4)


# catering to outliers

#1/sqrt(Price) lambda when -0.5
gmmod5 <-lm((1/sqrt(Price))~ Mileage+ Make + Liter + Cylinder ,data=gmdat)
gmmod5
summary(gmmod5)


#log(Price) lambda when -0.5
gmmod6 <- lm((log(Price))~ Mileage+ Make + Liter + Cylinder ,data=gmdat)
gmmod6
summary(gmmod6)


# compare models
anova(gmmod3, gmmod5, gmmod6)


# cross validation
library(lattice)
library(DAAG)
cv.lm(data = gmdat, gmmod1, m=3) # 3 fold cross validation
cv.lm(data = gmdat, gmmod2, m=3) # 3 fold cross validation
cv.lm(data = gmdat, gmmod3, m=3) # 3 fold cross validation
cv.lm(data = gmdat, gmmod4, m=3) # 3 fold cross validation
cv.lm(data = gmdat, gmmod5, m=3) # 3 fold cross validation
cv.lm(data = gmdat, gmmod6, m=3) # 3 fold cross validation

# Variable Selection
library(MASS)
step <- stepAIC(gmmod1, direction = "both")
step$anova # display results

# All Subsets Regression
library(leaps)
attach(gmdat)
leaps <- regsubsets(Price ~ ., data = gmdat, nbest = 1)
summary(leaps)

# plot a table of models showing variable of each model
# models are ordered by selection statistic

plot(leaps, scale ="r2")

# plot statistic by subset size 
library(car)
subsets(leaps, statistic="rsq")
## S3 method for class 'regsubsets'
plot(leaps, scale=c("bic", "Cp", "adjr2", "r2"),
     col=gray(seq(0, 0.9, length = 100)))

plot(leaps, scale=c("Cp"),
     col=gray(seq(0, 0.9, length = 100)))

plot(leaps, scale=c("adjr2"),
     col=gray(seq(0, 0.9, length = 100)))

plot(leaps, scale=c("r2"),
     col=gray(seq(0, 0.9, length = 100)))
