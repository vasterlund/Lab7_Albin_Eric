library(tidyverse)
library(nycflights13)
library(plotly)
library(MASS)
library(mlbench)
library(caret)
library(tidyverse)
library(car)
install.packages(car)




##### Skapa datamaterialet #####
data(iris)
data<-iris
formula<-Petal.Length~Sepal.Width+Sepal.Length
lambda<-c(0.5,0.2)
values<-data.frame(Sepal.Width=c(1,2,1,2,3,4),Sepal.Length=c(1,2,1,2,3,4))
values<-data.frame(Sepal.Length=c(1,2,1,2,3,4),Sepal.Width=c(1,2,1,2,3,4))
##### Kolla om funktionerna funkar #####
lm.ridge(formula,data,lambda = lambda)$coef

ridgereg$new(formula, data=data,lambda)
ridgereg$new(formula, data=data,lambda)$print()
ridgereg$new(formula, data=data,lambda)$predict(values)
ridgereg$new(formula, data=data,lambda)$coef()
##### RMD #####
library(mlbench)
library(caret)
library(Lab7SpaghettiBolognese)
data("BostonHousing")
#######
set.seed(12345)
training <- createDataPartition(BostonHousing$tax,p = 0.7)
train_data <- BostonHousing[training$Resample1, ]
test_data <- BostonHousing[-training$Resample1, ]
######
model_lm <- train(tax ~ .  ,data = train_data, method = "lm")
sol <- summary(model_lm)
sol$coefficients
######
model_leap_forward <- train(tax ~ .  ,data = train_data, method = "leapForward")

hej <-summary(model_leap_forward)
hej$which

as.data.frame(hej$which)[nrow(hej$which),]
names(as.data.frame(hej$which))[as.data.frame(hej$which)[nrow(hej$which),][1,]]
#######

model_lm <- train(tax ~ zn + indus + rad + medv  ,data = train_data, method = "lm")

summary(model_lm)
#######
ridgereg$new(tax ~ zn + indus + rad + medv, data=train_data ,lambda=seq(0.1,0.5,0.1))$print()





