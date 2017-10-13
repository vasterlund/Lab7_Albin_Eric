## ----echo=FALSE, message=FALSE-------------------------------------------
library(mlbench)
library(caret)
library(Lab7SpaghettiBolognese)
data("BostonHousing")

## ------------------------------------------------------------------------
training <- createDataPartition(BostonHousing$tax,p = 0.7)

train_data <- BostonHousing[training$Resample1, ]
test_data <- BostonHousing[-training$Resample1, ]

## ------------------------------------------------------------------------
model_lm <- train(tax ~ .  ,data = train_data, method = "lm")
sol <- summary(model_lm)
sol$coefficients

## ------------------------------------------------------------------------
model_leap_forward <- train(tax ~ .  ,data = train_data, method = "leapForward")

hej <-summary(model_leap_forward)
hej$which



## ------------------------------------------------------------------------
model_lm <- train(tax ~ zn + indus + rad + medv  ,data = train_data, method = "lm")
summary(model_lm)

## ------------------------------------------------------------------------
ridgereg$new(tax ~ zn + indus + rad + medv, data=train_data ,lambda=seq(0.1,0.5,0.1))$print()

