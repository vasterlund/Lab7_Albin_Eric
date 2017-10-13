## ----echo=FALSE, message=FALSE-------------------------------------------
library(mlbench)
library(caret)
library(Lab7SpaghettiBolognese)
data("BostonHousing")

## ------------------------------------------------------------------------
set.seed(12345)
training <- createDataPartition(BostonHousing$tax,p = 0.7)

train_data <- BostonHousing[training$Resample1, ]
test_data <- BostonHousing[-training$Resample1, ]

## ------------------------------------------------------------------------
model_lm <- train(tax ~ .  ,data = train_data, method = "lm")
sol <- summary(model_lm)
sol

## ------------------------------------------------------------------------
model_leap_forward <- train(tax ~ .  ,data = train_data, method = "leapForward")

summary_model_leap_forward <-summary(model_leap_forward)
summary_model_leap_forward$which

model_lm <- train(tax ~ zn + indus + rad + medv  ,data = train_data, method = "lm")
summary(model_lm)



## ------------------------------------------------------------------------
model_leap_forward


## ------------------------------------------------------------------------
# List of our own model
ridgereg_train(c(3,4,6,8,5))


