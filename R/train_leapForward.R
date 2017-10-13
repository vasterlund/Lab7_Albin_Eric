#'@title leapForward in a function
#'@description train(method=leapForward) dont work in Vignette
train_leapForward<-function(){
  model_leap_forward <- train(tax ~ .  ,data = train_data, method = "leapForward")
  
  summary_model_leap_forward <-summary(model_leap_forward)
  summary_model_leap_forward$which
  
  model_lm <- train(tax ~ zn + indus + rad + medv  ,data = train_data, method = "lm")
  summary(model_lm)
}