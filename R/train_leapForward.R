#'@title leapForward in a function
#'@description train(method=leapForward) dont work in Vignette
#'@param formula Formula
#'@param data Data
#'@export
train_leapForward<-function(formula, data){
  
  model_leap_forward <- train(formula ,data , method = "leapForward")
  
  summary_model_leap_forward <-summary(model_leap_forward)
  
  lasta <- list(leapForward = model_leap_forward, 
                model =summary_model_leap_forward$which)
}