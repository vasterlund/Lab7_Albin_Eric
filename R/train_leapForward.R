#'@title leapForward in a function
#'@description train(method=leapForward) dont work in Vignette
#'@export
train_leapForward<-function(){
  
  model_leap_forward <- train(tax ~ .  ,data = train_data, method = "leapForward")
  
  summary_model_leap_forward <-summary(model_leap_forward)
  
  lasta <- list(leapForward = model_leap_forward, 
                model =summary_model_leap_forward$which)
}