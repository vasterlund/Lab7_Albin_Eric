#'@title Extra
#'@description Extra
#'@param lambda lambda
#'
#' @export 
ridgereg_train<-function(lambda=0){
  
  ridgeregg  <- list(type = "Regression", 
                     library = "Lab7SpaghettiBolognese",
                     loop = NULL,
                     prob = NULL)
  
  ridgeregg$parameters <- data.frame(parameter = "lambda",
                                     class = "numeric",
                                     label = "Ridge Regression")
  
  
  ridgeregg$grid <- function (x, y, len = NULL, search = "grid"){
    data.frame(lambda = lambda)
  } 
  
  ridgeregg$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
    dat <- if (is.data.frame(x)) 
      x
    else as.data.frame(x)
    dat$.outcome <- y
    out <- ridgereg$new(.outcome ~ ., data=dat ,lambda = param$lambda, ...)
    
    out
  }
  
  ridgeregg$predict <- function (modelFit, newdata, submodels = NULL) {
    if (!is.data.frame(newdata)) 
      newdata <- as.data.frame(newdata)
    modelFit$predict(newdata)
  }
  
  set.seed(12345)
  data(BostonHousing)
  training <- createDataPartition(BostonHousing$tax,p = 0.7)
  train_data <- BostonHousing[training$Resample1, ]
  test_data <- BostonHousing[-training$Resample1, ]
  
  
  train(tax ~ zn + indus + rad + medv  ,data = train_data, ridgeregg)
}
