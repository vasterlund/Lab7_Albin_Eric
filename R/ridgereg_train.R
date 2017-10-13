#'@title Extra
#'@description Extra
#'@param lambda lambda
#'@param data If you dont put in any data 
#'
#' @export 
ridgereg_train<-function(lambda=0,cross_val=FALSE,fold_count=10,data=NULL,formula=NULL,set_seed=12345,p=0.7){
  
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
  
  if(is.null(data)){
    if(!is.null(set_seed)){
      set.seed(set_seed)
    }
    data(BostonHousing)
    training <- createDataPartition(BostonHousing$tax,p = p)
    data <- BostonHousing[training$Resample1, ]
  }
  
  
  if(is.null(formula)){
    formula<-tax ~ zn + indus + rad + medv
  }
  
  
  if(cross_val==FALSE){
    return(train(formula  ,data = train_data, ridgeregg))
  }
  else if (cross_val==TRUE){
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = fold_count,
      ## repeated ten times
      repeats = fold_count)
    return(train(formula ,data = train_data, method = ridgeregg,trControl = fitControl))
    
    
  }
  
  

}
