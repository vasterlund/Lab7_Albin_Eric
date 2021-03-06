#'@title Train function
#'@description A train function for the model ridgereg
#'@param lambda Your lambda values. A numeric vector
#'@param cross_val False or True
#'@param fold_count If cross_val=TRUE. Then how many values will it fold.
#'@param repeat_count If cross_val=TRUE. Then how many times will it be cross-validation.
#'@param data Your data. If data=NULL the data BostonHousing is used
#'@param formula Your formula. If formula=NULL the formula tax ~ zn + indus + rad + medv is used
#'@param set_seed Your seed. If set_seed=NULL then is no seed used.
#'@param p The proportion of trainging and test if BostonHousing data is used
#'@param normalize Normalize or not.
#'
#'@examples
#'#library(Lab7SpaghettiBolognese)
#'#ridgereg_train(seq(0,100,10))
#'
#' @export 
ridgereg_train<-function(lambda=0,cross_val=FALSE,fold_count=10,repeat_count=10,data=NULL,formula=NULL,set_seed=12345,p=0.7,normalize=TRUE){
  
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
    out <- ridgereg$new(.outcome ~ ., data=dat ,lambda = param$lambda, normalize=normalize, ...)
    
    out
  }
  
  ridgeregg$predict <- function (modelFit, newdata, submodels = NULL) {
    if (!is.data.frame(newdata)) 
      newdata <- as.data.frame(newdata)
    newdata <- scale(newdata)
    modelFit$predict(newdata)
  }
  
  if(is.null(data)){
    if(!is.null(set_seed)){
      set.seed(set_seed)
    }
    data(BostonHousing)
    training <- caret::createDataPartition(BostonHousing$tax,p = p)
    data <- BostonHousing[training$Resample1, ]
  }
  
  
  if(is.null(formula)){
    formula<-tax ~ zn + indus + rad + medv
  }
  
  
  if(cross_val==FALSE){
    return(caret::train(formula  ,data = data, ridgeregg))
  }
  else if (cross_val==TRUE){
    fitControl <- caret::trainControl(## 10-fold CV
      method = "repeatedcv",
      number = fold_count,
      ## repeated ten times
      repeats = repeat_count)
    return(caret::train(formula ,data = data, method = ridgeregg,trControl = fitControl))
    
    
  }
  
  

}
