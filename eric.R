

data(iris)

head(iris)



Y <- iris$Sepal.Length

X <- data.frame(x1 = iris$Sepal.Width, x2 = iris$Petal.Length)

data_test <- cbind(Y,X)


lambda <- 0.7
  
formula <- Y ~ x1 + x2
  
data <- data_test


x<-model.matrix(formula,data)
x[,2:ncol(x)]<-apply(x[,2:ncol(x)],2,function(a) (a-mean(a))/sd(a))
y_namn<-all.vars(formula)[1]
y<-as.matrix(data[,names(data)==y_namn])
data<-data.frame(cbind(x[,2:ncol(x)],y))
names(data)[ncol(data)]<-y_namn



###Model
X_ind <- model.matrix(formula, data)
# X_ind2 <- scale(X_ind[,2:ncol(X_ind)])
# X_ind <- cbind(X_ind[,1], X_ind2)

hej <- qr(X_ind)
R <- qr.R(hej) 
Q <- hej.Q(hej)

#### QR delen ####


forst_QR <- t(R) %*% R
lambda_matr_QR <- matrix(0, nrow(forst_QR), ncol(forst_QR))
diag(lambda_matr_QR) <- lambda
B_ridge_QR <- solve(forst_QR+lambda_matr_QR) %*% t(X_ind) %*% Y



forst <- t(X_ind) %*% X_ind
lambda_matr <- matrix(0, nrow(forst), ncol(forst))
diag(lambda_matr) <- lambda

B_ridge <- solve(forst+lambda_matr) %*% t(X_ind) %*% Y
  
###
B_ridge
lm.ridge(formula,data,lambda = lambda)

ridgereg$new(formula, data=data, seq(0,0.5,0.1),normalize=FALSE)$print()


lm.ridge(formula, data=data,lambda = seq(0,0.5,0.1))






lm.ridge(formula, data,lambda =lambda)



  
df <- nrow(data) - length(B_ridge)

e <- Y - X_ind %*% B_ridge

Var_e <- (t(e) %*% e)/df
Var_e

lambda <- c(0.3,0.5, 0.6, 0.7)

Var_test <- c()
for(i in 1:4){
  
  ###Model
  X_ind <- model.matrix(formula, data)
  
  forst <- t(X_ind) %*% X_ind
  lambda_matr <- matrix(0, nrow(forst), ncol(forst))
  diag(lambda_matr) <- lambda[i]
  
  B_ridge <- solve(forst+lambda_matr) %*% t(X_ind) %*% Y
  
  ###
  
  df <- nrow(data) - length(B_ridge)
  
  e <- Y - X_ind %*% B_ridge
  
  Var_e <- (t(e) %*% e)/df
  Var_test[i] <- Var_e
  
}

Var_test <- 1
n <- 1


  
  
  
  
  
library(nycflights13)

data(flights)
data(airports)




visualize_airport_delays <- function(){
  
  data(flights)
  data(airports)
  
  require(tidyverse)
  
  data <- inner_join(airports, flights, by = c("faa" = "dest")) 
  
  #doin some dplyr stuff

  vect <- data %>%
    group_by(faa) %>%
    summarize(Mean = mean(arr_delay,na.rm = TRUE))
  
  coordin <- data %>%
    group_by(faa) %>%
    summarize(Coordinates = paste0("lat = ", lat[1],", lon = ", lon[1], collapse = " "))
  
    
  
  graph_data <- data.frame(vect, coordin[,2])
  
  ###PLOTTA
  require(ggplot2)
  require(plotly) 
  p <- ggplot(graph_data, aes(x = faa, y = Mean, label = Coordinates)) + 
    geom_point() + labs(x = "Airports") + theme_bw() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
    
  suppressMessages(ggplotly(p))
  
  
}

visualize_airport_delays()


types <- list()




################################


data(iris)

data(BostonHousing)

ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=seq(0,1,0.1))$predict(c(0.2, 0.5))



hej <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=c(0.2,0.5))$coef()

t(hej[[1]])

training <- BostonHousing[1:(506*0.7),]
test <- BostonHousing[(nrow(training)+1):nrow(BostonHousing),]







training <- createDataPartition(BostonHousing$tax,p = 0.7)

train_data <- BostonHousing[training$Resample1, ]
test_data <- BostonHousing[-training$Resample1, ]


#Models
model_lm <- train(tax ~ .  ,data = train_data, method = "lm")
model_leap_forward <- train(tax ~ .  ,data = train_data, method = "leapForward")

hej <- summary(model_leap_forward)

train_data[,-10][hej$which[4,-1]]

model_leap_forward$modelInfo


ridgereg$new(tax ~ zn + indus + rad + medv, data=train_data ,lambda=seq(0.1,0.5,0.1))$print()




colnames(train_data)



################

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
  
  
  ridgeregg
  
}

####################





getModelInfo(model = "lm", regex = FALSE)


interesting <- ridgereg_train(0.5)

train(tax ~ zn + indus + rad + medv  ,data = train_data, method = ridgeregg)




hej$finalModel$print()

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)



train(tax ~ zn + indus + rad + medv  ,data = train_data, method = ridgeregg, trControl = fitControl)






ridgereg$new(tax ~ zn + indus + rad + medv, data=train_data)






