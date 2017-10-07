

data(iris)

head(iris)



Y <- iris$Sepal.Length

X <- data.frame(x1 = iris$Sepal.Width, x2 = iris$Petal.Length)

data_test <- cbind(Y,X)


lambda <- 0.7
  
formula <- Y ~ x1 + x2
  
data <- data_test


###Model
X_ind <- model.matrix(formula, data)

forst <- t(X_ind) %*% X_ind
lambda_matr <- matrix(0, nrow(forst), ncol(forst))
diag(lambda_matr) <- lambda

B_ridge <- solve(forst+lambda_matr) %*% t(X_ind) %*% Y
  
###
  
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
while(Var_test[n]- )


  
  
  
  
  
library(nycflights13)

data(flights)
data(airports)




visualize_airport_delays <- function(){
  
  data <- inner_join(airports, flights, by = c("faa" = "dest")) 
  
  #Takes the mean 
  joinedData <- sapply(1:length(unique(data$faa)), function(x){
    temp <- filter(data, data$faa == unique(data$faa)[x])
    mean(temp$arr_delay,na.rm = TRUE)
    
  })
  
  unique(data$faa)
  ###PLOTTA
  data
  
  
  
}








data <- filter(.data = flights, origin == airports$faa[85])



mtcars %>% filter_(~cyl==carb)


flights$arr_delay
flights$origin
airports$faa
airports$lat
airports$lon