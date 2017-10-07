

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



