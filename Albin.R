library(tidyverse)
library(nycflights13)
library(plotly)
library(MASS)
library(mlbench)
library(caret)
library(tidyverse)
install.packages("tidyverse")





##### Skapa datamaterialet
data(iris)
data<-iris
formula<-Petal.Length~Sepal.Width+Sepal.Length
lambda<-c(0.5,0.2)
values<-data.frame(Sepal.Width=c(1,2,1,2,3,4),Sepal.Length=c(1,2,1,2,3,4))
values<-data.frame(Sepal.Length=c(1,2,1,2,3,4),Sepal.Width=c(1,2,1,2,3,4))
##### Kolla om funktionerna funkar
lm.ridge(formula,data,lambda = lambda)$coef

ridgereg$new(formula, data=data,lambda)
ridgereg$new(formula, data=data,lambda)$print()
ridgereg$new(formula, data=data,lambda)$predict(values)
ridgereg$new(formula, data=data,lambda)$coef()
#####





