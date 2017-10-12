context("ridgereg")
data("iris")


test_that("class is correct", {
  linreg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=c(0.1,0.5))
  
  expect_true(class(linreg_mod)[1] == "ridgereg")
})


test_that("print() method works", {
  linreg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=c(0.1,0.5))
  
  expect_output(linreg_mod$print(),"0.1       3.7555     -0.58318       1.46935")
  expect_output(linreg_mod$print(),"0.5      3.74551     -0.58207       1.46555")
})

test_that("Gets the same coef as lm.ridge", {
  linreg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=c(0.1,0.5))
  jamfora<-MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=c(0.1,0.5))$coef[,1]
  round(linreg_mod$coef()[[1]][2:length(linreg_mod$coef()[[1]])],2)==round(jamfora,2)
  
  expect_equal(round(jamfora,1),round(linreg_mod$coef()[[1]][2:length(linreg_mod$coef()[[1]])],1))
})


