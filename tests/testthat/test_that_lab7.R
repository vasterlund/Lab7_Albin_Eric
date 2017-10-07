context("ridgereg")

data("iris")

Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)


test_that("resid() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda =0.5)
  print(ridgereg_mod)
  
  lm.ridge(formula=Petal.Length~Sepal.Width+Sepal.Length,data=iris,lambda =0.5)
  
  expect_equal(round(unname(linreg_mod$resid()[c(7,13,27)]),2), c(0.31, -0.58, -0.20))
})
