% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ridgereg.R
\docType{class}
\name{ridgereg-class}
\alias{ridgereg-class}
\alias{ridgereg}
\title{Do you want to make a Ridge Regression?}
\description{
This function make a "Reference Class" with class=ridgereg for a Ridge Regression
}
\section{Fields}{

\describe{
\item{\code{formula}}{The formula for the model}

\item{\code{data}}{A object of class data.frame}

\item{\code{lambda}}{The lambda value}
}}

\section{Methods}{

\describe{
\item{\code{coef()}}{Give you the coef values as a numeric vector}

\item{\code{predict(values = NULL)}}{Give you the fited values if the input is NULL and 
the predicted values if you have some new input}

\item{\code{print()}}{Give you a nice view of the calculation}
}}

\examples{
data(iris)
ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,0.5)$print()
ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,0.5)$predict()
ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,0.5)$coef()
}
