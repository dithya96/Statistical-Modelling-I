#########################################################################################
## Homework Assignment 2 - Question 3
## Dithya Sridharan
## Class Number : 48
## Edited: October 05, 2018
#########################################################################################

set.seed(11)


##Load the libraries 

install.packages("pls")
library(pls)
install.packages('ISLR')
install.packages("glmnet")
library(ISLR)
library(tidyverse)
library(broom)
library(glmnet)

## Generating a data set with 20 variables and 1000 observations and y=bx+e

x <- matrix(rnorm(1000 * 20), 1000, 20)
b <- rnorm(20)
b[2] <- 0 #some values of Beta are assigned 0 
b[5] <- 0
b[7] <- 0
b[9] <- 0
b[15]<- 0
e <- rnorm(1000)
y <- x %*% b + e
object.size(x)
object.size(b)

## Splitting data into training and test set

train <- sample(seq(1000), 100, replace = FALSE)
test <- -train
x.train <- x[train, ]
x.test <- x[test, ]
y.train <- y[train]
y.test <- y[test]

## Best Subset Selection and plot Train MSE

traindata <- data.frame(y = y.train, x = x.train)
regfit.full <- regsubsets(y ~ ., data = traindata, nvmax = 20)
trainmat <- model.matrix(y ~ ., data = traindata, nvmax = 20)
val.errors <- rep(NA, 20)
for (i in 1:20) {
  coefi <- coef(regfit.full, id = i)
  pred <- trainmat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((pred - y.train)^2)
}
plot(val.errors, xlab = "Number of predictors", ylab = "Training MSE")

## Plotting Test MSE

testdata <- data.frame(y = y.test, x = x.test)
testmat <- model.matrix(y ~ ., data = testdata, nvmax = 20)
val.errorst <- rep(NA, 20)
for (i in 1:20) {
  coefi <- coef(regfit.full, id = i)
  pred <- testmat[, names(coefi)] %*% coefi
  val.errorst[i] <- mean((pred - y.test)^2)
}
plot(val.errorst, xlab = "Number of predictors", ylab = "Test MSE", pch = 19, type = "b")

# Model where test set MSE is minimum
which.min(val.errorst)

# Minimum test mse model compared to true model
coef(regfit.full, which.min(val.errorst))