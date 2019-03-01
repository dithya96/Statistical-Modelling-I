#########################################################################################
## Homework Assignment 2 - Question 1
## Dithya Sridharan
## Class Number : 48
## Edited: October 05, 2018
#########################################################################################

rm(list = ls())

##Load the libraries 

install.packages("pls")
library(pls)
install.packages('ISLR')
install.packages("glmnet")
library(ISLR)
library(tidyverse)
library(broom)
library(glmnet)
data(College)
set.seed(1)


##Splitting the data set into training and test data

smp_size <- floor(0.75 * nrow(College))
train_ind <- sample(seq_len(nrow(College)), size = smp_size)
train <- College[train_ind, ]
test <- College[-train_ind, ]


##Fitting a linear model using least squares

linmod <- lm(Apps ~., data=train)
prediction <- predict(linmod, test)
LE <- mean((prediction - test$Apps)^2)


##Ridge regression model on the training set and error analysis

trainmat <- model.matrix(Apps ~ ., data =train)
testmat <- model.matrix(Apps ~ ., data =test)
ridgereg <- glmnet(trainmat, train$Apps, alpha = 0)
cv.out <- cv.glmnet(trainmat, train$Apps, alpha = 0)
plot(cv.out)
names(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridgeprediction <- predict(ridgereg, s = bestlam, newx = testmat)
mean((ridgeprediction - test$Apps)^2)


##LASSO model on training set and error analysis


lassoreg <- glmnet(trainmat, train$Apps, alpha = 1)
cv.las <- cv.glmnet(trainmat, train$Apps, alpha = 1)
plot(cv.las)
names(cv.las)
bestlamlas <- cv.las$lambda.min
bestlamlas
lassoprediction <- predict(lassoreg, s = bestlamlas, newx = testmat)
mean((lassoprediction - test$Apps)^2)

predict(lassoreg, s = bestlamlas, type = "coefficients")


##PCR fit

pcrfit = pcr(Apps ~., data = train, scale = TRUE, validation = "CV")
validationplot(pcrfit, val.type = "MSEP")
pcrpred <- predict(pcrfit, test, ncomp = 10)
mean((pcrpred - test$Apps)^2)


##PLS fit

plsfit = plsr(Apps ~., data = train, scale = TRUE, validation = "CV")
validationplot(plsfit, val.type = "MSEP")
plspred <- predict(plsfit, test, ncomp = 10)
mean((plspred - test$Apps)^2)


##R^2 for each model to compare errors

avgtest <- mean(test$Apps)
lm_r2 <- 1 - mean((prediction - test$Apps)^2) / mean((avgtest - test$Apps)^2)
ridge_r2 <- 1 - mean((ridgeprediction - test$Apps)^2) / mean((avgtest - test$Apps)^2)
lasso_r2 <- 1 - mean((lassoprediction - test$Apps)^2) / mean((avgtest - test$Apps)^2)
pcr_r2 <- 1 - mean((pcrpred - test$Apps)^2) / mean((avgtest - test$Apps)^2)
pls_r2 <- 1 - mean((plspred - test$Apps)^2) / mean((avgtest - test$Apps)^2)


