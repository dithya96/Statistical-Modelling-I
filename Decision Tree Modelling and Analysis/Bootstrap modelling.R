#########################################################################################
## Homework Assignment 4 - Question 1
## Dithya Sridharan
## Class Number : 48
## Edited: Novemeber 24, 2018
#########################################################################################

#########################################################################
##Load the libraries 
#########################################################################

install.packages("pls")
install.packages('ElemStatLearn')
install.packages("glmnet")
install.packages("caret")
install.packages("boot")
library(boot)
library(caret)
library(ElemStatLearn)
library(tidyverse)
library(broom)
library(glmnet)
library(pls)
library(leaps)
library(class)
set.seed(1)


##################################################################
##Splitting the data set into training and test data
##################################################################

smp_size <- floor(0.80 * nrow(prostate))
train_ind <- sample(seq_len(nrow(prostate)), size = smp_size)
train <- prostate[train_ind, ]
test <- prostate[-train_ind, ]
head(train)


##################################################################
##Best Subset Selection of predictors
##################################################################

best_subset <- regsubsets(train~., data = train,method = 'exhaustive')
my_summary <- summary(best_subset)
my_summary
my_summary$cp #1.481855 is the least
which.min(my_summary$cp) #3 is the least
#age,gleason,pgg45

#TEST MSE 

prostate.glm <- glm( lpsa ~ .+age + gleason + pgg45, data=train )
lm.y <- predict(prostate.glm, test)
mean((lm.y - test$lpsa)^2) #0.5048945

my_summary$bic #4.93734 is the least
which.min(my_summary$bic) #1 is the least
#age

prostate.glmb <- glm( lpsa ~ .+age, data=train )
lm.yb <- predict(prostate.glmb, test)
mean((lm.yb - test$lpsa)^2) #0.5048945



###################################################################
##Five fold cross validation
###################################################################

cv.error.k5= rep(0,5)
for (i in 1:5){
  prostate.glm <- glm( lpsa ~ ., data=train )
  cv.error.k5[i] = cv.glm(train,prostate.glm, K=5)$delta[1]
}
cv.error.k5 #1 is best

plot(cv.error.k5, xlab = "Number of variables",ylab = "5-fold cross validation error")

###################################################################
##Ten fold cross validation
###################################################################


cv.error.k= rep(0,10)
for (i in 1:10){
  prostate.glm <- glm( lpsa ~ .+age, data=train )  
  cv.error.k[i] = cv.glm(train,prostate.glm, K=10)$delta[1]
}
cv.error.k #1 is best
plot(cv.error.k, xlab = "Number of variables",ylab = "10-fold cross validation error")

###################################################################
##Bootstrap
###################################################################




install.packages("bootstrap")
library(bootstrap)

beta.fit <- function(X,Y){
  lsfit(X,Y)	
}

beta.predict <- function(fit, X){
  cbind(1,X)%*%fit$coef
}

sq.error <- function(Y,Yhat){
  (Y-Yhat)^2
}
selectboot = my_summary$outmat
boot_err = c()

for (i in 1:8){
  # Pull out the model
  temp <- which(select[i,] == "*")
  
  res <- bootpred(prostate[,temp], prostate$train, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
  boot_err <- c(boot_err, res[[3]])
  
}
boot_err
plot(boot_err, xlab = "Number of variables",ylab = ".632 bootstrap")
#3 is best




























