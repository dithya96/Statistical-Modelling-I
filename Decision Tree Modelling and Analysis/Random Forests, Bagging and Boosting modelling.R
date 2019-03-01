#########################################################################################
## Homework Assignment 4 - Question 3
## Dithya Sridharan
## Class Number : 48
## Edited: Novemeber 24, 2018
#########################################################################################

#########################################################################
##Load the libraries 
#########################################################################

install.packages("pls")
install.packages('ISLR')
install.packages("glmnet")
install.packages("caret")
install.packages("boot")
install.packages("gbm")
library(gbm)
library(boot)
library(caret)
library(ISLR)
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

smp_size <- floor(0.80 * nrow(College))
train_ind <- sample(seq_len(nrow(College)), size = smp_size)
college_train <- College[train_ind, ]
college_test <- College[-train_ind, ]
head(college_train)

##################################################################
##Linear Regression and Least Squares
##################################################################

college_linreg <- lm(Apps ~., data=college_train)
pred_linreg <- predict(college_linreg, college_test)
mean((pred_linreg - college_test$Apps)^2)
avgtest <- mean(college_test$Apps)
linreg_r2 <- 1 - mean((pred_linreg - college_test$Apps)^2) / mean((avgtest - college_test$Apps)^2)
linreg_r2 #0.8926812
plot(pred_linreg)

##################################################################
##Logistic Regression
##################################################################

c2 <- college_train
ct <- college_test
c2 <- mutate(c2, resp = ifelse(c2$Apps > median(c2$Apps),1,0))
ct <- mutate(ct, resp = ifelse(ct$Apps > median(ct$Apps),1,0))
college_logreg = glm(resp ~.-Top25perc -F.Undergrad -P.Undergrad -Books -Personal -Terminal -S.F.Ratio -perc.alumni -Grad.Rate, data = c2, family = binomial)
pred_logreg = predict(college_logreg,ct, type = 'response')
#college_logreg = glm(resp ~., data = c2, family = binomial)
#pred_logreg = predict(college_logreg,ct, type = 'response')
pred_lr=rep(0,length(pred_logreg))
pred_lr[pred_logreg>0.5] = 1
mean(pred_lr != ct$resp) #0.1417323

avgt <- mean(ct$resp)
logreg_r2 <- 1 - mean((pred_logreg - ct$resp)^2) / mean((avgt - ct$resp)^2)
logreg_r2 #0.8717949
plot(pred_lr)


###################################################################
##Random forests
###################################################################

fit_rf = randomForest(Apps ~. , data=college_train)
#rating_count_tot rating_count_ver user_rating_ver prime_genre ipadSc_urls.num lang.num
importance(fit_rf)
varImpPlot(fit_rf)
rf.probs <- predict(fit_rf, newdata = college_test)

rf.pred <- ifelse(rf.probs > 0.5, 1, 0)
tab <- table(ct$resp, rf.pred)
1-sum(diag(tab))/sum(tab) #0.5

pred_rf <- predict(fit_rf, college_test)
mean((pred_rf - college_test$Apps)^2)
avg <- mean(college_test$Apps)
r2 <- 1 - mean((pred_rf - college_test$Apps)^2) / mean((avg - college_test$Apps)^2)
r2 #0.9135211

#############################################################################################
## Bagging
#############################################################################################


bag.fit <- randomForest(Apps ~ . , data = college_train, mtry = 6)
bag.probs <- predict(bag.fit, newdata = college_test)
bag.pred <- ifelse(bag.probs > 0.5, 1, 0)
tabb <- table(college_test$Apps, bag.pred)
1-sum(diag(tabb))/sum(tabb)

pred_bag <- predict(bag.fit, college_test)
mean((pred_bag - college_test$Apps)^2)
avg <- mean(college_test$Apps)
r2 <- 1 - mean((pred_bag - college_test$Apps)^2) / mean((avg - college_test$Apps)^2)
r2 #0.902788


###########################################################################################
##Boosting
##########################################################################################
install.packages("gbm")
library(gbm)
boost.fit <- gbm(Apps ~ . , data = college_train, n.trees = 5000)
boost.probs <- predict(boost.fit, newdata = college_test, n.trees = 5000)
boost.pred <- ifelse(boost.probs > 0.5, 1, 0)
taboost <- table(college_test$Apps, boost.pred)
1-sum(diag(taboost))/sum(taboost) #0.8935897

pred_boost <- predict(boost.fit, college_test, n.trees = 5000)
mean((boost.probs - college_test$Apps)^2)
avg <- mean(college_test$Apps)
r2 <- 1 - mean((boost.probs - college_test$Apps)^2) / mean((avg - college_test$Apps)^2)
r2 #0.8944636
