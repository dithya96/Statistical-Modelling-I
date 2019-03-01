#########################################################################################
## Homework Assignment 3 - Question 1
## Dithya Sridharan
## Class Number : 48
## Edited: October 05, 2018
#########################################################################################

#########################################################################
##Load the libraries 
#########################################################################

install.packages("pls")
install.packages('ISLR')
install.packages("glmnet")
install.packages("MASS")
install.packages("caret")
library(caret)
library(ISLR)
library(tidyverse)
library(broom)
library(glmnet)
library(MASS)
library(pls)
library(leaps)
attach(Boston)
library(class)
set.seed(1)


##################################################################
##Response variable when crim>median
##################################################################

Boston <- mutate(Boston, resp = ifelse(Boston$crim > median(Boston$crim),1,0))
data_1 = data.frame(Boston,Boston$resp)
Boston$resp <-factor(Boston$resp)
Boston$crim <- NULL
head(Boston)


##################################################################
##Splitting the data set into training and test data
##################################################################

smp_size <- floor(0.75 * nrow(Boston))
train_ind <- sample(seq_len(nrow(Boston)), size = smp_size)
train <- Boston[train_ind, ]
test <- Boston[-train_ind, ]
head(train)


##################################################################
##Subset Selection of predictors
##################################################################

fit_data <- regsubsets(resp~., data = train,method = 'exhaustive',nvmax = 14)
my_fit_data <- summary(fit_data)
my_fit_data
my_fit_data$cp
which.min(my_fit_data$cp) #Gives 5 variables
#nox, medv, rad, zn, age
which.min(my_fit_data$bic) #Gives 4 variables
#nox, rad, age, medv
which.min(my_fit_data$rss) #Gives 13 variables
#nox, zn, indus, chas, age, dis, rad, ptratio, black, lstat, medv,rm,tax



###################################################################
##Making subsets
###################################################################

myvars1 <- c("nox", "medv", "rad", "zn", "age")
subset1 <- Boston[myvars1]
myvars2 <- c("nox", "medv", "rad", "age")
subset2 <- Boston[myvars2]
myvars3 <- c("nox", "medv", "rad", "zn", "age", "chas", "dis", "ptratio", "lstat", "rm", "tax", "indus", "black")
subset3 <- Boston[myvars1]


###################################################################
##Logistic Regression
###################################################################

logistic_1 = glm(resp ~ .-chas-dis-ptratio-lstat-rm-tax-indus-black, data = train, family = binomial)
logistic_1_pred = predict(logistic_1, test, type = 'response')
pred_bool1=rep(0,length(logistic_1_pred))
pred_bool1[logistic_1_pred>0.5] = 1
mean(pred_bool1 != test$resp)  #0.1417323
plot(pred_bool1)

logistic_2 = glm(resp ~ .-chas-dis-ptratio-lstat-rm-tax-indus-black-zn, data = train, family = binomial)
logistic_2_pred = predict(logistic_2, test, type = 'response')
pred_bool2=rep(0,length(logistic_2_pred))
pred_bool2[logistic_2_pred>0.5] = 1
mean(pred_bool2 != test$resp)  #0.1338583
plot(pred_bool2)

logistic_3 = glm(resp ~ ., data = train, family = binomial)
logistic_3_pred = predict(logistic_3, test,type = 'response')
pred_bool3=rep(0,length(logistic_3_pred))
pred_bool3[logistic_3_pred>0.5] = 1
mean(pred_bool3 != test$resp)  #0.07874016
plot(pred_bool3)


######################################################################################
##KNN
######################################################################################

train_mod1 = train[ ,-which(names(train) %in% c("resp","chas","dis","ptratio","lstat","rm","tax","indus","black"))]
test_mod1 = test[ ,-which(names(test) %in% c("resp","chas","dis","ptratio","lstat","rm","tax","indus","black"))]
knn.pred = knn(train_mod1, test_mod1, train$resp, k = 10)
mean(knn.pred != test$resp) #0.1811024
plot(knn.pred)

train_mod2 = train[ ,-which(names(train) %in% c("resp","chas","dis","ptratio","lstat","rm","tax","indus","black","zn"))]
test_mod2 = test[ ,-which(names(test) %in% c("resp","chas","dis","ptratio","lstat","rm","tax","indus","black","zn"))]
knn.pred2 = knn(train_mod2, test_mod2, train$resp, k = 10)
mean(knn.pred2 != test$resp) #0.2047244
plot(knn.pred2)

train_mod3 = train[ ,-which(names(train) %in% c("resp"))]
test_mod3 = test[ ,-which(names(test) %in% c("resp"))]
knn.pred3 = knn(train_mod3, test_mod3, train$resp, k = 10)
mean(knn.pred3 != test$resp) #0.1102362
plot(knn.pred3)


#####################################################################################
##LDA
#####################################################################################

lda_1 = lda(resp ~ .-chas-dis-ptratio-lstat-rm-tax-indus-black, data = train)
lda_pred1=predict(lda_1,test)
mean(lda_pred1$class != test$resp) #0.1574803
plot(lda_1)

lda_2 = lda(resp ~ .-chas-dis-ptratio-lstat-rm-tax-indus-black-zn, data = train)
lda_pred2=predict(lda_2,test)
mean(lda_pred2$class != test$resp) #0.1574803
plot(lda_2)

lda_3 = lda(resp ~ ., data = train)
lda_pred3=predict(lda_3,test)
mean(lda_pred3$class != test$resp) #0.1653543
plot(lda_3)






















