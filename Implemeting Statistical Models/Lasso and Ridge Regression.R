#########################################################################################
## Homework Assignment 2 - Question 2
## Dithya Sridharan
## Class Number : 48
## Edited: October 05, 2018
#########################################################################################

set.seed(11)


##Load the libraries 

install.packages("pls")
library(pls)
install.packages("e1071")
library(e1071)
install.packages("caret")
library(caret)
install.packages('ISLR')
install.packages("glmnet")
library(ISLR)
library(tidyverse)
library(broom)
library(glmnet)
library(leaps)

##Read the data files

data1 <- read.delim("E:\\ticdata2000.txt", sep = "\t", header= FALSE)
data2 <- read.delim("E:\\ticeval2000.txt", sep = "\t", header= FALSE)
test_trgt <- read.delim("E:\\tictgts2000.txt", sep = "\t", header = FALSE)

data_train<-data.frame(data1)
head(data_train)
data_test<-data.frame(data2)
head(data_test)
data_test_trgt<-data.frame(test_trgt)
head(data_test_trgt)

new_test <- cbind(rep(1,length(data_test[,1])), data_test)
colnames(new_test) <- c("(Intercept)",colnames(data_test))

##OLS estimate

olsfit = lm(V86~., data=data_train)
lm.y <- predict(olsfit, data_test)
mean((lm.y - data_test_trgt$V1)^2)
which.min(lm.y)
lm.y <- round(lm.y)
lm.y <- as.numeric(lm.y)
lm.y <- as.factor(lm.y)
head(data_test_trgt)
trgt <- data_test_trgt
trgt$V1 <- as.factor(trgt$V1)
regconfusion <- confusionMatrix(lm.y, trgt$V1)
accuracyReg <- regconfusion$overall[1]
accuracyReg


## Forward Selection

regfit.for <- regsubsets(V86~., data = data_train, nbest = 1, nvmax = 86, method = 'forward', really.big = FALSE)
my_for <- summary(regfit.for)
my_for
which.min(my_for)
# V10,V18,V21,V43, V44,V47, V59,V82, V83 V85 are the best subsets
par(mfrow = c(2,2))
plot(my_for$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(my_for$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")

plot(my_for$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(my_for$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")
summary(regfit.for)$outmat[3,]

formod = lm(V86~ V10 + V18 + V21 + V43 + V44 + V47 + V59 + V82 + V83 + V85, data=data_train)
lm.for <- predict(formod, data_test)
mean((lm.for - data_test_trgt$V1)^2)
which.min(lm.for)
lm.for <- round(lm.for)
lm.for <- as.numeric(lm.for)
lm.for <- as.factor(lm.for)
lm.yf <- predict.lm(formod, data_test)
lm.yf <- round(lm.yf)
lm.yf <- as.numeric(lm.yf)
lm.yf <- as.factor(lm.yf)
regconfusionf <- confusionMatrix(lm.yf, trgt$V1)
accuracyRegf <- regconfusionf$overall[1]
accuracyRegf



## Backward Selection

regfit.bac <- regsubsets(V86~., data = data_train, nbest = 1, nvmax = 86, method = 'backward', really.big = FALSE)
my_bac <- summary(regfit.bac)
my_bac
# V10,V18,V21, V46,V47, V59,V76, V82 V85 are the best subsets
par(mfrow = c(2,2))
plot(my_bac$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(my_bac$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")

plot(my_bac$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(my_bac$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")
summary(regfit.bac)$outmat[3,]

bacmod = lm(V86~ V10 + V18 + V21 + V46 + V47 + V59 + V76 + V82 + V85, data=data_train)
lm.bac <- predict(bacmod, data_test)
mean((lm.bac - data_test_trgt)^2)
which.min(lm.bac)
lm.bac <- round(lm.bac)
lm.bac <- as.numeric(lm.bac)
lm.bac <- as.factor(lm.bac)
regconfusionb <- confusionMatrix(lm.bac, trgt$V1)
accuracyRegb <- regconfusionb$overall[1]
accuracyRegb


## COMPARING FORWARD AND BACKWARD FOR 5 VARIABLES

coef(regfit.for, 5)
coef(regfit.bac, 5)



## LASSO REGRESSION


data_test1 <- cbind(data_test,data_test_trgt)
data_testt <- data.frame(data_test1)
head(data_testt)
x=model.matrix (V86~., data_train)[,-1]
y=model.matrix (V1.1~.,data_testt)[,-1]
lassoreg2 <- glmnet(x, data_train$V86, alpha = 1)
cv.las2 <- cv.glmnet(x, data_train$V86, alpha = 1)
bestlamlas2 <- cv.las2$lambda.min
bestlamlas2
lassoprediction2 <- predict(lassoreg2, s = bestlamlas2, newx = y)
mean((lassoprediction2 - data_test_trgt)^2)
predict(lassoreg, s = bestlamlas, type = "coefficients")
lassoprediction2 <- round(lassoprediction2)
lassoprediction2 <- as.numeric(lassoprediction2)
lassoprediction2 <- as.factor(lassoprediction2)
regconfusionlas <- confusionMatrix(lassoprediction2, trgt$V1)
accuracyReglas <- regconfusionlas$overall[1]
accuracyReglas


## Ridge Regression


data_test1 <- cbind(data_test,data_test_trgt)
data_testt <- data.frame(data_test1)
head(data_testt)
x=model.matrix (V86~., data_train)[,-1]
y=model.matrix (V1.1~.,data_testt)[,-1]
ridgereg2 <- glmnet(x, data_train$V86, alpha = 0)
cv.rid2 <- cv.glmnet(x, data_train$V86, alpha = 0)
bestlamrid2 <- cv.rid2$lambda.min
bestlamrid2
ridgeprediction2 <- predict(ridgereg2, s = bestlamrid2, newx = y)
mean((ridgeprediction2 - data_test_trgt)^2)
predict(ridgereg2, s = bestlamrid2, type = "coefficients")
ridgeprediction2 <- round(ridgeprediction2)
ridgeprediction2 <- as.numeric(ridgeprediction2)
ridgeprediction2 <- as.factor(ridgeprediction2)
regconfusionrid <- confusionMatrix(ridgeprediction2, trgt$V1)
accuracyRegrid <- regconfusionrid$overall[1]
accuracyRegrid

## COMPARE THE ESTIMATES OF EACH MODEL WITH R^2

avg <- mean(data_test_trgt$V1)
ols_r2 <- 1 - mean((lm.y - data_test_trgt$V1)^2) / mean((avg - data_test_trgt$V1)^2)
for_r2 <- 1 - mean((lm.for - data_test_trgt$V1)^2) / mean((avg - data_test_trgt$V1)^2)
bac_r2 <- 1 - mean((lm.bac - data_test_trgt$V1)^2) / mean((avg - data_test_trgt$V1)^2)
las_r2 <- 1 - mean((lassoprediction2 - data_test_trgt$V1)^2) / mean((avg - data_test_trgt$V1)^2)
rid_r2 <- 1 - mean((ridgeprediction2 - data_test_trgt$V1)^2) / mean((avg - data_test_trgt$V1)^2)
ols_r2
for_r2
bac_r2
las_r2
rid_r2

















