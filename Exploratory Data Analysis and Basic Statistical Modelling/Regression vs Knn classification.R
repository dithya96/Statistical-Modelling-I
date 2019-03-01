###########################################################################
## This code is to compare performance of linear regression and knn classification methods (EDA)
## Dithya Sridharan 
## Person no : 50286923
## Created : 12 September, 2018
###########################################################################

#install some libraries

install.packages("ElemStatLearn")
install.packages("DAAG")
install.packages("lattice")
install.packages("MASS")
install.packages("devtools")
install.packages("gridExtra")

ls.("package:ElemStatLearn")
??zip.test
??zip.train

#load library
library("gridExtra")
library(ggplot2)
library(DAAG)
library(lattice)
library(MASS)
library(devtools)
library(easyGgplot2)
library(tidyverse)
library(dplyr)
library(ElemStatLearn)

##############################################################################
## Training and test data
##############################################################################

head(zip.test)
head(zip.train)


#############################################################################
## Converting to dataframe and subsetting the train and test data according to question
############################################################################


ziptrain23 <- subset(zip.train, zip.train[,1]==2 | zip.train[,1]==3)
ziptest23 <- subset(zip.test, zip.test[,1]==2 | zip.test[,1]==3)
Traindf1 <- as.data.frame(ziptrain23)
Testdf1 <- as.data.frame(ziptest23)
head(Traindf1)
head(Testdf1)

##############################################################################
## Regression classification
#############################################################################
trainreg <- lm(V1 ~ ., data=Traindf1)
prediction <- predict.lm(trainreg,Testdf1)
print(prediction)


#############################################################################
## Error analysis for regression classification
#############################################################################
e1 <- mean((Traindf1$V1 - (prediction)) ^ 2)
print(e1)




############################################################################
## KNN Classification
###########################################################################

library(class)
nrow(ziptest23)
nrow(ziptrain23)

nrow(Traindf1)
nrow(Testdf1)


##########################################################################
## Initializing error dataframe
###########################################################################

error_df <- tibble(k=rep(0, 8),
                   tr=rep(0, 8),
                   tst=rep(0, 8))



###########################################################################
## cl values for KNN function
##########################################################################

knn.train <- Traindf1[,2:1389]
knn.test <- Testdf1[,2:364]

knn.train.V1 <- as.factor(Traindf1$V1)
knn.test.V1 <- as.factor(Testdf1$V1)


prediction_knn1 <- knn(train = Traindf1,test = Testdf1,cl = knn.train.V1,k = 1)
prediction_knn3 <- knn(train = Traindf1,test = Testdf1,cl = knn.train.V1,k = 3)
prediction_knn5 <- knn(train = Traindf1,test = Testdf1,cl = knn.train.V1,k = 5)
prediction_knn7 <- knn(train = Traindf1,test = Testdf1,cl = knn.train.V1,k = 7)
prediction_knn9 <- knn(train = Traindf1,test = Testdf1,cl = knn.train.V1,k = 9)
prediction_knn11 <- knn(train = Traindf1,test = Testdf1,cl = knn.train.V1,k = 11)
prediction_knn13 <- knn(train = Traindf1,test = Testdf1,cl = knn.train.V1,k = 13)
prediction_knn15<- knn(train = Traindf1,test = Testdf1,cl = knn.train.V1,k = 15)
#kne1 <- mean((Traindf1$V1 - (prediction_knn1)) ^ 2)


###################################################################################
## Error analysis for KNN
####################################################################################


err_rate1 <- round(mean(knn.train.V1!=prediction_knn1))
print(err_rate1)
err_rate3 <- round(mean(knn.train.V1!=prediction_knn3))
print(err_rate3)
err_rate5 <- round(mean(knn.train.V1!=prediction_knn5))
print(err_rate5)
err_rate7 <- round(mean(knn.train.V1!=prediction_knn7))
print(err_rate7)
err_rate9 <- round(mean(knn.train.V1!=prediction_knn9))
print(err_rate9)
err_rate11 <- round(mean(knn.train.V1!=prediction_knn11))
print(err_rate11)
err_rate13 <- round(mean(knn.train.V1!=prediction_knn13))
print(err_rate13)
err_rate15 <- round(mean(knn.train.V1!=prediction_knn15))
print(err_rate15)



