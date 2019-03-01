#########################################################################################
## Homework Assignment 3 - Question 2
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
library(data.table)
library(class)
set.seed(1)

############################################################################
##Reading Diabetes data from source
############################################################################

mydat <- fread('https://astro.temple.edu/~alan/DiabetesAndrews36_1.txt')


############################################################################
##Splitting into training and test data
############################################################################

smp_size <- floor(0.75 * nrow(mydat))
train_ind <- sample(seq_len(nrow(mydat)), size = smp_size)
train1 <- mydat[train_ind, ]
test1 <- mydat[-train_ind, ]
head(train1)


############################################################################
##Scatter plots
############################################################################

cols <- character(5)
cols[]<-"green"
cols[mydat$V10 == 3] <- "blue"
cols[mydat$V10 == 2] <- "red"
pairs(~V5+V6+V7+V8+V9,data=mydat,col = cols,
      main="Simple Scatterplot Matrix")



##############################################################################
##LDA and QDA for the data
##############################################################################

ldaval = lda(V10 ~ .-V1-V2-V3-V4 , data = train1)
ldaval_pred_train=predict(ldaval,newdata = train1)
mean(ldaval_pred_train$class != train1$V10) #0.09259259
ldaval_pred_test=predict(ldaval,newdata = test1)
mean(ldaval_pred_test$class != test1$V10) #0.05405405


qdaval = qda(V10 ~ .-V1-V2-V3-V4 , data = train1)
qdaval_pred_train=predict(qdaval,newdata = train1)
mean(qdaval_pred_train$class != train1$V10) #0.06481481
qdaval_pred_test=predict(qdaval,newdata = test1)
mean(qdaval_pred_test$class != test1$V10) #0.02702703



##############################################################################
##Predictng QDA and LDA for given values
##############################################################################

V1=c(36)
V2=c(1)
V3=c(146)
V4 = 146
V5=c(0.98)
V6=c(122)
V7=c(544)
V8=c(186)
V9=c(184)
df=data.frame(V1,V2,V3,V4,V5,V6,V7,V8,V9)
predict(qdaval,df) #2
predict(ldaval,df) #3
