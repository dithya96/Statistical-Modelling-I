#########################################################################################
## Homework Assignment 4 - Question 4
## Dithya Sridharan
## Class Number : 48
## Edited: Novemeber 24, 2018
#########################################################################################

########################################################################################
##Loading the libraries
#######################################################################################


library(ISLR)
#install.packages('svm')
library(e1071)
data(OJ)

#######################################################################################
##Splitting as training and test
#####################################################################################

oj_train = sample(1:nrow(OJ), nrow(OJ)*0.80)
oj_test = -oj_train
oj_train_data = OJ[oj_train, ]
oj_test_data = OJ[oj_test, ]

##################################################################################
##SVM FROM 0.01 TO 10
####################################################################################

for(i in seq(from=0.01, to=10, by=0.99)){
oj.svm.fit <- svm(Purchase ~. , data = oj_train_data, cost = i,
                  kernel = 'linear')
summary(oj.svm.fit)
tr.pred <- predict(oj.svm.fit)
ts.pred <- predict(oj.svm.fit, newdata = oj_test_data)
Tr.table <- table(Predict = tr.pred, Truth = oj_train_data$Purchase)
Ts.table <- table(Predict = ts.pred, Truth = oj_test_data$Purchase)

}

(Tr.table[1,2]+Tr.table[2,1])/sum(Tr.table); (Ts.table[1,2]+Ts.table[2,1])/sum(Ts.table)



# 0.160046729
# 0.1682242991
###########################################################################################
##Tuning
##########################################################################################

cost <- seq(0.01,10,100)
tune.oj.svm.fit <- tune(svm, Purchase~., data = OJ,
                        kernel = 'linear',
                        ranges = list(cost = cost))
tune.oj.svm.fit$best.model
#optimal cost is 0.01

#########################################################################
##Radial fit
#########################################################################

cost <- seq(0.01,10,100)
tune.oj.rd.svm.fit <- tune(svm, Purchase~., data = oj_train_data,
                           kernel = 'radial',
                           ranges = list(cost = cost))

tr.rd.pred <- predict(tune.oj.rd.svm.fit$best.model, oj_train_data)
ts.rd.pred <- predict(tune.oj.rd.svm.fit$best.model, oj_test_data)
table(pred = tr.rd.pred, truth = oj_train_data$Purchase)
#516 340

table(pred = ts.rd.pred, truth = oj_test_data$Purchase)
#137 77

##Radial is not good fit

#################################################################################
##Polynomial fit
################################################################################

tune.oj.polu.svm.fit <- tune(svm, Purchase~., data = oj_train_data,
                             kernel = 'polynomial',
                             ranges = list(cost = cost),
                             degree = 2)
tr.rd.pred <- predict(tune.oj.polu.svm.fit$best.model, oj_train_data)
ts.rd.pred <- predict(tune.oj.polu.svm.fit$best.model, oj_test_data)
table(pred = tr.rd.pred, truth = oj_train_data$Purchase)
#516 340

table(pred = ts.rd.pred, truth = oj_test_data$Purchase)
#137 72

#linear approach is better