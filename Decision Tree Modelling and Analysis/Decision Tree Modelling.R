#########################################################################################
## Homework Assignment 4 - Question 2
## Dithya Sridharan
## Class Number : 48
## Edited: Novemeber 24, 2018
#########################################################################################


#########################################################################
##Load the libraries 
#########################################################################

install.packages("rpart")
library(rpart)
install.packages("tree")
library(tree)
library(MASS)
install.packages("caret")
library(caret)


########################################################################
##Reading the data
########################################################################

winedata = read.csv("E:\\wine.data",header = FALSE)


#######################################################################
##Splitting data into training and test
#######################################################################

smp_size <- floor(0.80 * nrow(winedata))
train_ind <- sample(seq_len(nrow(winedata)), size = smp_size)
wine_train <- winedata[train_ind, ]
wine_test <- winedata[-train_ind, ]
head(wine_train)

######################################################################
##Growing a tree model and checking prediction error
######################################################################

model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
wine_model <- rpart(V1~., data = wine_train, method = "class", control = model.control)
tree_pred = predict(wine_model, wine_test, type = "class")
mean(tree_pred == wine_test$V1) #0.08333 error 
#good fit

plot(wine_model)
text(wine_model)

#####################################################################
##Pruning and comparing
#####################################################################

min_cp = which.min(wine_model$cptable[,4])
wine_model_pruned <- prune(wine_model, cp = wine_model$cptable[min_cp,1])
tpred = predict(wine_model_pruned, wine_test, type = "class")
mean(tpred != wine_test$V1) #0.08333 error
#Same for pruned and original tree
#confusionMatrix(tpred, test$class)
wine_model$frame
table(wine_test$V1,tpred)
summary(wine_model)


nodes_wine <- wine_model
nodes_wine$frame$yval = as.numeric(rownames(nodes_wine$frame))
testnodes <- table(predict(nodes_wine, wine_test, type="vector"))
testnodes
