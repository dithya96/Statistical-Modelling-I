#########################################################################################
## Homework Assignment 5 - Question 3
## Dithya Sridharan
## Class Number : 48
## Edited: December 14, 2018
#########################################################################################

########################################################################################
##Loading the libraries
#######################################################################################

rm(list = ls())
library(neuralnet)
library(ElemStatLearn)
library(gam)
library(plyr)
library(ggplot2)


formula = "spam~ A.1"
for (colname in colnames(spam)){
  if(colname != "spam" & colname != "A.1" )
  {
    formula = paste(formula, colname , sep = " + ")    
  }
  
}

formula = as.formula(formula)

Spam = spam

Spam$spam = as.numeric((Spam$spam))-1
Spam = as.data.frame(scale(Spam))
Spam$spam = spam$spam
Spam$spam = as.numeric((spam$spam))-1

maxs <- apply(Spam, 2, max) 
mins <- apply(Spam, 2, min)

scaled <- as.data.frame(scale(Spam, center = mins, scale = maxs - mins))

set.seed(123)
train = sample(1:nrow(Spam), nrow(Spam)*0.30) # Took the training size smaller because it would be easier to run on local
test = -train
trainData = Spam[train, ]
testData = Spam[test, ]

nn <- neuralnet(formula , data = trainData, hidden = 7, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(nn, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam) #0.0922703
#plot(nn)
trainDataOutLier = trainData
outlierSet <- c(300, 100, 30, 3, 0.1, -300, -10, -1)

errors <- 0
j <- 0
for (i in outlierSet){
  
  trainDataOutLier[1,4] = i
  nn <- neuralnet(formula , data = trainDataOutLier, hidden = 7, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
  new.output <- compute(nn, covariate = testData[,1:57])
  predicted_class_new_data <- round(new.output$net.result)
  errors[j] <- mean(predicted_class_new_data != testData$spam)
  j <- j+ 1
  
}

plot(outlierSet[-1], errors)
df = cbind(outlierSet[-1], errors)

df <- as.data.frame(df)
ggplot(df, aes(x=V1, y=errors)) + geom_point(size=2, shape=21)
# use cv to get the best number of neurons

crossvalidate <- function(i){
  data <- Spam
  set.seed(450)
  cv.error <- NULL
  k <- 10
  
  pbar <- create_progress_bar('text')
  pbar$init(k)
  
  maxs <- apply(data, 2, max) 
  mins <- apply(data, 2, min)
  
  scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
  
  for(i in 1:k){
    index <- sample(1:nrow(data),round(0.3*nrow(data)))
    train.cv <- scaled[index,]
    test.cv <- scaled[-index,]
    
    nn <- neuralnet(formula,data=train.cv,hidden=c(i),linear.output=T)
    
    pr.nn <- compute(nn,test.cv[,1:57])
    pr.nn <- pr.nn$net.result*(max(data$spam)-min(data$spam))+min(data$spam)
    
    test.cv.r <- (test.cv$spam)*(max(data$spam)-min(data$spam))+min(data$spam)
    
    cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
    
    pbar$step()
  }
}


test.error = NULL

for(i in 1:13)
{       
  # Calculate test error through cross validation
  test.error[i] <- crossvalidate(i)
}
