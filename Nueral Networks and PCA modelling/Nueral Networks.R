#########################################################################################
## Homework Assignment 5 - Question 2
## Dithya Sridharan
## Class Number : 48
## Edited: December 16, 2018
#########################################################################################

#########################################################################
##Load the libraries 
#########################################################################

install.packages('neuralnet')
library(neuralnet)
ls("package:neuralnet")

##################################################################
##Splitting the data set into training and test data
##################################################################

spam_train = sample(1:nrow(spam), nrow(spam)*0.80)
spam_test = -spam_train
spam_train_data = spam[spam_train, ]
spam_test_data = spam[spam_test, ]
#spam_train_data$spam <- as.character(spam_train_data$spam)
spam_train_data$spam <- as.numeric(spam_train_data$spam)
spam$spam <- as.numeric(spam$spam)

######################################################################################
##Cross Validating
#####################################################################################

crossvalidate <- function(spam,hidden_l=c(5))
{
  # @params
  
  # data          Spam dataset (data.frame)
  # hidden_l      a numeric vector with number of neurons for each hidden layer
  #               default to 5.
  
  # Scaling the data (min-max scaling)
  #maxs <- apply(data, 2, max) 
  #mins <- apply(data, 2, min)
  #scaled <- as.data.frame(scale(spam, center = mins, scale = maxs - mins))
  
  # Initialize cv.error vector
  cv.error <- NULL
  
  # Number of train-test splits
  k <- 10
  
  # Cross validating
  for(j in 1:k)
  {
    # Train-test split
    index <- sample(1:nrow(spam),round(0.90*nrow(spam)))
    train.cv <- spam[index,]
    test.cv <- spam[-index,]
    n<-names(spam_train_data)
    f <- as.formula(paste("spam ~", paste(n[!n %in% "spam"], collapse = " + ")))
    
    # NN fitting
    nn <- neuralnet(f,data=train.cv,hidden=hidden_l,linear.output=T)
    
    # Predicting
    pr.nn <- compute(nn,test.cv[,1:13])
    
    # Scaling back the predicted results
    pr.nn <- pr.nn$net.result*(max(spam$spam)-min(spam$spam))+min(spam$spam)
    
    # Real results
    test.cv.r <- (test.cv$spam)*(max(spam$spam)-min(spam$spam))+min(spam$spam)
    
    # Calculating MSE test error
    cv.error[j] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  }
  
  # Return average MSE
  return(mean(cv.error))
}

###################################################################################
##Neural Network 
###############################################################################



n<-names(spam_train_data)
f <- as.formula(paste("spam ~", paste(n[!n %in% "spam"], collapse = " + ")))
nn <- neuralnet(f, data=spam_train_data,hidden = 5, err.fct = "ce",linear.output = FALSE)
#
names(nn)
nn$result.matrix
round(nn$net.result[[1]])
nn_pred = round(compute(nn,spam_test_data[,1:57])$net.result[,1])
mean(spam_test_data$spam != nn_pred)
#0.03876221498

