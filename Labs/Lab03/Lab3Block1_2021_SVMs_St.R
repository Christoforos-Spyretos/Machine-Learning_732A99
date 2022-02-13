# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

library(kernlab)
set.seed(1234567890)

data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo,]
spam[,-58]<-scale(spam[,-58]) # Scale data except for response (type)
tr <- spam[1:3000, ] # Training data
va <- spam[3001:3800, ] # Validation data
trva <- spam[1:3800, ] # Training and validation data
te <- spam[3801:4601, ] # Testing data

by <- 0.3 # seq(0.3, 5, by = 0.3) -> 16 models with different values for the cost of constraints violation.
# C is the Its the regularization term in the Lagrange formulation
err_va <- NULL # Error on validation data
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE) # Runs 16 different models with different C
  mailtype <- predict(filter,va[,-58]) # Predict on validation
  t <- table(mailtype,va[,58]) # confusion matrix
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t)) # error on validation data
}

# Some different models with C=3.9 (most optimal based on validation error)

## Model on training data
filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
# Predict validation
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0 # Error on validation

# Model on training data
filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
# Predict testing
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1 # Error on testing

# Model on training+validation data
filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
# Predict testing
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2 # Error on testing

# Training on all the data
filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
# Predict testing
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3 # Error on testing

# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?

#filter1 is the one that should be used. This model is trained using the training data and has had hyperparameters (C) tuned by
# testing different models on the validation data. Then finally it achieves an error of 0.085 on the testing data.

# Using the other models all bring some sort of issues with it. Filter0 is computing error on the validation data that was used to
# tune C.

# Filter2 uses both the training data and the validation data which means we cannot be certain that the tuning of the hyperparameter C
# actually is reasonable

# Filter3 uses all of the data (including the testing data) to fit the model and this creates reliability issues in terms of the error rate
# on testing dat as we do now know how well the model will generalize to new data.


# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?

# 3. Implementation of SVM predictions.

sv<-alphaindex(filter3)[[1]]
co<-coef(filter3)[[1]]
inte<- - b(filter3)
k<-NULL
for(i in 1:10){ # We produce predictions for just the first 10 points in the dataset.
  k2<-NULL
  for(j in 1:length(sv)){
    k2<- # Your code here
  }
  k<-c(k, # Your code here)
}
k
predict(filter3,spam[1:10,-58], type = "decision")