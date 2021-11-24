#-------------------------- Assignment 1 --------------------------#

# Reading Data

data <- read.csv("Labs/Lab02/tecator.csv")

# Splitting Data into test and train data (50/50)

n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
test=data[-id,]

# Task 1 (Fitting the linear regression model) 

train <- train[c(-1,-103,-104)]
test <- test[c(-1,-103,-104)]

lrm <- lm(Fat ~., data = train)

summary(lrm)

train_error <- mean((train$Fat - predict(lrm))^2)
train_error
test_error<- mean((test$Fat - predict(lrm,test))^2)
test_error #big number

# Task 2

# Cost function from towards data science 

# Task 3
library(glmnet)

X <- train[,-101]
Y <- data.frame(train[,101])

lasso <- glmnet(as.matrix(X),as.matrix(Y),alpha=1, family="gaussian") # alpha 1 for lasso

summary(lasso)

plot(lasso,xvar="lambda",label=TRUE) #understand the plot/make a better plot

# Task 4

ridge <- glmnet(as.matrix(X),as.matrix(Y),alpha=0,family="gaussian") # alpha 0 for ridge

plot(ridge,xvar="lambda",label=TRUE) #understand the plot/make a better plot

# Task 5


