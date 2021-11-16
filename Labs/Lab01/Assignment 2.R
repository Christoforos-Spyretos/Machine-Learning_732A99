#-------------------------- Assignment 2 --------------------------#

# Reading & Preparing Data

data <- read.csv("Labs/Lab01/parkinsons.csv")

data <- data[,-c(1:4,6)]  # deleting the variables that we do not need.

# Task 1 (Scaling & splitting data into train & test set.)

n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.6)) 
train=data.frame(data[id,])
test=data.frame(data[-id,])

mean_train <- numeric(ncol(train))
sd_train <- numeric(ncol(train))

for(i in 1:ncol(train)){
  mean_train[i] <- mean(train[,i])
  sd_train[i] <- sd(train[,i])
}

train <- data.frame(scale(train))

test_new <- data.frame(matrix(nrow = nrow(test), ncol = ncol(test)))

for(i in 1:ncol(train)){
  test_new[,i] <- (test[,i] - mean_train[i])/sd_train[i]
}

test <- as.data.frame(test_new)
colnames(test) <- colnames(train)

# Task 2 (Linear regression model & estimation of train and test MSE.)

# Fitting linear regression model

lrm <- lm(motor_UPDRS ~ Jitter... + Jitter.Abs. + Jitter.RAP + Jitter.PPQ5 + Jitter.DDP +
            Shimmer + Shimmer.dB. + Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + 
            Shimmer.DDA + NHR + HNR + RPDE + DFA + PPE, data = train)

summary(lrm)

# Estimating training & test MSE 

train_MSE <- mean((train$motor_UPDRS - predict(lrm))^2)
train_MSE
test_MSE <- mean((test$motor_UPDRS - predict(lrm,test))^2)
test_MSE

# Task 3 (Implementing 4 functions)

# Loglikelihood

Loglikelihood <- function(theta, input_data){
  
  Y <- input_data[,1]
  X <- as.matrix(input_data[,-1])
  predicted <- X %*% theta
  residual <- Y - predicted
  sigma <- sd(residual)
  n <- nrow(input_data)
  
  logl <- -n*log(sqrt(2*pi)*sigma) - (1/(2*(sigma^2)))*sum((Y-X %*% theta)^2) 
  
  return(-logl)
}

Loglikelihood(theta = rep(1,16), input_data = train)

# Ridge

Ridge <- function(param, input_data, lamda){
  
  Υ <- input_data[,1]
  X <- as.matrix(input_data[,-1])
  theta <- param
  n <- nrow(input_data)
  
  penalty <- lamda*sum(theta^2)
  
  logl <- Loglikelihood(theta, input_data) + penalty
  
  return(logl)
}

Ridge(param = rep(1,16),input_data = train, lamda = 2)

# Ridge optimal

RidgeOpt <- function(lamda, input_data){
  
  optimal <- optim(par = rep(1, times = 16), fn = Ridge,
                   lamda = lamda, input_data = input_data ,method = "BFGS")
  
  k <- length(optimal$par)
  optimal_theta <- optimal$par[-k]
  
  
  return(optimal$par)
}

theta <- RidgeOpt(lamda = 1, input_data = train)

# Degrees of freedom

df <- function(input_data){
  
  X <- as.matrix(input_data[,-1])
  I <- diag(ncol(X))
  hat_matrix <- X %*% solve(t(X) %*% X) %*% t(X)
  
  degrees_of_freedom <- sum(diag(hat_matrix))
  
  return(degrees_of_freedom)
}

df(input_data = train)

# Task 4

theta_hat_1 <- RidgeOpt( lamda = 1, input_data = train)
theta_hat_100 <- RidgeOpt( lamda = 100, input_data = train)
theta_hat_1000 <- RidgeOpt( lamda = 1000, input_data = train)

predicted_values <- function(theta_hat, input_data){
  
  X <- as.matrix(input_data[,-1])
  y_hat <- X %*% theta_hat 
  
  return(y_hat)
}

y_hat_1_train <- predicted_values(theta_hat_1, train)
y_hat_100_train <- predicted_values(theta_hat_100, train)
y_hat_1000_train  <- predicted_values(theta_hat_1000, train)

y_hat_1_test <- predicted_values(theta_hat_1, test)
y_hat_100_test <- predicted_values(theta_hat_100, test)
y_hat_1000_test  <- predicted_values(theta_hat_1000, test)


MSE <- function(Y_hat, input_data){
  
  Y <- input_data[,1]
  n <- nrow(input_data)
  
  mse <- (1/n) * sum((Y-Y_hat)^2)
  
  return(mse)
}

MSE_1_train <- MSE(y_hat_1_train, train)
MSE_100_train <- MSE(y_hat_100_train, train)
MSE_1000_train <- MSE(y_hat_1000_train, train)

MSE_1_test <- MSE(y_hat_1_test, test)
MSE_100_test <- MSE(y_hat_100_test, test)
MSE_1000_test <- MSE(y_hat_1000_test, test)

degrees_of_freedom_train <- df(input_data = train)
degrees_of_freedom_test <- df(input_data = test)
