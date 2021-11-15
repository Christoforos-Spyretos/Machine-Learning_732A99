#-------------------------- Assignment 2 --------------------------#

# Reading & Preparing Data

parkinsons <- read.csv("Labs/Lab01/parkinsons.csv")

parkinsons <- parkinsons[,-c(1:4,6)]  # deleting undesirable variables

# Task 1 (Scaling & splitting data into train & test set.)

data <- scale(parkinsons)

n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.7)) 
train=data.frame(data[id,])
test=data.frame(data[-id,])

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

Loglikelihood <- function(theta, sigma, input_data){
  
  Υ <- input_data[,1]
  X <- as.matrix(input_data[,-1])
  n <- nrow(input_data)

  logl <- -n*log(sqrt(2*pi)*sigma) - (1/(2*(sigma^2)))*sum((Υ-X %*% theta)^2) 
  
  return(-logl)
}

Loglikelihood( theta = rep(1,16), sigma = 1, input_data = train)

# Ridge

Ridge <- function(param, input_data, lamda){
  
  Υ <- input_data[,1]
  X <- as.matrix(input_data[,-1])
  k <- length(param)
  theta <- param[1:k-1]
  sigma <- param[k]
  n <- nrow(input_data)
  
  penalty <- lamda*sum(theta^2)
  
  logl <- -n*log(sqrt(2*pi)*sigma) - (1/(2*(sigma^2)))*sum((Υ-X %*% theta)^2) + penalty
  
  return(-logl)
}

Ridge(param = rep(1,17),input_data = train, lamda = 2)

# Ridge optimal

RidgeOpt <- function(lamda, input_data){
  
  optimal <- optim( par = c(1:17), fn = Ridge, lamda = lamda, input_data = input_data ,method = "BFGS")
  
  k <- length(optimal$par)
  optimal_theta <- optimal$par[-k]
  
  return(optimal_theta)
}

RidgeOpt(lamda = 2, input_data = train)

# Degrees of freedom

df <- function(lamda, input_data){
  
  X <- as.matrix(input_data[,-1])
  I <- diag(ncol(X))
  hat_matrix <- X %*% solve(t(X) %*% X + lamda*I) %*% t(X)
  
  degrees_of_freedom <- sum(diag(hat_matrix))
  
  return(degrees_of_freedom)
}

df(lamda = 3,input_data = train)

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


# For linear smoothers Y_hat = S(X)Y df = trace(s)

df2 <- function(input_data){
  
  X <- as.matrix(input_data[,-1])

  hat_matrix <- X %*% solve(t(X) %*% X) %*% t(X)
  
  degrees_of_freedom <- sum(diag(hat_matrix))
  
  return(degrees_of_freedom)
}

degrees_of_freedom_train <- df2(input_data = train)
degrees_of_freedom_test <- df2(input_data = test)
