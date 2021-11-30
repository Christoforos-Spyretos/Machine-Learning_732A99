data <- read.csv("Labs/Lab02/communities.csv")

# Task 1

data_scale <- data.frame(scale(data[,-101]))

data_scale$ViolentCrimesPerPop <- data$ViolentCrimesPerPop

cov_matrix <- cov(data_scale)

eig <- eigen(cov_matrix)

counter <- which(cumsum(eig$values/sum(eig$values) * 100) >= 95)
counter[1]

prop_var <- sprintf("%2.3f", eig$values/sum(eig$values)*100)
prop_var[c(1:2)]

# Task 2

pca <- princomp(data_scale)

plot(pca$loadings[,1], main = "Score Plot", pch = 16, col = "navy", ylab = "First Principle Component")

df_scores <- data.frame(pca$scores[,1:2])

df_scores$ViolentCrimesPerPop <- data$ViolentCrimesPerPop

library(ggplot2)

ggplot(data = df_scores, aes(x=Comp.1, y=Comp.2, color = ViolentCrimesPerPop)) + 
  geom_point() +
  labs(title("PC Scores of PC1 & PC2")) + xlab("PC1") + ylab("PC2") +
  scale_colour_gradientn(colors=rainbow(7))

# Task 3

n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
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

lrm <- lm(ViolentCrimesPerPop~., data = train)

# summary(lrm)

train_mse <- mean((train$ViolentCrimesPerPop-predict(lrm))^2)
test_mse <- mean((test$ViolentCrimesPerPop-predict(lrm,test))^2)

# Task 4 

my_cost <- function(beta,input_data){
  X <- as.matrix(input_data[,101])
  Y <- as.matrix(input_data[,-101])
  
  cost <- mean((X - Y%*%beta)^2)

  return(cost)
}

params <- list()
mse_train <- list()
mse_test <- list()
k <- 0

my_function <- function(beta,train_data = train,test_data = test){
  .GlobalEnv$k = .GlobalEnv$k+1
  x <-  my_cost(beta,train_data)
  .GlobalEnv$mse_train[[k]] = x
  .GlobalEnv$mse_test[[k]] = my_cost(beta,test_data)
  return(x)
} 

res <- optim( par = rep(0,100), fn = my_function, method = "BFGS")

plot(2000:k, mse_train[2000:k], col="navy", type="l", ylim = c(0,1), 
     ylab = "MSE of Train & Test", xlab = "Iterations")
lines(2000:k, mse_test[2000:k], col="red4")



