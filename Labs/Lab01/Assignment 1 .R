#-------------------------- Assignment 1 --------------------------#

# Reading Data 

data <- read.csv("Labs/Lab01/optdigits.csv", header = FALSE)

# Task 1 Splitting the data into training, validation and test sets (50%/25%/25%).

n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 

id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
valid=data[id2,]

id3=setdiff(id1,id2)
test=data[id3,] 

# Task 2  Training data to fit 30-nearest neighbor classifier.

library(kknn)

knn_train <- kknn(as.factor(V65)~., train = train, test = train, k = 30, kernel = "rectangular")
knn_valid <- kknn(as.factor(V65)~., train = train, test = valid, k = 30, kernel = "rectangular")
knn_test <- kknn(as.factor(V65)~., train = train, test = test, k = 30, kernel = "rectangular")

# Generating confusion matrices

confusion_train <- table(train$V65, knn_train$fitted.values)
confusion_train

confusion_test <- table(test$V65, knn_test$fitted.values)
confusion_test

# Misclassification error

misclass <- function(actual_val,fitted_val){
  
  confusion_matrix <- table(actual_val,fitted_val)
  n <- length(actual_val)
  
  error <- 1 - (sum(diag(confusion_matrix))/n)
  
  return(error)
}

misclass(train$V65, knn_train$fitted.values)

misclass(test$V65, knn_test$fitted.values)

# Task 3 Cases & Heatmap

index_8 = which(train$V65 == 8)

knn_train_prob = data.frame(knn_train$prob)

prob_8 = knn_train_prob$X8[index_8]

easiest_8 = index_8[tail(order(prob_8), n = 2)]
hardest_8 = index_8[head(order(prob_8), n = 3)]

# Visualize digits with heatmap.
easiest_8_1 <- matrix(as.matrix(train[easiest_8[1], 1:64]), nrow = 8, ncol = 8, byrow = TRUE)
heatmap(easiest_8_1, Rowv = NA, Colv = NA)

easiest_8_2 <- matrix(as.matrix(train[easiest_8[2], 1:64]), nrow = 8, ncol = 8, byrow = TRUE)
heatmap(easiest_8_2, Rowv = NA, Colv = NA)

hardest_8_1 <- matrix(as.matrix(train[hardest_8[1], 1:64]), nrow = 8, ncol = 8, byrow = TRUE)
heatmap(hardest_8_1, Rowv = NA, Colv = NA)

hardest_8_2 <- matrix(as.matrix(train[hardest_8[2], 1:64]), nrow = 8, ncol = 8, byrow = TRUE)
heatmap(hardest_8_2, Rowv = NA, Colv = NA)

hardest_8_3 <- matrix(as.matrix(train[hardest_8[3], 1:64]), nrow = 8, ncol = 8, byrow = TRUE)
heatmap(hardest_8_3, Rowv = NA, Colv = NA)

# Task 4 Optimal K

k_optmimal_train <- c() 
k_optmimal_valid <- c()

for (i in 1:30){ 
  set.seed(12345)
  knn_train <-kknn(as.factor(V65)~., train=train,test=train, k = i,kernel = "rectangular")
  knn_valid <-kknn(as.factor(V65)~., train=train,test=valid, k = i,kernel = "rectangular")
  
  k_optmimal_train[i] = 1-(sum(diag(table(train$V65, knn_train$fitted.values)))/nrow(train))
  k_optmimal_valid[i] = 1-(sum(diag(table(valid$V65, knn_valid$fitted.values)))/nrow(valid))
  
}


k_optmimal_train
k_optmimal_valid

rate <- data.frame( "K" <- c(1:30),
                    "k_optmimal_train" <- k_optmimal_train,
                    "k_optmimal_valid" <- k_optmimal_valid)

library(ggplot2)

my_plot <- ggplot( data = rate) + 
  geom_line(aes(x = K, y = k_optmimal_train, colour="#F8766D")) +
  geom_line(aes(x = K, y = k_optmimal_valid, colour="#00BFC4")) +
  ylab("Missclassification Rate ") + xlab("K") +
  scale_color_manual(name = "Data Sets", 
                     labels = c("Validation ", "Training "),
                     values =c("#F8766D", "#00BFC4")) + 
  theme_bw() +
  scale_alpha_continuous( breaks = 1:30)

my_plot

# Task 5
cross_entropy <- list()
for (K in 1:30) {
  fit = kknn(as.factor(V65)~., train, valid, k=K, kernel="rectangular")
  
  df <- data.frame(fit$prob)
  df$digit <- valid$V65
  
  entropy <- list()
  for (i in 1:nrow(df)) { 
    for (n in 1:10) {
      if (df$digit[i] == n-1) {
        entropy[[i]] = -(log(df[i, n]+ 1e-15))
      }
    }
  }
  cross_entropy[[K]] <- sum(unlist(entropy))
}
cross_entropy_valid <- data.frame(cross_entropy = unlist(cross_entropy), K = 1:30)

ggplot(cross_entropy_valid, aes(x = K, y = cross_entropy)) + 
  geom_point(col = "blue") + theme_bw() + 
  labs(title = "Cross-entropy error for different Ks in KNN", y = "Cross-entropy")+
  scale_x_continuous(breaks = 1:30)







