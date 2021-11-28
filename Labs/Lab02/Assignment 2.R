data <- read.csv2("Labs/Lab02/bank-full.csv",stringsAsFactors=TRUE)

# Task 1

data <- data[,-12]

n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.4)) 
train=data[id,] 
id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.3)) 
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,] 

# Task 2

library(tree)

misclass <- function(actual_val,fitted_val){
  
  confusion_matrix <- table(actual_val,fitted_val)
  n <- length(actual_val)
  
  rate <- 1 - (sum(diag(confusion_matrix))/n)
  
  return(rate)
}

# Decision tree with default settings
tree_train <- tree(y~., data=train)

pred_train <- predict(tree_train, newdata = train, type = "class")
rate_train <- misclass(train$y,pred_train)

pred_valid <- predict(tree_train, newdata = valid, type = "class")
rate_valid <- misclass(valid$y,pred_valid)

# Decision tree with smallest allowed node size equal to 7000
tree_train_7000 <- tree(y~., data=train, control = tree.control(nobs = nrow(train), minsize = 7000))

pred_train_7000 <- predict(tree_train_7000, newdata = train, type = "class")
rate_train_7000 <- misclass(train$y,pred_train_7000)

pred_valid_7000 <- predict(tree_train_7000, newdata = valid, type = "class")
rate_valid_7000 <- misclass(valid$y,pred_valid_7000)

# Decision tree minimum deviance 0.0005

set.seed(12345)
tree_train_dev <- tree(y~., data=train, control = tree.control(nobs = nrow(train), mindev = 0.0005))

set.seed(12345)
pred_train_dev <- predict(tree_train_dev, newdata = train, type = "class")
rate_train_dev <- misclass(train$y,pred_train_dev)

set.seed(12345)
pred_valid_dev <- predict(tree_train_dev, newdata = valid, type = "class")
rate_valid_dev <- misclass(valid$y,pred_valid_dev)

# Task 3

trainScore <- rep(0,49)
validScore <- rep(0,49)

for(i in 2:50) {
  prunedTree <- prune.tree(tree_train_dev,best=i)
  pred <- predict(prunedTree, newdata=valid, type="tree")
  trainScore[i] <- deviance(prunedTree)
  validScore[i] <- deviance(pred)
  }

df <- data.frame("n" = c(2:50), "train" = trainScore[2:50], "valid" = validScore[2:50])

library(ggplot2)

ggplot(data = df) + 
  geom_point(aes(x = n, y = train, color = "#0072B2")) + 
  geom_point(aes(x = n, y = valid, color = "#d11141")) +
  theme_bw() + 
  labs(title = "Dependence of Deviance Between Training and Validation Data") +
  xlab("Number of Leaves") +
  ylab("Deviance") + 
  theme(legend.position="right") +
  scale_color_manual(values = c('#0072B2','#d11141'),
                     name = "Legend",
                     labels = c("Train","Valid" ))

# which.min(validScore[-1]) optimal number of leafs 21
optimal <- prune.tree(tree_train_dev, best=which.min(validScore[-1]))
# plot(optimal)
# text(optimal,pretty = 0)

leafs <- list(optimal$frame$var)

sort(table(leafs))

# Task 4

mis_test_opt <- predict(finalTree, newdata=test, type="class")
confusion_matrix <- table(test$y, mis_test_opt)

# Accuracy = TP+TN/TP+FP+FN+TN
accuracy <- ((confusion_matrix[1,1] + confusion_matrix[2,2])/ sum(confusion_matrix))*100

# Precision = TP/TP+FP
precision <- confusion_matrix[2,2]/sum(confusion_matrix[1,2]+confusion_matrix[2,2])

# Recall = TP/TP+FN
recall <- confusion_matrix[2,2]/sum(confusion_matrix[2,1]+confusion_matrix[2,2])

# F1 Score = 2*(Recall * Precision) / (Recall + Precision)
f1_score <- 2*(recall*precision)/(recall+precision)

# Task 5
loss_matrix <- matrix(c(0, 5, 1, 0), nrow = 2, byrow = TRUE)

library(rpart)

tree_test_loss <- rpart(y~., data = test, method = "class", parms = list(loss = loss_matrix))
pred_test_loss <- predict(tree_test_loss, newdata=test, type="class")
confusion_matrix_loss <- table(test$y, pred_test_loss)

# Task 6





