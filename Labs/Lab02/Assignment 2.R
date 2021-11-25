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

library(rpart)

# Decision tree with default settings
tree_train <- tree(y~., data=train)

mis_train <- predict(tree_train, newdata = train, type = "class")
table(train$y,mis_train)

mis_valid <- predict(tree_train, newdata = valid, type = "class")
table(valid$y,mis_valid)

# Decision tree with smallest allowed node size equal to 7000
tree_train_7000 <- tree(y~., data=train, control = tree.control(nobs = nrow(train), minsize = 7000))

mis_train_7000 <- predict(tree_train_7000, newdata = train, type = "class")
table(train$y,mis_train_7000)

mis_valid_7000 <- predict(tree_train_7000, newdata = valid, type = "class")
table(valid$y,mis_valid_7000)

# Decision tree minimum deviance 0.0005

set.seed(12345)
tree_train_dev<- tree(y~., data=train, control = tree.control(nobs = nrow(train), mindev = 0.0005))

set.seed(12345)
mis_train_dev <- predict(tree_train_dev, newdata = train, type = "class")
table(train$y,mis_train_dev)

set.seed(12345)
mis_valid_dev <- predict(tree_train_dev, newdata = valid, type = "class")
table(valid$y,mis_valid_dev)
