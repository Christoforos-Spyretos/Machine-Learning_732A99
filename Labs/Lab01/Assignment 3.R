#-------------------------- Assignment 3 --------------------------#

# Reading & Preparing Data

data <- read.csv("Labs/Lab01/pima-indians-diabetes.csv", header = FALSE)

colnames(data) <- c("Pregnancies",
                  "Glucose",
                  "BloodPressure",
                  "SkinThickness",
                  "Insulin",
                  "BMI",
                  "DiabetesPedigreeFunction",
                  "Age",
                  "Diabetes")

data$Diabetes <- as.factor(data$Diabetes)
levels(data$Diabetes) <- c("No","Yes")

# Task 1 

library(ggplot2)

scatterplot <- ggplot(data, aes(x = Age, y = Glucose, col = Diabetes)) +
  geom_point() +
  theme_classic() +
  ggtitle("Glucose vs. Age coloured by Diabetes Level")

scatterplot

# Task 2
library(caret)

glm_fit <- train(Diabetes ~ Age + Glucose, data = data, method="glm")
data$PredDiabetes <- predict(glm_fit) # predict function uses 0.5 as default value

misclass <- function(actual_val,predicted_val){
  
  confusion_matrix <- table(actual_val,predicted_val)
  n <- length(actual_val)
  
  error <- 1 - (sum(diag(confusion_matrix))/n)
  
  return(error)
}

misclass(data$Diabetes,data$PredDiabetes)

scatterplot2 <- ggplot(data, aes(x = Age, y = Glucose, col = PredDiabetes)) +
  geom_point() +
  theme_classic() + 
  ggtitle("Glucose vs. Age coloured by Predicted Diabetes Level")

scatterplot2

# Task 3

intercept <- glm_fit$finalModel$coefficients[1]/(-glm_fit$finalModel$coefficients[3]) 

slope <- glm_fit$finalModel$coefficients[2]/(-glm_fit$finalModel$coefficients[3])

scatterplot3 <- scatterplot2 + geom_abline( intercept = intercept, slope = slope)

scatterplot3

# Task 4

data$PredDiabetes2 <- as.factor(ifelse(predict(glm_fit,
                                             type="prob") < 0.2,
                                     "No","Yes")[,2])

misclass(data$Diabetes, data$PredDiabetes2)

scatterplot3 <- ggplot(data, aes(x = Age, y = Glucose, col = PredDiabetes2)) +
  geom_point()

scatterplot3

data$PredDiabetes8 <- as.factor(ifelse(predict(glm_fit,
                                             type="prob") < 0.8,
                                     "No","Yes")[,2])

misclass(data$Diabetes, data$PredDiabetes8)

scatterplot4 <- ggplot(data, aes(x = Age, y = Glucose, col = PredDiabetes8)) +
  geom_point()

scatterplot4

# Task 5

data$z1 <- data$Glucose^4
data$z2 <- data$Glucose^3 * data$Age
data$z3 <- data$Glucose^2 * data$Age^2
data$z4 <- data$Glucose * data$Age^3
data$z5 <- data$Age^4

glm_fit_new <- train(Diabetes ~ Age + Glucose + z1 + z2 +
                       z3 + z4 + z5, data = data, method="glm")
data$PredDiabetesNew <- predict(glm_fit_new) 

misclass(data$Diabetes, data$PredDiabetesNew)

scatterplot5 <- ggplot( data = data, aes( x = Age, y = Glucose, colour = PredDiabetesNew)) +
  geom_point()

scatterplot5



































