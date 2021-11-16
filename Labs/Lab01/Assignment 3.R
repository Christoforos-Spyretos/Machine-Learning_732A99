#-------------------------- Assignment 3 --------------------------#

# Reading & Preparing Data

data <- read.csv("Labs/Lab01/pima-indians-diabetes.csv", header = FALSE)

colnames(data) <- c("TimesPregnant", "Plasma", "Diastolic", "Triceps",
                    "Insulin", "MassIndex", "Pedigree", "Age", "Diabetes")

# Task 1 

library(ggplot2)

scatterplot <- ggplot( data = data, aes( x = Age, y = Plasma, colour = as.factor(Diabetes))) +
  geom_point() + scale_color_manual(name = "Diabetes", 
                                    labels = c("Non-Diabetic ", "Diabetic "),
                                    values =c("#F8766D", "#00BFC4"))

scatterplot

# Task 2

fit <- glm( Diabetes ~ Age + Plasma, data = data, family = binomial)
probabilities <- predict(object = fit, newdata =  data, type = "link")
predicted_values <- ifelse(probabilities > 0.5, "pos", "neg")

data$Predict <- predicted_values

misclass <- function(actual_val,predicted_val){
  
  confusion_matrix <- table(actual_val,predicted_val)
  n <- length(actual_val)
  
  error <- 1 - (sum(diag(confusion_matrix))/n)
  
  return(error)
}

misclass(data$Diabetes, data$Predict)

scatterplot2 <- ggplot( data = data, aes( x = Age, y = Plasma, colour = Predict)) +
  geom_point() + scale_color_manual(name = "Diabetes", 
                                    labels = c("Non-Diabetic ", "Diabetic "),
                                    values =c("#F8766D", "#00BFC4"))

scatterplot2

# Task 3

intercept <- fit$coefficients[1]/(-fit$coefficients[3]) 

slope <- fit$coefficients[2]/(-fit$coefficients[3])

scatterplot3 <- scatterplot2 + geom_abline( intercept = intercept, slope = slope)

scatterplot3

# Task 4

predict_0.2 <- as.factor(ifelse(predict(fit, data, type="response")>0.2,"neg","pos"))

data$Predict_0.2 <- predict_0.2

misclass(data$Diabetes, data$Predict_0.2)

scatterplot3 <- ggplot(data, aes(x = Age, y = Plasma, col = Predict_0.2)) +
  geom_point()

scatterplot3

predict_0.8 <- as.factor(ifelse(predict(fit, data, type="response")>0.8,"neg","pos"))
data$Predict_0.8 <- predict_0.8

misclass(data$Diabetes, data$Predict_0.8)

scatterplot4 <- ggplot(data, aes(x = Age, y = Plasma, col = Predict_0.8)) +
  geom_point()

scatterplot4

# Task 5

data$z1 <- data$Plasma^4
data$z2 <- data$Plasma^3 * data$Age
data$z3 <- data$Plasma^2 * data$Age^2
data$z4 <- data$Plasma * data$Age^3
data$z5 <- data$Age^4

fit2 <- glm(Diabetes ~ Age + Plasma + z1 + z2 + z3 + z4 + z5, data = data, family = binomial)
probabilities <- predict(object = fit2, newdata =  data, type = "response")
predicted_values2 <- ifelse(probabilities > 0.5, "pos", "neg")

data$Predict2 <- predicted_values2

misclass(data$Diabetes, data$Predict2)

scatterplot5 <- ggplot( data = data, aes( x = Age, y = Plasma, colour = Predict2)) +
  geom_point()

scatterplot5



































