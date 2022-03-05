library(tidyverse)
library(dbplyr)
library(titanic)
library(janitor)

data("titanic_train")
#View(titanic_train)
titanic_train <- tibble(titanic_train)

titanic_train <- titanic_train %>%
  clean_names()

names(titanic_train)

# check missing value
count_NA <- function(data) {
  sum(is.na(data))
}

count_NA(titanic_train)

# drop NA
titanic_train <- na.omit(titanic_train)


#split data
set.seed(42)
n <- nrow(titanic_train)
id <- sample(1:n, size = n * 0.8)
train_data <- titanic_train[id, ]
test_data <- titanic_train[-id, ]

#Train data 
model_titanic <- glm(survived ~ pclass, data = train_data,
                     family = "binomial")
model_titanic

#p-value
summary(model_titanic)

p_train <- predict(model_titanic, 
                    type = "response")
train_data$pred <- if_else(p_train >= 0.5,
                           1,0)
train_data$survived == train_data$pred
mean(train_data$survived == train_data$pred)

# Test data 
p_test <- predict(model_titanic,
                  newdata = test_data,
                   type = "response")
test_data$pred <- if_else(p_test >= 0.5,
                           1,0)
test_data$survived == test_data$pred
mean(test_data$survived == test_data$pred)

conM <- table(test_data$pred,
              test_data$survived,
              dnn = c("Predicted",
                      "Actual"))
conM

# Evaluate Model
accuracy <- (conM[1, 1] + conM[2, 2])/
  sum(conM)
cat("Accuracy", accuracy)

precision <- conM[2, 2] / (conM[2, 1] + 
                             conM[2, 2])
cat("Precision", precision)

recall <- conM[2, 2] / (conM[1, 2] + 
                          conM[2, 2])
cat("Recall", recall)

F1_Score <- 2 * (precision * recall) /
  (precision + recall)
cat("F1 Score", F1_Score)


######################
# Full Model
titanic_train <- titanic_train

# convert survived to factor
titanic_train$survived <- factor(
               titanic_train$survived,
               levels = c(0,1),
               labels = c("No survive",
                          "Survive"))

titanic_train$sex <- factor(
  titanic_train$sex,
  levels = c("male", "female"),
  labels = c(0, 1))

#split data
set.seed(42)
n <- nrow(titanic_train)
id <- sample(1:n, size = n * 0.7)
train_data <- titanic_train[id, ]
test_data <- titanic_train[-id, ]

#Train data 
model_titanic <- glm(survived ~ pclass +
          age + sib_sp + sex + parch, 
          data = train_data,
          family = "binomial")

model_titanic

coef(model_titanic)
#p-value
summary(model_titanic)

p_train <- predict(model_titanic,
                   type="response")

train_data$pred_model_titanic <- if_else(p_train >= 0.5,
                                      1,0)
train_data$survived == train_data$pred_model_titanic
mean(train_data$survived == train_data$pred_model_titanic)

# Test data
p_test <- predict(model_titanic,
                  newdata = test_data,
                  type = "response")
test_data$pred_model_titanic <- if_else(p_test >= 0.5,
                          1,0)
test_data$survived == test_data$pred_model_titanic
mean(test_data$survived == test_data$pred_model_titanic)

# confusion matrix
conM <- table(test_data$pred_model_titanic,
              test_data$survived,
              dnn = c("Predicted", 
                      "Actual"))
conM

accuracy <- (conM[1, 1] + conM[2, 2]) /
  sum(conM)
precision <- conM[2, 2] / (conM[2 ,1] +
                             conM[2, 2])
recall <- conM[2, 2] / (conM[1, 2] +
                          conM[2, 2])
F1_Score <- 2 * (precision * recall) /
  (precision + recall)

cat("Accuracy:", accuracy, 
    "\nPrecision:", precision,
    "\nRecall:", recall,
    "\nF1 Score:", F1_Score)










