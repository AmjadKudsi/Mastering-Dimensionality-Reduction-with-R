library(MASS)
library(caret)
library(ggplot2)
library(nnet)

# Load iris dataset
data(iris)

# Standardizing the features
sc <- preProcess(iris[, 1:4], method = c("center", "scale"))
X <- predict(sc, iris[, 1:4])
y <- iris$Species

# Splitting the dataset into training and testing sets
set.seed(0)
trainIndex <- createDataPartition(y, p = 0.4, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# TODO: Initialize LDA, fit it to the training data along with the class labels, and transform both training and testing sets
lda_model <- lda(x = X_train, grouping = y_train)

X_train_lda <- predict(lda_model, X_train)$x
X_test_lda <- predict(lda_model, X_test)$x

# TODO: Train the Logistic Regression model using the original and transformed LDA data and predict with the testing set

# Logistic Regression with original data
model_original <- multinom(
  y_train ~ .,
  data = data.frame(y_train, X_train),
  trace = FALSE
)

y_pred_original <- predict(
  model_original,
  newdata = X_test,
  type = "class"
)

# Logistic Regression with LDA transformed data
model_lda <- multinom(
  y_train ~ .,
  data = data.frame(y_train, X_train_lda),
  trace = FALSE
)

y_pred_lda <- predict(
  model_lda,
  newdata = as.data.frame(X_test_lda),
  type = "class"
)

# Printing accuracy scores
cat("Accuracy of Logistic Regression with original data:", mean(y_pred_original == y_test), "\n")
cat("Accuracy of Logistic Regression with LDA transformed data:", mean(y_pred_lda == y_test), "\n")