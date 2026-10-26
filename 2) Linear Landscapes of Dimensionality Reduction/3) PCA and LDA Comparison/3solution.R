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

# TODO: Apply Linear Discriminant Analysis on the training data
lda_model <- lda(y_train ~ ., data = as.data.frame(X_train))
X_train_lda <- predict(lda_model, as.data.frame(X_train))$x
X_test_lda <- predict(lda_model, as.data.frame(X_test))$x

# TODO: Train a logistic regression classifier using the LDA-transformed training data
logreg_lda_model <- multinom(y_train ~ ., data = as.data.frame(X_train_lda))
y_pred_lda <- predict(logreg_lda_model, as.data.frame(X_test_lda))
accuracy_lda <- mean(y_pred_lda == y_test)
print(paste("Accuracy with LDA:", round(accuracy_lda, 2))) # e.g., 0.96

# TODO: Apply Principal Component Analysis on the training data
pca_model <- prcomp(X_train, center = TRUE, scale. = TRUE)
summary(pca_model)

# TODO: Train a logistic regression classifier using the PCA-transformed training data
# Use first 2 PCs for illustration
X_train_pca <- predict(pca_model, X_train)[, 1:2]
X_test_pca <- predict(pca_model, X_test)[, 1:2]

logreg_pca_model <- multinom(y_train ~ ., data = as.data.frame(X_train_pca))
y_pred_pca <- predict(logreg_pca_model, as.data.frame(X_test_pca))

# TODO: Evaluate the accuracies of both classifiers on their respective test sets and print them

accuracy_pca <- mean(y_pred_pca == y_test)
print(paste("Accuracy with PCA:", round(accuracy_pca, 2))) # e.g., 0.84