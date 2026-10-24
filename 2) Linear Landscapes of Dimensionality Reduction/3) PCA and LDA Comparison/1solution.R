library(MASS)
library(caret)
library(ggplot2)
library(nnet)

# Load iris dataset
data(iris)

# Standardizing the features
sc <- preProcess(iris[, 1:4], method = c("center", "scale"))
X <- predict(sc, iris[, 1:4])

# Splitting the dataset into training and testing sets
set.seed(0)
trainIndex <- createDataPartition(iris$Species, p = 0.4, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- iris$Species[trainIndex]
y_test <- iris$Species[-trainIndex]

# Applying LDA
lda <- lda(Species ~ ., data = iris[trainIndex, ])
X_train_lda <- predict(lda, X_train)$x
X_test_lda <- predict(lda, X_test)$x

# Applying PCA
pca <- prcomp(X_train, center = TRUE, scale. = TRUE)
X_train_pca <- predict(pca, X_train)[, 1:2]
X_test_pca <- predict(pca, X_test)[, 1:2]

# Training the model using the original data
classifier <- multinom(Species ~ ., data = iris[trainIndex, ])
y_pred <- predict(classifier, X_test)
cat("Accuracy of Logistic Regression with original data:", mean(y_pred == y_test), "\n")

# Training the model using the LDA data
classifier_lda <- multinom(y_train ~ ., data = as.data.frame(X_train_lda))
y_pred_lda <- predict(classifier_lda, as.data.frame(X_test_lda))
cat("Accuracy of Logistic Regression with LDA transformed data:", mean(y_pred_lda == y_test), "\n")

# Training the model using the PCA data
classifier_pca <- multinom(y_train ~ ., data = as.data.frame(X_train_pca))
y_pred_pca <- predict(classifier_pca, as.data.frame(X_test_pca))
cat("Accuracy of Logistic Regression with PCA transformed data:", mean(y_pred_pca == y_test), "\n")