library(stats)
library(ggplot2)

set.seed(42)

# Construct a dataset with 3 features height, weight and BMI with 100 samples
height <- rnorm(100, mean = 170, sd = 10)
weight <- rnorm(100, mean = 70, sd = 5)
bmi <- weight / (height / 100) ^ 2

# Create a binary target variable based on BMI
bmi_threshold <- 25
target <- bmi > bmi_threshold

# Create a data frame
df <- data.frame(Height = height, Weight = weight, BMI = bmi, Overweight = target)

# Take the data and target values for Logistic Regression
data <- df
target <- data$Overweight
data$Overweight <- NULL

# Split the data
set.seed(42)
train_indices <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
x_train <- data[train_indices, ]
x_test <- data[-train_indices, ]
y_train <- target[train_indices]
y_test <- target[-train_indices]

# Applying PCA to reduce to 1 dimension
pca <- prcomp(x_train, center = TRUE, scale. = TRUE)
x_train_pca <- pca$x[, 1, drop = FALSE]
x_test_pca <- predict(pca, newdata = x_test)[, 1, drop = FALSE]

# Fitting Logistic Regression to the PCA-transformed data
model_pca <- suppressWarnings(glm(y_train ~ ., data = as.data.frame(x_train_pca), family = binomial))
y_pred_pca <- suppressWarnings(predict(model_pca, newdata = as.data.frame(x_test_pca), type = "response"))
y_pred <- ifelse(y_pred_pca > 0.5, TRUE, FALSE)

# Printing predictions
cat("Predictions on test set:", y_pred, "\n")

# Evaluate the model
accuracy <- mean(y_pred == y_test)
cat("Accuracy:", round(accuracy, 2), "\n")
