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

# Applying PCA to reduce to 2 dimensions
pca <- prcomp(x_train, center = TRUE, scale. = TRUE)
x_train_pca <- pca$x[, 1:2]
x_test_pca <- predict(pca, newdata = x_test)[, 1:2]

# TODO: Train the Logistic Regression model with PCA-reduced training data and the target variable
train_data <- data.frame(x_train_pca, Overweight = y_train)
model <- glm(Overweight ~ PC1 + PC2, data = train_data, family = binomial)

# TODO: Predict the labels for the PCA-reduced testing data using the trained model
predicted_probabilities <- predict(model, newdata = as.data.frame(x_test_pca), type = "response")
predicted_labels <- predicted_probabilities > 0.5

# Visualizing the PCA-reduced data (2D scatter plot for the first two principal components)
plot <- ggplot(as.data.frame(x_train_pca), aes(x = PC1, y = PC2, color = as.factor(y_train))) +
  geom_point() +
  labs(x = "First principal component", y = "Second principal component", color = "Overweight")

print(plot)

# TODO: Calculate and print the accuracy of the Logistic Regression model.
accuracy <- mean(predicted_labels == y_test)
cat("Logistic Regression Accuracy:", accuracy * 100, "%\n")
