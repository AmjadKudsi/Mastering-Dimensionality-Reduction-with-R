# Load necessary libraries
library(MASS)
library(ggplot2)

# Load the iris dataset
data(iris)

# Scale the features to zero mean and unit variance
X <- scale(iris[, 1:4])
y <- iris$Species

# Split the dataset into training and testing data
X_train <- X[1:120, ]
y_train <- y[1:120]
X_test <- X[121:150, ]
y_test <- y[121:150]

# Apply LDA with MASS
train_data <- data.frame(X_train, Species = y_train)
lda_model <- lda(Species ~ ., data = train_data)
lda_pred <- predict(lda_model, train_data)
X_lda <- lda_pred$x

# Visualize LDA results
plot_data <- data.frame(X_lda, Species = y_train)
plot <- ggplot(plot_data, aes(x = LD1, y = LD2, color = Species)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "LDA: Projected data onto the first two components",
    x = "Discriminant 1",
    y = "Discriminant 2"
  ) +
 theme_minimal() + theme( panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA) )

# Use LDA as a classifier to predict and calculate accuracy
test_data <- data.frame(X_test)
y_pred <- predict(lda_model, test_data)$class
accuracy <- mean(y_pred == y_test)
print(sprintf("Accuracy: %.2f", accuracy))