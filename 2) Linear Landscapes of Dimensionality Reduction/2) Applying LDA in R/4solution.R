
library(MASS)
library(ggplot2)

# TODO: Load the Iris dataset and store the feature in X and the target in y
data(iris)
X <- iris[, 1:4]
y <- iris$Species

# TODO: Manually scale the features of the dataset
X_scaled <- as.data.frame(scale(X))

# TODO: Manually split the dataset into training and test sets using slicing
set.seed(42)

# Shuffle the dataset indices
indices <- sample(1:nrow(X_scaled))

# Use 80% for training and 20% for testing
train_size <- floor(0.8 * nrow(X_scaled))
train_indices <- indices[1:train_size]
test_indices <- indices[(train_size + 1):nrow(X_scaled)]

X_train <- X_scaled[train_indices, ]
X_test <- X_scaled[test_indices, ]

y_train <- y[train_indices]
y_test <- y[test_indices]

# TODO: Apply Linear Discriminant Analysis (LDA) to the training data and transform it
lda_model <- lda(x = X_train, grouping = y_train)

# Transform training data into LDA space
lda_train <- predict(lda_model, X_train)$x

# Visualize LDA results
# TODO: Prepare the data for plotting and create a scatter plot of the first two linear discriminants
lda_df <- data.frame(
  LD1 = lda_train[, 1],
  LD2 = lda_train[, 2],
  Species = y_train
)

lda_plot <- ggplot(lda_df, aes(x = LD1, y = LD2, color = Species)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "LDA Projection of Iris Dataset",
    x = "Linear Discriminant 1",
    y = "Linear Discriminant 2"
  ) +
  theme_minimal()

print(lda_plot)

# TODO: Predict the classes for the test set using LDA and calculate the accuracy
lda_predictions <- predict(lda_model, X_test)

# Extract predicted classes
y_pred <- lda_predictions$class

# Calculate classification accuracy
accuracy <- mean(y_pred == y_test)

# Display accuracy
cat("LDA Projection Accuracy:", round(accuracy * 100, 2), "%\n")

# Display confusion matrix
confusion_matrix <- table(
  Actual = y_test,
  Predicted = y_pred
)
print(confusion_matrix)