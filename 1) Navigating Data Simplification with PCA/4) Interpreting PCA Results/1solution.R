library(stats)
library(ggplot2)

set.seed(42)

# Generate a synthetic dataset
n_samples <- 100
X <- matrix(rnorm(n_samples * 5), n_samples, 5)
y <- sample(0:1, n_samples, replace = TRUE)

# Apply PCA to reduce to 2 components
pca <- prcomp(X, center = TRUE, scale. = TRUE)
X_pca <- pca$x[, 1:2]

# Split the dataset into training and testing sets
train_indices <- sample(seq_len(nrow(X_pca)), size = 0.7 * nrow(X_pca))
X_train_pca <- X_pca[train_indices, ]
X_test_pca <- X_pca[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

# Train a logistic regression model on the PCA-transformed data
model_pca <- glm(y_train ~ ., data = as.data.frame(X_train_pca), family = binomial)

# Prepare data for plotting
df_train_pca <- as.data.frame(X_train_pca)
colnames(df_train_pca) <- c("PC1", "PC2")
df_train_pca$y_train <- factor(y_train)

# Plot the PCA-transformed data
plot <- ggplot(df_train_pca, aes(x = PC1, y = PC2, color = y_train)) +
  geom_point() +
  labs(x = "First principal component", y = "Second principal component", title = "PCA-transformed data visualization") +
  theme_bw()

# Predict on the test data
prob_pred_pca <- predict(model_pca, newdata = as.data.frame(X_test_pca), type = "response")
y_pred_pca <- ifelse(prob_pred_pca > 0.5, 1, 0)

# Print the predicted labels on test data
cat("Predicted labels on test data:", y_pred_pca, "\n")