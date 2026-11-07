library(ggplot2)
library(kernlab)
library(caret)

# Create a non-linearly separable dataset
set.seed(42)
n_samples <- 100
noise <- 0.05
X <- matrix(0, n_samples, 2)
y <- rep(0, n_samples)

# Generate moon-shaped data
for (i in 1:(n_samples/2)) {
  angle <- runif(1, 0, pi)
  X[i, ] <- c(cos(angle), sin(angle)) + rnorm(2, sd = noise)
  y[i] <- 1
}
for (i in (n_samples/2 + 1):n_samples) {
  angle <- runif(1, 0, pi)
  X[i, ] <- c(1 - cos(angle), 1 - sin(angle) - 0.5) + rnorm(2, sd = noise)
  y[i] <- 2
}

data <- data.frame(X1 = X[, 1], X2 = X[, 2], class = factor(y))

# Split the dataset into training and testing sets
set.seed(42)
train_index <- createDataPartition(data$class, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# TODO: Instantiate and fit KernelPCA on the training data to reduce the data to 2 components.
# TODO: Use 'rbfdot' for kernel. Set sigma to 1/15.
set.seed(42)
kpca_model <- kpca(
  ~.,
  data = train_data[, c("X1", "X2")],
  kernel = "rbfdot",
  kpar = list(sigma = 1/15),
  features = 2
)

# Transform the test data and plot the projection
test_features <- as.matrix(test_data[, c("X1", "X2")])
test_proj <- predict(kpca_model, test_features)
test_proj_df <- as.data.frame(test_proj)
colnames(test_proj_df) <- c("PC1", "PC2")
test_proj_df$class <- test_data$class

plot <- ggplot(test_proj_df, aes(x = PC1, y = PC2, color = class)) +
  geom_point(alpha = 0.7) +
  labs(title = "Projection onto the first two principal components using Kernel PCA",
       x = "1st principal component", y = "2nd principal component") +
  theme_bw()

print(plot)

# Inversely transform the training data back to original space
X_train_inverse_transformed <- pcv(kpca_model)

# Calculate the reconstruction mean squared error for the training set
mse <- mean((as.matrix(train_data[, c("X1", "X2")]) -
             X_train_inverse_transformed)^2)
cat("Mean squared error for Kernel PCA is:", mse, "\n")