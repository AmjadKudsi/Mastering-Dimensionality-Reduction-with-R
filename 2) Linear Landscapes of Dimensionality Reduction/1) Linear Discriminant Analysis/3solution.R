
library(MASS)
library(ggplot2)

# Load the iris dataset
data(iris)

# Rename features for demonstration
colnames(iris)[1:3] <- c("weight_lbs", "weight_kgs", "height")

# Extract features and target
X <- as.matrix(iris[, c("weight_lbs", "weight_kgs", "height", "Petal.Width")])
y <- as.numeric(iris$Species)

# Normalize the data to zero mean and unit variance
X <- scale(X)

within_class_scatter_matrix <- function(X, y) {
  class_labels <- unique(y)
  n_features <- ncol(X)
  S_W <- matrix(0, n_features, n_features)

  for (class_id in class_labels) {
    X_class <- X[y == class_id, , drop = FALSE]
    mean_vec <- colMeans(X_class)
    class_sc_mat <- matrix(0, n_features, n_features)
    for (i in 1:nrow(X_class)) {
      row <- as.matrix(X_class[i, ])
      mv <- as.matrix(mean_vec)
      diff <- row - mv
      class_sc_mat <- class_sc_mat + diff %*% t(diff)
    }
    S_W <- S_W + class_sc_mat
  }
  return(S_W)
}

between_class_scatter_matrix <- function(X, y) {
  class_labels <- unique(y)
  n_features <- ncol(X)
  overall_mean <- colMeans(X)
  S_B <- matrix(0, n_features, n_features)

  for (class_id in class_labels) {
    X_class <- X[y == class_id, , drop = FALSE]
    mean_vec <- colMeans(X_class)
    n <- nrow(X_class)
    mean_diff <- as.matrix(mean_vec - overall_mean)
    S_B <- S_B + n * (mean_diff %*% t(mean_diff))
  }
  return(S_B)
}

fit_lda <- function(X, y, n_components) {
  S_W <- within_class_scatter_matrix(X, y)
  S_B <- between_class_scatter_matrix(X, y)

  # TODO: Solve the generalized eigenvalue problem using eigen and solve functions
  eigen_result <- eigen(solve(S_W, S_B))

  # TODO: Sort the eigenvectors by eigenvalues in descending order
  sorted_indices <- order(Re(eigen_result$values), decreasing = TRUE)
  sorted_eigenvectors <- eigen_result$vectors[, sorted_indices, drop = FALSE]

  # TODO: Form the transformation matrix W using the eigenvectors corresponding to the n_components highest eigenvalues
  W <- Re(sorted_eigenvectors[, 1:n_components, drop = FALSE])

  return(W)
}

transform_lda <- function(X, W) {
  return(X %*% W)
}

# Fit LDA and transform data
n_components <- 2
W <- fit_lda(X, y, n_components)
X_lda <- transform_lda(X, W)

# Prepare data for plotting
lda_df <- data.frame(
  LD1 = X_lda[, 1],
  LD2 = X_lda[, 2],
  Species = iris$Species
)

# Plot the transformed data
plot <- ggplot(lda_df, aes(x = LD1, y = LD2, color = Species)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "LDA: Projected data onto the first two components",
    x = "LD1",
    y = "LD2"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(plot)