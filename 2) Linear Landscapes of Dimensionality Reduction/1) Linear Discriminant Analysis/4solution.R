library(MASS)
library(ggplot2)

# Load the iris dataset
data(iris)

# Scale the data to zero mean and unit variance
X <- scale(iris[, 1:4])
y <- as.numeric(iris$Species)

# Define LDA class
LDA <- list()
LDA$W <- NULL

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

  # Solve the generalized eigenvalue problem
  eig <- eigen(solve(S_W) %*% S_B)
  eig_vals <- eig$values
  eig_vecs <- eig$vectors

  # Sort eigenvectors by eigenvalues in descending order
  eig_pairs <- lapply(1:length(eig_vals), function(i) list(value = abs(eig_vals[i]), vector = eig_vecs[, i]))
  eig_pairs <- eig_pairs[order(sapply(eig_pairs, function(x) -x$value))]

  # Form the transformation matrix W
  W <- do.call(cbind, lapply(1:n_components, function(i) eig_pairs[[i]]$vector))

  return(W)
}

transform_lda <- function(X, W) {
  # TODO: Write the line of code that projects the data points onto the new LDA subspace
  return(X %*% W)
}

# TODO: Fit LDA and transform data
W <- fit_lda(X, y, n_components = 2)
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