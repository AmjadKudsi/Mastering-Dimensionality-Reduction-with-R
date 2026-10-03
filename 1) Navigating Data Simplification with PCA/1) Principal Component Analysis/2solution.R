# Reduce the three dimensional dataset to two dimensions using PCA with two principal components.
# Project the data onto PC1 and PC2, then visualize the transformed data in a 2D scatter plot.

library(ggplot2)

# Standardizing the data
standardize <- function(X) {
  scale(X, center = TRUE, scale = TRUE)
}

# Computing the covariance matrix
compute_covariance_matrix <- function(X) {
  cov(X)
}

# Principal Component Analysis with specified number of components
PCA <- function(X, num_components) {
  X <- standardize(X)
  covariance_matrix <- compute_covariance_matrix(X)
  eig <- eigen(covariance_matrix)
  idx <- order(eig$values, decreasing = TRUE)
  eigenvalues <- eig$values[idx][1:num_components]
  eigenvectors <- eig$vectors[, idx][, 1:num_components, drop = FALSE]
  list(eigenvalues = eigenvalues, eigenvectors = eigenvectors)
}

# Data preparation with syntactic column names
data <- data.frame(
  weight_lbs = c(150, 160, 155, 165, 170, 160, 158, 175, 180, 170),
  height_inches = c(68, 72, 66, 69, 71, 65, 67, 70, 73, 68),
  height_cm = c(172.72, 182.88, 167.64, 175.26, 180.34, 165.1, 170.18, 177.8, 185.42, 172.72)
)

# Performing PCA on 'weight_lbs', 'height_inches', and 'height_cm' features specifying two components
X <- as.matrix(data[, c("weight_lbs", "height_inches", "height_cm")])

# TODO: Change the number of components for PCA to 2
pca_result <- PCA(X, 2)
eigenvectors <- pca_result$eigenvectors

# Projecting onto the first two principal components
X_pca <- X %*% eigenvectors

# Combine original and PCA-transformed data for plotting
combined_df <- rbind(
  data.frame(
    PC1 = X_pca[, 1],
    PC2 = X_pca[, 2],
    Type = "Data after PCA"
  )
)

# Plotting the PCA-transformed data
# TODO: Update the plot to show two Principal Components.
plot <- ggplot(combined_df, aes(x = PC1, y = PC2, color = Type)) +
  geom_point() +
  labs(
    x = "Principal Component 1",
    y = "Principal Component 2",
    title = "Scatter Plot of 2 Principal Components"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Assign the plot to a variable
plot