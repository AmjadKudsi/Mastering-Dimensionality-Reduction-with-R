# Complete the missing pieces of the function that handles the PCA transformation for our space data.

library(ggplot2)

# Function to standardize the data
standardize <- function(X) {
  scale(X, center = TRUE, scale = TRUE)
}

# Function to compute the covariance matrix
compute_covariance_matrix <- function(X) {
  cov(X)
}

# PCA function
PCA <- function(X, num_components) {
  # TODO: Apply standardization to X
  # TODO: Compute the covariance matrix from X
  X_std <- standardize(X)
  covariance_matrix <- compute_covariance_matrix(X_std)
  eig <- eigen(covariance_matrix)
  idx <- order(eig$values, decreasing = TRUE)
  print("Eigenvalues:")
  print(eig$values)
  print("Eigenvectors:")
  print(eig$vectors)
  # TODO: Select the eigenvectors corresponding to the num_components largest eigenvalues
  idx <- order(eig$values, decreasing = TRUE)
  eigenvalues <- eig$values[idx][1:num_components]
  eigenvectors <- eig$vectors[, idx][, 1:num_components, drop = FALSE]
  list(eigenvalues = eigenvalues, eigenvectors = eigenvectors)
}

# Example data
data <- data.frame(
  weight_lbs = c(145, 170, 175, 180, 185),
  height_inches = c(67, 68, 69, 70, 72),
  height_cm = c(170.18, 172.72, 175.26, 177.80, 182.88)
)

# Applying PCA from 3D to 2D
X <- as.matrix(data[, c("height_inches", "height_cm", "weight_lbs")])
pca_result <- PCA(X, 2)
eigenvectors <- pca_result$eigenvectors
# The next line will project our data into the new PCA dimensions
X_pca <- X %*% eigenvectors

# Scatter plot of the two principal components
combined_df <- data.frame(
  PC1 = X_pca[, 1],
  PC2 = X_pca[, 2],
  Type = "Data after PCA"
)

plot <- ggplot(combined_df, aes(x = PC1, y = PC2, color = Type)) +
  geom_point() +
  labs(
    x = "Principal Component 1",
    y = "Principal Component 2",
    title = "Scatter Plot after PCA Reduction"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Assign the plot to a variable
plot