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
  weight_kg = c(68, 72, 66, 69, 71, 65, 67, 70, 73, 68)
)

# Performing PCA on 'weight_lbs' and 'weight_kg' features specifying one component
X <- as.matrix(data[, c("weight_lbs", "weight_kg")])
pca_result <- PCA(X, 1)
eigenvectors <- pca_result$eigenvectors

# Projecting onto the first principal component
X_pca <- X %*% eigenvectors

# Combine original and PCA-transformed data for plotting
combined_df <- rbind(
  data.frame(
    weight_lbs = X[, 1],
    weight_kg = X[, 2],
    Type = "Original Data"
  ),
  data.frame(
    weight_lbs = X_pca[, 1],
    weight_kg = rep(0, nrow(X_pca)),
    Type = "Data after PCA"
  )
)

# Plotting the original and PCA-transformed data
plot <- ggplot(combined_df, aes(x = weight_lbs, y = weight_kg, color = Type)) +
  geom_point() +
  facet_wrap(~Type, scales = "free") +
  labs(
    x = "Weight (lbs)",
    y = "Weight (kg)",
    title = "PCA Transformation"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Assign the plot to a variable
plot