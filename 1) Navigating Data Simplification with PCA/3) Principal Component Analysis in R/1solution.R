library(stats)
library(ggplot2)

# Define the dataset
data <- data.frame(
  weight_kgs = c(80, 85, 88, 92, 98, 100, 105, 110, 115, 120),
  height_cm = c(180, 183, 185, 187, 190, 188, 192, 195, 200, 205),
  BMI = c(24.69, 25.38, 25.75, 26.32, 27.04, 28.24, 28.58, 30.46, 31.25, 31.75)
)

# Standardize the data
data_scaled <- as.data.frame(scale(data))

# Perform PCA
pca_result <- prcomp(data_scaled, center = FALSE, scale. = FALSE)

# Project the data onto the first two principal components
pca_scores <- pca_result$x[, 1:2]

# View explained variance ratio
explained_variance <- summary(pca_result)$importance[2, 1:2]
print(paste("Explained Variance: ", explained_variance))

# Create a data frame for plotting
plot_data <- as.data.frame(pca_scores)
colnames(plot_data) <- c("PC1", "PC2")

# Visualize the PCA results
plot <- ggplot(plot_data, aes(x = PC1, y = PC2)) +
  geom_point(size = 3) +
  labs(
    x = "Principal Component 1",
    y = "Principal Component 2",
    title = "PCA on Simplified Sensor Data"
  ) +
  theme_bw() +
  theme(panel.grid = element_line())