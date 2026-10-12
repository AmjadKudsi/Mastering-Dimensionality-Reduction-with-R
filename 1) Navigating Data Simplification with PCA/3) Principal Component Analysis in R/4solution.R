
library(stats)
library(ggplot2)

# Define the dataset with updated values
data <- data.frame(
  Weight_kgs = c(80, 85, 88, 92, 98, 100, 105, 110, 115, 120, 125, 130, 135, 140),
  Height_cm = c(180, 183, 185, 187, 190, 188, 192, 195, 200, 180, 183, 185, 187, 190),
  BMI = c(24.69, 25.38, 25.75, 26.32, 27.04, 28.24, 28.58, 30.46, 31.25, 31.75, 32.34, 33.01, 34.01, 35.97)
)

# TODO: Standardize the dataset using scale function
data_scaled <- as.data.frame(scale(data))

# TODO: Perform PCA using prcomp and transform the dataset
pca_result <- prcomp(data_scaled, center = FALSE, scale. = FALSE)
plot_data <- as.data.frame(pca_result$x)

# TODO: Print the variance explained by each of the principal components
variance_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100
print(variance_explained)

# Visualize the results of PCA
plot <- ggplot(plot_data, aes(x = PC1, y = PC2)) +
  geom_point(size = 3) +
  labs(
    x = "Principal Component 1",
    y = "Principal Component 2",
    title = "PCA on Toy Dataset"
  ) +
  theme_bw() +
  theme(panel.grid = element_line())

print(plot)
