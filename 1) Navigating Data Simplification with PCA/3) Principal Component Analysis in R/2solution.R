
library(stats)
library(ggplot2)

# Define the dataset
data <- data.frame(
  Feature_A = c(50, 60, 70, 80, 90, 100, 110, 120, 130, 140),
  Feature_B = c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75),
  Feature_C = c(200, 210, 220, 230, 240, 250, 260, 270, 280, 290)
)

# Standardize the data
data_scaled <- as.data.frame(scale(data))

# TODO: Update the code to reduce to 2 dimensions.
pca_result <- prcomp(data_scaled, center = FALSE, scale. = FALSE, rank. = 2)

# Project the data onto the first two principal components
pca_scores <- pca_result$x[, 1:2]

# View explained variance ratio
explained_variance <- summary(pca_result)$importance[2, 1:2]
print(paste("Explained Variance: ", explained_variance))

# Plot both principal components on a 2D plot
# TODO: Update the plot to show the second principal component as well.
plot_data <- data.frame(PC1 = pca_scores[, 1], PC2 = pca_scores[, 2])

plot <- ggplot(plot_data, aes(x = PC1, y = PC2)) +
  geom_point(size = 3) +
  labs(
    x = "Principal Component 1",
    y = "Principal Component 2",
    title = "PCA Result with 2 Components"
  ) +
  theme_bw() +
  theme(panel.grid = element_line())

print(plot)
