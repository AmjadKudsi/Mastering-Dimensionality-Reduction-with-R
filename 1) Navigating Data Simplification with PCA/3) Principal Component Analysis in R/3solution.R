library(stats)
library(ggplot2)

# Define the dataset with updated values
data <- data.frame(
  Feature_1 = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
  Feature_2 = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
  Feature_3 = c(5, 4, 6, 5, 7, 6, 8, 7, 9, 8)
)

# TODO: Standardize the data
data_scaled <- as.data.frame(scale(data))

# TODO: Perform PCA on the standardized data
pca_result <- prcomp(data_scaled, center = FALSE, scale. = FALSE)
pca_scores <- pca_result$x[, 1:2]

# View explained variance ratio
# TODO: Print the explained variance ratio
summary_pca <- summary(pca_result)
explained_variance <- summary_pca$importance[2, 1:2]
print(explained_variance)

# Visualize the PCA results
# TODO: Plot the PCA results using ggplot2
plot_data <- as.data.frame(pca_scores)
colnames(plot_data) <- c("PC1", "PC2")

ggplot(plot_data, aes(x = PC1, y = PC2)) +
  geom_point(size = 3) +
  labs(
    x = "Principal Component 1",
    y = "Principal Component 2",
    title = "PCA on Measurement Dataset"
  ) +
  theme_bw() +
  theme(panel.grid = element_line())