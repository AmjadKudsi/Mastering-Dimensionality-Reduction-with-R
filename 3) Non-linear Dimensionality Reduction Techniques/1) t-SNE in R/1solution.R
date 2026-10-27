library(Rtsne)
library(ggplot2)
library(datasets)
library(dplyr)

# Load the Iris dataset
data(iris)
data <- as.matrix(iris[, 1:4])
target <- iris$Species

# Remove duplicate rows (required by Rtsne)
unique_idx <- !duplicated(data)
data_unique <- data[unique_idx, ]
target_unique <- target[unique_idx]

# Run t-SNE
tsne <- Rtsne(data_unique, dims = 2, perplexity = 30, verbose = TRUE)

# Prepare data for plotting in the requested style
original_df <- data.frame(
  x = data_unique[, 1],
  y = data_unique[, 2],
  cluster = target_unique,
  method = "Original Data"
)

tsne_df <- data.frame(
  x = tsne$Y[, 1],
  y = tsne$Y[, 2],
  cluster = target_unique,
  method = "t-SNE"
)

all_data <- rbind(original_df, tsne_df)

# Set a color palette for the species
palette <- c(
  "setosa" = "#1b9e77",
  "versicolor" = "#d95f02",
  "virginica" = "#7570b3"
)

# Plot using facets in the requested style
plot <- ggplot(all_data, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 2.5, alpha = 0.9) +
  facet_wrap(~ method, nrow = 1) +
  scale_color_manual(values = palette, name = "Species") +
  labs(
    title = "t-SNE Visualization of the Iris Dataset",
    subtitle = "Comparison of Original Features vs t-SNE Embedding",
    x = "Component 1", y = "Component 2"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold", size = 15),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    legend.position = "right"
  )