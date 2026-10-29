
library(Rtsne)
library(ggplot2)
library(datasets)

# Load the Iris dataset
data(iris)
data <- as.matrix(iris[, 1:4])
target <- iris$Species

# Remove duplicate rows (required by Rtsne)
unique_idx <- !duplicated(data)
data_unique <- data[unique_idx, ]
target_unique <- target[unique_idx]

# Initialize t-SNE with 2 components
tsne <- Rtsne(data_unique, dims = 2, perplexity = 30, verbose = TRUE)

# TODO: Prepare data for plotting and assign the result to a variable named `tsne_df`
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

palette <- c(
  "setosa" = "#1b9e77",
  "versicolor" = "#d95f02",
  "virginica" = "#7570b3"
)

# Visualize the transformed data with a scatter plot
plot <- ggplot(tsne_df, aes(x = x, y = y, color = cluster)) +  # TODO: Replace blanks with the correct references
  geom_point(size = 2.5, alpha = 0.9) +
  labs(
    title = "Iris dataset visualized with t-SNE",
    x = "t-SNE feature 1", y = "t-SNE feature 2"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right"
  )

print(plot)
