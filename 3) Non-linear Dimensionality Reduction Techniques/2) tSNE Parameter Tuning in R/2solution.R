# Load required libraries
library(ggplot2)
library(Rtsne)

# Generate a non-linearly separable dataset (two circles)
set.seed(42)
n_samples <- 500
factor <- 0.3
noise  <- 0.1

half <- n_samples / 2
theta_outer <- runif(half, 0, 2*pi)
theta_inner <- runif(half, 0, 2*pi)

r_outer <- 1 + rnorm(half, sd = noise)
r_inner <- factor + rnorm(half, sd = noise)

X <- rbind(
  cbind(r_outer * cos(theta_outer), r_outer * sin(theta_outer)),
  cbind(r_inner * cos(theta_inner), r_inner * sin(theta_inner))
)
X <- as.data.frame(X)
colnames(X) <- c("x1", "x2")
y <- as.factor(c(rep(0, half), rep(1, half)))

# t-SNE with perplexity = 30
set.seed(42)
tsne_result <- Rtsne(
  as.matrix(X),
  dims = 2,
  perplexity = 5,
  exaggeration_factor = 20,
  eta = 500,
  pca = FALSE,
  check_duplicates = FALSE,
  verbose = FALSE
)
tsne_transformed <- as.data.frame(tsne_result$Y)
colnames(tsne_transformed) <- c("tsne1", "tsne2")
tsne_transformed$class <- y

# Plot the t-SNE result
tsne_plot <- ggplot(tsne_transformed, aes(x = tsne1, y = tsne2, color = class)) +
  geom_point(size = 2) +
  labs(
    title = "t-SNE Result with Perplexity = 30",
    x = "t-SNE 1", y = "t-SNE 2", color = "Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    legend.position = "right"
  )