library(Rtsne)
library(ggplot2)

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

# TODO: Create a t-SNE instance with a perplexity value of your choice, a learning rate, and set the random_state for reproducibility
set.seed(42)
perplexity <- 30
learning_rate <- 200

# TODO: Apply t-SNE to the dataset (no standardization needed)
tsne_result <- Rtsne(
  as.matrix(X),
  dims = 2,
  perplexity = perplexity,
  eta = learning_rate,
  max_iter = 1000,
  theta = 0.5,
  pca = FALSE,
  normalize = FALSE
)

tsne_data <- data.frame(
  tSNE1 = tsne_result$Y[, 1],
  tSNE2 = tsne_result$Y[, 2],
  Class = y
)

# TODO: Use ggplot2 to create a scatter plot of the transformed data
p <- ggplot(tsne_data, aes(x = tSNE1, y = tSNE2, color = Class)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(
    title = "t-SNE Visualization of Two Circles Dataset",
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2",
    color = "Class"
  ) +
  theme_minimal()

print(p)

dir.create("static/images", recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = "static/images/tsne_plot.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)