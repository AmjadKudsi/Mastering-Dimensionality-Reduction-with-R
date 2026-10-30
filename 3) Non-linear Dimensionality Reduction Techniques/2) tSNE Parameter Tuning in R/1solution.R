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

# t-SNE with default-like parameters
tsne_1 <- Rtsne(
  as.matrix(X),
  dims = 2,
  perplexity = 30,
  exaggeration_factor = 12,
  eta = 200,
  pca = FALSE,
  check_duplicates = FALSE,
  verbose = FALSE
)
tsne_transformed_1 <- as.data.frame(tsne_1$Y)
colnames(tsne_transformed_1) <- c("tsne1", "tsne2")
tsne_transformed_1$class <- y
tsne_transformed_1$method <- "Default parameters"

# t-SNE with custom (more aggressive) parameters
set.seed(42)
tsne_2 <- Rtsne(
  as.matrix(X),
  dims = 2,
  perplexity = 50,
  exaggeration_factor = 20,
  eta = 500,
  pca = FALSE,
  check_duplicates = FALSE,
  verbose = FALSE
)
tsne_transformed_2 <- as.data.frame(tsne_2$Y)
colnames(tsne_transformed_2) <- c("tsne1", "tsne2")
tsne_transformed_2$class <- y
tsne_transformed_2$method <- "Aggresive Tuning"

# Combine results for faceted plotting
all_tsne <- rbind(tsne_transformed_1, tsne_transformed_2)

# Plot using facets in ggplot2
tsne_plot <- ggplot(all_tsne, aes(x = tsne1, y = tsne2, color = class)) +
  geom_point(size = 2) +
  facet_wrap(~ method, nrow = 1) +
  labs(
    title = "t-SNE Parameter Tuning in R",
    subtitle = "Comparison of Default vs Custom Parameters",
    x = "t-SNE 1", y = "t-SNE 2", color = "Class"
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