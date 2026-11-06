# ---- Dimensionality Reduction: LLE on Swiss Roll (R) ----
# install.packages("ggplot2")  # run once if needed
library(ggplot2)

# ---------------------------
# Generate Swiss Roll data (same generator as lesson)
# ---------------------------
generate_swiss_roll <- function(n_samples = 1500, noise = 0.05, seed = 42) {
  set.seed(seed)
  t <- 1.5 * pi * (1 + 2 * runif(n_samples))
  h <- 21 * runif(n_samples)
  X <- cbind(
    x = t * cos(t) + rnorm(n_samples, sd = noise),
    y = h           + rnorm(n_samples, sd = noise),
    z = t * sin(t) + rnorm(n_samples, sd = noise)
  )
  list(X = X, color = t)  # continuous color, same as lesson
}

# --- Task-specific settings (only differences from lesson) ---
n_samples <- 800
noise <- 0.1
swiss <- generate_swiss_roll(n_samples, noise, seed = 42)
X <- swiss$X
color <- swiss$color

# ---------------------------
# Minimal LLE (same implementation & signature as lesson)
# ---------------------------
lle <- function(X, k = 10, d = 2, reg = 1e-3) {
  X <- as.matrix(X)
  n <- nrow(X)
  D2 <- as.matrix(dist(X))^2

  # k-nearest neighbors (exclude self)
  Nbrs <- t(apply(D2, 1, function(row) order(row)[2:(k + 1)]))

  # Reconstruction weights
  W <- matrix(0, n, n)
  for (i in 1:n) {
    idx <- Nbrs[i, ]
    Xi  <- X[idx, , drop = FALSE]
    xi  <- X[i,  , drop = FALSE]

    Z <- sweep(Xi, 2, xi)
    C <- Z %*% t(Z)
    C <- C + diag(reg * sum(diag(C)), k)  # regularization

    ones <- rep(1, k)
    w <- solve(C, ones)
    w <- w / sum(w)

    W[i, idx] <- w
  }

  # Embedding: bottom non-trivial eigenvectors of M = (I - W)'(I - W)
  I <- diag(n)
  M <- t(I - W) %*% (I - W)
  ev <- eigen(M, symmetric = TRUE)
  ord <- order(ev$values, decreasing = FALSE)
  Y <- ev$vectors[, ord[2:(d + 1)], drop = FALSE]

  list(Y = Y, W = W)
}

# --- Run LLE with task's neighbor count (k = 10) ---
k <- 10
d <- 2
reg <- 1e-3

set.seed(42)
lle_fit <- lle(X, k = k, d = d, reg = reg)
X_lle <- lle_fit$Y

# ---------------------------
# Build dataframe for ggplot (same fields & facet layout)
# ---------------------------
df <- data.frame(x = X_lle[,1], y = X_lle[,2], color = color, method = "LLE (k=10)")

# ---------------------------
# Plot (white background, same theme style as lesson)
# ---------------------------
plot <- ggplot(df, aes(x, y, color = color)) +
  geom_point(size = 0.8) +
  scale_color_viridis_c() +
  labs(
    title = "LLE on Swiss Roll",
    x = "Component 1", y = "Component 2", color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = NA),
    plot.title       = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.text       = element_text(face = "bold", size = 14)
  )

print(plot)

# TODO: Print the reconstruction error for LLE
reconstruction_error <- sum((X_lle - lle_fit$W %*% X_lle)^2)

cat("LLE Reconstruction Error:", reconstruction_error, "\n")