library(kernlab)
library(ggplot2)
library(dplyr)
library(caret)

# Generate concentric circles data (manual make_circles-style)
set.seed(42)
n_samples   <- 150
ring_factor <- 0.3
noise       <- 0.04

half <- n_samples / 2
theta_outer <- runif(half, 0, 2*pi)
theta_inner <- runif(half, 0, 2*pi)

r_outer <- 1.0 + rnorm(half, sd = noise)
r_inner <- ring_factor + rnorm(half, sd = noise)

X <- rbind(
  cbind(r_outer * cos(theta_outer), r_outer * sin(theta_outer)),
  cbind(r_inner * cos(theta_inner), r_inner * sin(theta_inner))
)
colnames(X) <- c("feature_1", "feature_2")
y <- factor(c(rep("outer", half), rep("inner", half)))

data <- as.data.frame(X) %>% mutate(class = y)

# Split into training and testing sets
set.seed(42)
train_index <- createDataPartition(data$class, p = 0.75, list = FALSE)
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

# Apply Kernel PCA with Polynomial kernel
set.seed(42)
kpca_model <- kpca(~., data = train_data[, c("feature_1", "feature_2")],
                   kernel = "polydot",
                   kpar = list(degree = 3, scale = 1, offset = 5),
                   features = 2)

# Project the test data onto the principal components
test_features <- as.matrix(test_data[, c("feature_1", "feature_2")])
test_proj <- predict(kpca_model, test_features)
test_proj_df <- as.data.frame(test_proj)
colnames(test_proj_df) <- c("PC1", "PC2")
test_proj_df$class <- test_data$class

# Plot projection onto principal components
plot <- ggplot(test_proj_df, aes(x = PC1, y = PC2, color = class)) +
  geom_point(alpha = 0.7) +
  labs(title = "Projection onto PCs (Polynomial Kernel)",
       x = "1st principal component", y = "2nd principal component") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

print(plot)

# Inverse transform the projected training data (approximate)
X_hat_kpca <- pcv(kpca_model)

# Calculate mean squared error
mse <- mean((as.matrix(train_data[, c("feature_1", "feature_2")]) -
             as.matrix(X_hat_kpca))^2)
cat("Mean squared error for Kernel PCA is:", mse, "\n")