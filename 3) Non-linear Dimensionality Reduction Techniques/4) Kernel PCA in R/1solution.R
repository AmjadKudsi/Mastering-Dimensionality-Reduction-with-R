# Required packages
# install.packages(c("ggplot2", "dplyr", "caret", "kernlab"))
library(ggplot2)
library(dplyr)
library(caret)
library(kernlab)

set.seed(42)

# ---- Manual two-circle generator (make_circles-style) ----
n_samples   <- 150
ring_factor <- 0.3  # 'factor' is a base R function; use a different name
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

# ---- Train/test split (stratified) ----
set.seed(42)
idx <- createDataPartition(data$class, p = 0.75, list = FALSE)
train_data <- data[idx, ]
test_data  <- data[-idx, ]

# ---- Kernel PCA (RBF) on training set ----
# sigma controls the RBF width; 1/15 is a common starting point for unit-scale rings
set.seed(42)
kpca_model <- kpca(
  ~ .,
  data     = train_data[, c("feature_1", "feature_2")],
  kernel   = "rbfdot",
  kpar     = list(sigma = 1/15),
  features = 2
)

# ---- Project test set into KPCA space ----
test_features <- as.matrix(test_data[, c("feature_1", "feature_2")])
test_proj <- predict(kpca_model, test_features)

# ---- Plot KPCA embedding (white background) ----
plot_df <- data.frame(PC1 = test_proj[,1], PC2 = test_proj[,2], class = test_data$class)

p <- ggplot(plot_df, aes(PC1, PC2, color = class)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "Kernel PCA on Two Concentric Circles",
    x = "1st principal component", y = "2nd principal component", color = "Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = NA)
  )