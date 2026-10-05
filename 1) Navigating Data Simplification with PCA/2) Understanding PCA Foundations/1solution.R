library(ggplot2)

# Data
weight_lbs <- c(150, 160, 155, 165, 170, 160, 158, 175, 180, 170)
height_inches <- c(68, 72, 66, 69, 71, 65, 67, 70, 73, 68)
height_cm <- c(172.72, 182.88, 167.64, 175.26, 180.34, 165.1, 170.18, 177.8, 185.42, 172.72)

# Create a data frame
df <- data.frame(
  weight_lbs = weight_lbs,
  height_inches = height_inches,
  height_cm = height_cm
)

# Standardize the data
X <- df[, c("height_inches", "height_cm")]
X_standard <- scale(X)

# Compute Covariance Matrix
cov_matrix <- cov(X_standard)

# Eigendecomposition
eig <- eigen(cov_matrix)
eigenvalues <- eig$values
eigenvectors <- eig$vectors

# Prepare data for plotting
X_standard_df <- as.data.frame(X_standard)
colnames(X_standard_df) <- c("height_inches", "height_cm")

eigvec_df <- data.frame(
  xend = eigenvectors[1, ],
  yend = eigenvectors[2, ],
  label = c("Eigenvector 1", "Eigenvector 2"),
  color = c("red", "green")
)

# Plot standardized data and eigenvectors
plot <- ggplot(X_standard_df, aes(x = height_inches, y = height_cm)) +
  geom_point(color = "blue") +
  geom_segment(
    data = eigvec_df,
    aes(x = 0, y = 0, xend = xend, yend = yend, color = label),
    arrow = arrow(length = unit(0.3, "cm")), linewidth = 1.2
  ) +
  scale_color_manual(values = c("red", "green")) +
  ggtitle("Eigenvectors of Covariance Matrix") +
  xlab("height_inches") +
  ylab("height_cm") +
  theme_bw() +
  theme(panel.grid = element_line())