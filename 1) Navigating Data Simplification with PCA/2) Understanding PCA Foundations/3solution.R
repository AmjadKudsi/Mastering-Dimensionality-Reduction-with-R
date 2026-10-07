library(ggplot2)

# Given data
weight_lbs <- c(120, 180, 150, 160, 130, 140, 165, 175, 155, 145)
height_inches <- c(60, 70, 65, 68, 62, 64, 72, 74, 66, 63)
height_cm <- c(152.4, 177.8, 165.1, 172.72, 157.48, 162.56, 182.88, 187.96, 167.64, 160.02)

# Create a data frame
df <- data.frame(
  weight_lbs = weight_lbs,
  height_inches = height_inches,
  height_cm = height_cm
)

# Standardize just the height columns
X <- df[, c("height_inches", "height_cm")]
X_standard <- scale(X)

# TODO: Compute the covariance matrix for the standardized height data
cov_matrix <- cov(X_standard)
print(cov_matrix)

# TODO: Perform eigendecomposition on the covariance matrix to obtain eigenvalues and eigenvectors
eig <- eigen(cov_matrix)
eigenvalues <- eig$values
eigenvectors <- eig$vectors

cat("\nEigenvalues:\n")
print(eigenvalues)

cat("\nEigenvectors:\n")
print(eigenvectors)

# Plotting the standardized data and eigenvectors
X_standard_df <- as.data.frame(X_standard)
colnames(X_standard_df) <- c("height_inches", "height_cm")

origin <- data.frame(x = 0, y = 0)
eigvec_df <- data.frame(
  xend = eigenvectors[1, ],
  yend = eigenvectors[2, ],
  label = c("Eigenvector 1", "Eigenvector 2"),
  color = c("red", "green")
)

plot <- ggplot(X_standard_df, aes(x = height_inches, y = height_cm)) +
  geom_point(color = "blue") +
  geom_segment(
    data = eigvec_df,
    aes(x = 0, y = 0, xend = xend, yend = yend, color = label),
    arrow = arrow(length = unit(0.3, "cm")), linewidth = 1.2
  ) +
  scale_color_manual(values = c("red", "green")) +
  ggtitle("Standardized Data with Eigenvectors") +
  xlab("Height (inches) standardized") +
  ylab("Height (cm) standardized") +
  theme_bw() +
  theme(panel.grid = element_line())

cat("\nCovariance Matrix:\n")
print(cov_matrix)

cat("\nEigenvalues:\n")
print(eigenvalues)

cat("\nEigenvectors:\n")
print(eigenvectors)