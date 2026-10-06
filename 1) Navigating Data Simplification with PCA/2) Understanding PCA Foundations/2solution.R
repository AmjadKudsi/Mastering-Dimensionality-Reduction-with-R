library(ggplot2)

# Given modified data
weight_lbs <- c(120, 140, 135, 155, 160, 145, 130, 165, 175, 150)
height_inches <- c(62, 70, 68, 64, 72, 66, 58, 73, 74, 69)
height_cm <- c(157.48, 177.8, 172.72, 162.56, 182.88, 167.64, 147.32, 185.42, 187.96, 175.26)

# Create a data frame
df <- data.frame(
  weight_lbs = weight_lbs,
  height_inches = height_inches,
  height_cm = height_cm
)

# Standardize the data
X_weight_height <- df[, c("weight_lbs", "height_inches")]
X_weight_height_standard <- scale(X_weight_height)

# Convert standardized matrix to data frame for ggplot
X_weight_height_standard_df <-
  as.data.frame(X_weight_height_standard)
  
# Compute Covariance Matrix
cov_matrix_weight_height <- cov(X_weight_height_standard)

# TODO: Calculate the eigenvalues and eigenvectors of the covariance matrix
eig <- eigen(cov_matrix_weight_height)

eigenvalues <- eig$values
eigenvectors <- eig$vectors

cat("\nEigenvalues:\n")
print(eigenvalues)

cat("\nEigenvectors:\n")
print(eigenvectors)

# Create data frame for eigenvector arrows
arrow_scale <- 2

# Prepare data for plotting
origin <- data.frame(x = 0, y = 0)
eigvec_df <- data.frame(
  xend = eigenvectors[1, ] * arrow_scale,
  yend = eigenvectors[2, ] * arrow_scale,
  label = c("Eigenvector 1", "Eigenvector 2"),
  color = c("red", "green")
)

# Plot standardized data and eigenvectors
plot <- ggplot(X_weight_height_standard_df, aes(x = weight_lbs, y = height_inches)) +
  geom_point(color = "blue") +
  geom_segment(
    data = eigvec_df,
    aes(x = 0, y = 0, xend = xend, yend = yend, color = label),
    arrow = arrow(length = unit(0.3, "cm")), linewidth = 1.2
  ) +
  scale_color_manual(values = c("red", "green")) +
  ggtitle("Eigenvectors of Covariance Matrix") +
  xlab("Weight (lbs)") +
  ylab("Height (inches)") +
  theme_bw() +
  theme(panel.grid = element_line())