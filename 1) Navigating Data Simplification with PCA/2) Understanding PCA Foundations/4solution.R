library(ggplot2)

# Given data
weight_lbs <- c(150, 160, 155, 165, 170, 160, 158, 175, 180, 170)
height_inches <- c(68, 72, 66, 69, 71, 65, 67, 70, 73, 68)
height_cm <- c(172.72, 182.88, 167.64, 175.26, 180.34, 165.1, 170.18, 177.8, 185.42, 172.72)

# Create a data frame
df <- data.frame(
  weight_lbs = weight_lbs,
  height_inches = height_inches,
  height_cm = height_cm
)

# TODO: Standardize 'Height (inches)' and 'Height (cm)'
X_standard_df <- df
X_standard_df$height_inches <- as.numeric(scale(df$height_inches))
X_standard_df$height_cm <- as.numeric(scale(df$height_cm))

# Plot standardized data
plot <- ggplot(X_standard_df, aes(x = height_inches, y = height_cm)) +
  geom_point(color = "blue") +
  ggtitle("Standardized Heights") +
  xlab("Standardized Height (inches)") +
  ylab("Standardized Height (cm)") +
  theme_bw() +
  theme(panel.grid = element_line())

print(plot)