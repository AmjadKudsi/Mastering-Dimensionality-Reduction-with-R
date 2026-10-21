
library(MASS)
library(ggplot2)

# Load the iris dataset
data(iris)

# Preprocessing: Standardizing the data
X <- scale(iris[, 1:4])
y <- iris$Species

# Splitting the dataset into training and testing sets
X_train <- X[1:120, ]
y_train <- y[1:120]
X_test <- X[121:150, ]
y_test <- y[121:150]

# Applying Linear Discriminant Analysis with only one component
train_data <- data.frame(X_train, Species = y_train)
lda_model <- lda(Species ~ ., data = train_data)
lda_pred <- predict(lda_model, train_data)
X_lda <- lda_pred$x[, 1:2]

# Prepare data for plotting
plot_data <- data.frame(X_lda, Species = y_train)

# Scatter plot of the first linear discriminant
plot <- ggplot(plot_data, aes(x = LD1, y = LD2, color = Species)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "LDA: Projected data onto two components",
    x = "Discriminant 1",
    y = "Discriminant 2"  # TODO: Change this to reflect the second component
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Display the scatter plot
print(plot)