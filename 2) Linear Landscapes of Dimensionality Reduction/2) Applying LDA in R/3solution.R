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

# Applying Linear Discriminant Analysis
train_data <- data.frame(X_train, Species = y_train)
lda_model <- lda(Species ~ ., data = train_data)

# TODO: Transform the training data using LDA and store the result
lda_pred <- predict(lda_model, train_data)
X_lda <- lda_pred$x

# Prepare data for plotting
# TODO: Create a data frame for plotting using the transformed data
plot_data <- data.frame(X_lda, Species = y_train)

# Scatter plot of the first two linear discriminants
# TODO: Create a ggplot scatter plot for the transformed data
print(
  ggplot(plot_data, aes(x = LD1, y = LD2, color = Species)) +
    geom_point(size = 3, alpha = 0.8) +
    labs(
      title = "LDA: Projected data onto the first two components",
      x = "Discriminant 1",
      y = "Discriminant 2"
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
)

# Save the plot as an image
ggsave("lda_plot.png", width = 8, height = 6)