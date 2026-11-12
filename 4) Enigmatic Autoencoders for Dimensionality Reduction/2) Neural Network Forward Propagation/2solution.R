# Load required libraries
library(keras3)
library(dplyr)
library(ggplot2)

set.seed(42)

# Load the iris dataset
data(iris)

# Only use the first two features and first two classes
iris_binary <- iris %>%
  filter(Species %in% c("setosa", "versicolor")) %>%
  mutate(Species = as.numeric(Species == "versicolor")) # 0 for setosa, 1 for versicolor

X <- as.matrix(iris_binary[, 1:2]) # Sepal.Length and Sepal.Width
y <- iris_binary$Species

# Scale the inputs to (0, 1) range
X_scaled <- scale(X, center = TRUE, scale = TRUE)
X_scaled <- as.matrix(X_scaled)

# Split into train and test sets (80/20 split)
set.seed(42)
train_indices <- sample(1:nrow(X_scaled), size = 0.8 * nrow(X_scaled))
X_train <- X_scaled[train_indices, ]
X_test <- X_scaled[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

# Define the neural network structure
n_x <- ncol(X_train) # number of inputs
n_h <- 5             # neurons in hidden layer
n_y <- 1             # number of outputs

# Build the model
model <- keras_model_sequential()

# TODO: Add a hidden layer with 5 neurons and 'relu' activation, specifying input_shape = n_x.
# TODO: Add an output layer with 1 neuron and 'sigmoid' activation.
# TODO: Compile the model with optimizer 'sgd' and loss function 'binary_crossentropy'.
model <- model %>%
  layer_dense(units = n_h, activation = "relu", input_shape = n_x) %>%
  layer_dense(units = n_y, activation = "sigmoid")
  
model %>% compile(
  optimizer = "sgd",
  loss = "binary_crossentropy",
  metrics = "accuracy"
)

# Train the model
history <- model %>% fit(
  X_train,
  y_train,
  epochs = 100,
  verbose = 0
)

# Evaluate the model's loss on the training data
final_loss <- history$metrics$loss[length(history$metrics$loss)]
cat("Train Loss: ", final_loss, "\n")

# Predict probabilities on test data
y_pred_prob <- model %>% predict(X_test)

# Convert probabilities to binary predictions using 0.5 threshold
y_pred <- ifelse(y_pred_prob > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(y_pred == y_test)
cat("Accuracy: ", accuracy, "\n")

# Create a mesh grid over the feature space
x_min <- min(X_scaled[, 1]) - 0.5
x_max <- max(X_scaled[, 1]) + 0.5
y_min <- min(X_scaled[, 2]) - 0.5
y_max <- max(X_scaled[, 2]) + 0.5

grid <- expand.grid(
  Sepal.Length = seq(x_min, x_max, length.out = 200),
  Sepal.Width = seq(y_min, y_max, length.out = 200)
)

# Predict class probabilities for each point in the grid
grid_pred_prob <- model %>% predict(as.matrix(grid))
grid$Class <- as.factor(ifelse(grid_pred_prob > 0.5, "Versicolor", "Setosa"))

# Prepare the original data for plotting
plot_data <- data.frame(
  Sepal.Length = X_scaled[, 1],
  Sepal.Width = X_scaled[, 2],
  Species = factor(ifelse(y == 1, "Versicolor", "Setosa"))
)

# Plot decision boundary and data points
plot <- ggplot() +
  geom_contour_filled(
    data = grid,
    aes(x = Sepal.Length, y = Sepal.Width, z = as.numeric(Class == "Versicolor")),
    breaks = c(-Inf, 0.5, Inf),
    alpha = 0.4
  ) +
  geom_point(
    data = plot_data,
    aes(x = Sepal.Length, y = Sepal.Width, color = Species),
    size = 2
  ) +
  scale_color_manual(values = c("Setosa" = "red", "Versicolor" = "blue")) +
  labs(
    title = "Iris Classification Decision Boundary (Setosa=0, Versicolor=1)",
    x = "Sepal Length (scaled)",
    y = "Sepal Width (scaled)"
  ) +
  theme_bw()