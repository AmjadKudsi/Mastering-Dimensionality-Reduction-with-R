# Load required libraries
library(keras3)
library(tensorflow)
library(dplyr)

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

# TODO: Define a Sequential neural network with one input layer, one hidden layer with 'relu' function, and one output layer with 'sigmoid' function
n_x <- ncol(X_train)
n_h <- 5
n_y <- 1

model <- keras_model_sequential() %>%
  layer_dense(units = n_h, activation = "relu", input_shape = n_x) %>%
  layer_dense(units = n_y, activation = "sigmoid")


# TODO: Compile the model with 'adam' optimizer and loss function for binary classification
model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = "accuracy"
)

# TODO: Train the model on the training data
history <- model %>% fit(
  X_train,
  y_train,
  epochs = 100,
  verbose = 0
)

# TODO: Evaluate the model's training loss after the training is complete
final_loss <- history$metrics$loss[length(history$metrics$loss)]
cat("Train Loss: ", final_loss, "\n")

# TODO: Predict the labels of the test set and calculate the accuracy of the model
y_pred_prob <- model %>% predict(X_test)

y_pred <- ifelse(y_pred_prob > 0.5, 1, 0)

accuracy <- mean(y_pred == y_test)
cat("Accuracy: ", accuracy, "\n")