
library(keras3)
library(tensorflow)

# Set random seed for reproducibility
set.seed(42)
tensorflow::tf$random$set_seed(42)

# Function to create an autoencoder model
create_autoencoder <- function(input_dim, encoded_dim, learning_rate) {
  input_layer <- layer_input(shape = input_dim)
  # Use 'tanh' activation for the encoded layer as in the previous task
  encoded <- input_layer |> 
    layer_dense(units = encoded_dim, activation = "tanh")
  # Add another dense layer as in the previous task
  decoded <- encoded |> 
    layer_dense(units = input_dim, activation = "relu") |> 
    layer_dense(units = input_dim, activation = "sigmoid")
  
  autoencoder <- keras_model(input_layer, decoded)
  autoencoder |> compile(
    optimizer = optimizer_adam(learning_rate = learning_rate),
    loss = "mean_squared_error"
  )
  return(autoencoder)
}

# Simulate training and testing data
x_train <- matrix(runif(1000 * 32), nrow = 1000, ncol = 32)
x_test <- matrix(runif(300 * 32), nrow = 300, ncol = 32)

# TODO: Initialize two different learning rates for the experiment
learning_rate_high <- 0.01
learning_rate_low <- 0.001

# TODO: Create the first autoencoder with a higher learning rate, train it, and calculate its reconstruction error with encoded dimension set to 24
autoencoder_high <- create_autoencoder(
  input_dim = 32,
  encoded_dim = 24,
  learning_rate = learning_rate_high
)

autoencoder_high |> fit(
  x_train, x_train,
  epochs = 50,
  batch_size = 32,
  verbose = 0
)

predictions_high <- autoencoder_high |> predict(x_test)
reconstruction_error_high <- mean((x_test - predictions_high)^2)

# TODO: Create the second autoencoder with a lower learning rate, train it, and calculate its reconstruction error with the same setup as the previous one
autoencoder_low <- create_autoencoder(
  input_dim = 32,
  encoded_dim = 24,
  learning_rate = learning_rate_low
)

autoencoder_low |> fit(
  x_train, x_train,
  epochs = 50,
  batch_size = 32,
  verbose = 0
)

predictions_low <- autoencoder_low |> predict(x_test)
reconstruction_error_low <- mean((x_test - predictions_low)^2)

# TODO: Compare and print the reconstruction errors to see the effect of learning rates on model performance
cat("Higher Learning Rate (0.01) Reconstruction Error:",
    reconstruction_error_high, "\n")

cat("Lower Learning Rate (0.001) Reconstruction Error:",
    reconstruction_error_low, "\n")

if (reconstruction_error_high < reconstruction_error_low) {
  cat("The higher learning rate produced a lower reconstruction error.\n")
} else if (reconstruction_error_high > reconstruction_error_low) {
  cat("The lower learning rate produced a lower reconstruction error.\n")
} else {
  cat("Both learning rates produced the same reconstruction error.\n")
}