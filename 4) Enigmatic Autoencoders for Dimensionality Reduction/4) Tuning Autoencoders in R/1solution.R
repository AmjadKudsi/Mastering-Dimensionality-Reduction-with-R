library(keras3)

# Set random seed for reproducibility
set.seed(42)

# Function to create an autoencoder model
create_autoencoder <- function(input_dim, encoded_dim, learning_rate) {
  input_layer <- layer_input(shape = input_dim)
  # Add a dense layer with 'relu' activation
  encoded <- input_layer |> 
    layer_dense(units = encoded_dim, activation = "relu")
  # Add a dense output layer with 'sigmoid' activation
  decoded <- encoded |> 
    layer_dense(units = input_dim, activation = "sigmoid")
  
  autoencoder <- keras_model(input_layer, decoded)
  autoencoder |> compile(
    optimizer = optimizer_adam(learning_rate = learning_rate),
    loss = "mean_squared_error"
  )
  return(autoencoder)
}

# Simulate training and testing data
x_train <- matrix(runif(1000 * 20), nrow = 1000, ncol = 20)
x_test <- matrix(runif(300 * 20), nrow = 300, ncol = 20)

# Training with a higher learning rate
learning_rate_fast <- 0.1
autoencoder_fast <- create_autoencoder(20, 16, learning_rate_fast)
history_fast <- autoencoder_fast |> fit(
  x = x_train, y = x_train,
  epochs = 50,
  batch_size = 256,
  shuffle = TRUE,
  validation_data = list(x_test, x_test),
  verbose = 0
)

# Training with a slower learning rate
learning_rate_slow <- 0.01
autoencoder_slow <- create_autoencoder(20, 16, learning_rate_slow)
history_slow <- autoencoder_slow |> fit(
  x = x_train, y = x_train,
  epochs = 50,
  batch_size = 256,
  shuffle = TRUE,
  validation_data = list(x_test, x_test),
  verbose = 0
)

# Compare the model performances
decoded_imgs_fast <- autoencoder_fast |> predict(x_test)
decoded_imgs_slow <- autoencoder_slow |> predict(x_test)

reconstruction_error_fast <- mean((x_test - decoded_imgs_fast)^2)
reconstruction_error_slow <- mean((x_test - decoded_imgs_slow)^2)

cat(sprintf("Reconstruction error (fast learning rate): %.4f\n", reconstruction_error_fast))
cat(sprintf("Reconstruction error (slow learning rate): %.4f\n", reconstruction_error_slow))