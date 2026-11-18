library(keras3)
library(ggplot2)

# Set random seed for reproducibility
set.seed(42)

# Function to create an autoencoder model
create_autoencoder <- function(input_dim, encoded_dim, learning_rate) {
  input_layer <- layer_input(shape = input_dim)
  # Change the activation function from 'relu' to 'tanh'
  encoded <- input_layer |> 
    layer_dense(units = encoded_dim, activation = "tanh")
  # Add another dense layer
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

# Initialize list to store reconstruction errors
reconstruction_errors <- c()
learning_rates <- c(0.1, 0.01, 0.001, 0.0001)

# Train and test autoencoders with different learning rates
for (lr in learning_rates) {
  autoencoder <- create_autoencoder(32, 20, lr)
  autoencoder |> fit(
    x = x_train, y = x_train,
    epochs = 50,
    batch_size = 256,
    shuffle = TRUE,
    validation_data = list(x_test, x_test),
    verbose = 0
  )
  decoded_imgs <- autoencoder |> predict(x_test)
  reconstruction_error <- mean((x_test - decoded_imgs)^2)
  reconstruction_errors <- c(reconstruction_errors, reconstruction_error)
}

# Create a data frame for plotting
df <- data.frame(
  learning_rate = learning_rates,
  reconstruction_error = reconstruction_errors
)

# Plot reconstruction error vs learning rates using ggplot2 and assign to recon_plot
recon_plot <- ggplot(df, aes(x = learning_rate, y = reconstruction_error)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  labs(
    title = "Reconstruction Error vs Learning Rate",
    x = "Learning Rate",
    y = "Reconstruction Error"
  )