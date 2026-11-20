# Load required libraries
library(keras3)
library(ggplot2)
library(dplyr)
library(mlbench)

# For reproducibility
set.seed(42)

# Generate a digits-like dataset using mlbench.waveform
data <- mlbench.waveform(5000)
X <- as.matrix(data$x)
y <- as.vector(data$classes)

# Scale the data
X <- scale(X)

# Split the data into a training set and a test set
n <- nrow(X)
train_idx <- sample(1:n, size = floor(0.75 * n))
x_train <- X[train_idx, ]
x_test <- X[-train_idx, ]

# Define a simple Autoencoder architecture
create_autoencoder <- function(input_dim, encoded_dim, optimizer) {
  model <- keras_model_sequential() %>%
    layer_dense(units = encoded_dim, activation = "relu", input_shape = input_dim) %>%
    layer_dense(units = input_dim, activation = "sigmoid")
  
  model %>% compile(
    optimizer = optimizer,
    loss = "binary_crossentropy"
  )
  return(model)
}

# Train and evaluate the model
train_and_evaluate <- function(optimizer, optimizer_name) {
  input_dim <- ncol(x_train)
  encoded_dim <- 8
  autoencoder <- create_autoencoder(input_dim, encoded_dim, optimizer)
  
  autoencoder %>% fit(
    x = x_train, y = x_train,
    epochs = 50, batch_size = 256,
    shuffle = TRUE,
    validation_data = list(x_test, x_test),
    verbose = 0
  )
  
  decoded_imgs <- autoencoder %>% predict(x_test)
  reconstruction_error <- mean((x_test - decoded_imgs)^2)
  cat(sprintf("Reconstruction error (%s): %f\n", optimizer_name, reconstruction_error))
  return(reconstruction_error)
}

# Compare different optimizers
optimizers <- list(
  SGD = optimizer_sgd(learning_rate = 0.001),
  Adam = optimizer_adam(learning_rate = 0.001),
  RMSprop = optimizer_rmsprop(learning_rate = 0.001),
  Adagrad = optimizer_adagrad(learning_rate = 0.001)
)

results <- c()
for (opt_name in names(optimizers)) {
  opt <- optimizers[[opt_name]]
  results[opt_name] <- train_and_evaluate(opt, opt_name)
}

# Visualize the results in a bar plot
results_df <- data.frame(
  Optimizer = names(results),
  ReconstructionError = as.numeric(results)
)

plot <- ggplot(results_df, aes(x = Optimizer, y = ReconstructionError, fill = Optimizer)) +
  geom_bar(stat = "identity", color = "black") +
  ylab("Reconstruction Error") +
  ggtitle("Comparison of Different Optimizers") +
  theme_bw() +
  theme(legend.position = "none")