library(keras3)
library(tensorflow)
library(mlbench)
library(ggplot2)
library(tidyr)
library(dplyr)

# Suppress TensorFlow logs except for errors
Sys.setenv(TF_CPP_MIN_LOG_LEVEL = 3)

# Load digits dataset and normalize data
digits_data <- mlbench::mlbench.2dnormals(1000, 2)$x
data <- scale(digits_data)

# TODO: Define the encoder with input layer and encoded layer
input_img <- layer_input(shape = c(2))
encoded <- input_img %>%
  layer_dense(units = 1, activation = 'relu')

# TODO: Define the decoder with decoded layer
decoded <- encoded %>%
  layer_dense(units = 2, activation = 'sigmoid')

# TODO: Define the autoencoder model and compile it
autoencoder <- keras_model(input_img, decoded)
autoencoder %>% compile(optimizer = 'adam', loss = 'binary_crossentropy')

# TODO: Train the autoencoder model with normalized data
autoencoder %>% fit(data, data, epochs = 100, batch_size = 256, shuffle = TRUE, verbose = 0)

# TODO: Predict the reconstructed data using the trained autoencoder
predicted <- autoencoder %>% predict(data)

# Prepare data for ggplot
n <- 10  # Number of digit images to display
df <- data.frame(
  id = rep(1:n, 2),
  x = c(data[1:n, 1], predicted[1:n, 1]),
  y = c(data[1:n, 2], predicted[1:n, 2]),
  type = rep(c("Original", "Reconstructed"), each = n)
)

# Plot using ggplot2
plot <- ggplot(df, aes(x = x, y = y, color = type, shape = type)) +
  geom_point(size = 3) +
  facet_wrap(~id, nrow = 2) +
  labs(title = "Original vs Reconstructed (Autoencoder Output)",
       x = "Feature 1", y = "Feature 2") +
  scale_color_manual(values = c("Original" = "blue", "Reconstructed" = "red")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", colour = NA))
plot