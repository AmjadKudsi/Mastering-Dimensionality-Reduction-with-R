Sys.setenv(KERAS_BACKEND = "tensorflow")  # choose backend
library(keras3)

# Initialize a Sequential Neural Network
model <- keras_model_sequential() |>
  layer_dense(units = 64, activation = "relu", input_shape = c(20)) |>
  layer_dense(units = 3, activation = "softmax")

# Compile the model
model |> compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

# Print the summary of the model
summary(model)