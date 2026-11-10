
Sys.setenv(KERAS_BACKEND = "tensorflow")
library(keras3)

model <- keras_model_sequential()

# Input layer and first hidden layer
model <- model |>
  layer_dense(units = 64, activation = "relu", input_shape = 20)

# Second hidden layer: 32 neurons with ReLU
model <- model |>
  layer_dense(units = 32, activation = "relu")

# Output layer: 10 neurons without activation
model <- model |>
  layer_dense(units = 10)

# Compile the model
model |> compile(
  loss = "mean_squared_error",
  optimizer = "adam"
)

# Display model architecture
summary(model)