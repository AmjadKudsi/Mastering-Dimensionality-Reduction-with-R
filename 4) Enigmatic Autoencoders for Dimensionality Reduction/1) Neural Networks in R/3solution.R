# Tell Keras to use TensorFlow as backend
Sys.setenv(KERAS_BACKEND = "tensorflow")
library(keras3)

# TODO: Initialize a Sequential Neural Network
model <- keras_model_sequential()

# TODO: Define the input layer with the appropriate shape
model <- keras_model_sequential() |>
    layer_dense(units = 64, activation = "relu", input_shape = 20)

# TODO: Add a dense hidden layer with ReLU activation function
model <- model |>
layer_dense(units = 10, activation = "relu")

# TODO: Add a second dense hidden layer with ReLU activation function
model <- model |>
layer_dense(units = 10, activation = "relu")

# TODO: Add a dense output layer with a suitable number of neurons
model <- model |>
layer_dense(units = 10)

# TODO: Compile the model with the proper loss function and optimizer
model |> compile(
    loss = "categorical_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
)

# TODO: Print a summary of the model to check its architecture
summary(model)