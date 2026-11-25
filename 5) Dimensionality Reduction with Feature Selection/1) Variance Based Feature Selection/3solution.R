# Importing necessary libraries
library(caret)

# Setting a fixed seed for reproducibility
set.seed(36)

# Generating synthetic data with ten features
data <- data.frame(
  feature_1 = rnorm(1000, mean = 0, sd = 1),
  feature_2 = rnorm(1000, mean = 0, sd = 2),
  feature_3 = rep(1, 1000),  # Constant value
  feature_4 = rnorm(1000, mean = 0, sd = 4),
  feature_5 = rnorm(1000, mean = 0, sd = 1),
  feature_6 = rep(0, 1000),  # Constant value
  feature_7 = rnorm(1000, mean = 0, sd = 1),
  feature_8 = rnorm(1000, mean = 0, sd = 1),
  feature_9 = rnorm(1000, mean = 0, sd = 2),
  feature_10 = rep(3, 1000)  # Constant value
)

cat("Original data shape: ", dim(data), "\n")

# TODO: Calculate variance for each feature
variances <- sapply(data, var)

# TODO: Filter features with variance above the threshold of 0.1
thresholded_features <- names(variances[variances >= 0.1])

# TODO: Display the names of the kept features
cat("Kept features: ", thresholded_features)