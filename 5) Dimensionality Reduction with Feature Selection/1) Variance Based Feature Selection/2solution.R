# Importing necessary libraries
library(caret)

# Setting a fixed seed for reproducibility
set.seed(10)

# Generating synthetic data with modified feature values
data <- data.frame(
  feature_1 = rnorm(1000, mean = 0, sd = 1),
  feature_2 = rnorm(1000, mean = 5, sd = 2),
  feature_3 = rep(0.5, 1000),  # This feature has variance 0
  feature_4 = rbinom(1000, 1, 0.2), # Binary feature will have low variance if p is close to 0 or 1
  feature_5 = rnorm(1000, mean = 10, sd = 5)
)

# Calculate variance for each feature
variances <- apply(data, 2, var)

# Filter features with variance above the threshold
threshold <- 0.5  # The change should be made on this line
kept_features <- names(variances[variances > threshold])

# Displaying shapes of original and reduced data
original_shape <- dim(data)
reduced_shape <- c(nrow(data), length(kept_features))

# Displaying the names of the kept features
cat("Original data shape: ", original_shape, "\n")
cat("Reduced data shape: ", reduced_shape, "\n")
cat("Kept Features: ", kept_features, "\n")