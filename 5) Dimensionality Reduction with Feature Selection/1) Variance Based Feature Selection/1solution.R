# Importing necessary libraries
library(caret)

# Setting a fixed seed for reproducibility
set.seed(36)

# Generating synthetic data
data <- data.frame(
  feature_1 = runif(1000),
  feature_2 = runif(1000) * 10,
  feature_3 = runif(1000),
  feature_4 = runif(1000) * 100,
  feature_5 = runif(1000),
  feature_6 = runif(1000) * 0.1,
  feature_7 = runif(1000),
  feature_8 = runif(1000) * 0.01,
  feature_9 = runif(1000),
  feature_10 = runif(1000) * 50
)

# TODO: Calculate variance for each feature
variances <- sapply(data, var)

# TODO: Filter features with variance above the threshold
threshold <- 0.1
kept_features <- names(variances[variances >= threshold])

data_reduced <- data[ , kept_features]

# TODO: Display shapes of original and reduced data
cat("Original data shape: ", dim(data)[1], "rows and", dim(data)[2], "columns\n")
cat("Reduced data shape: ", dim(data_reduced)[1], "rows and", dim(data_reduced)[2], "columns\n")

# TODO: Display the names of the kept features
cat("Kept features and their variances:\n")
for (feature in kept_features) {
  cat(feature, ": ", variances[feature], "\n")
}