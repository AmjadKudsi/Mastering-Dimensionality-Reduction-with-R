data(iris)

chi2_score <- function(x, y, nbins = 5) {
  # Discretize x into nbins (quantile-based)
  qs <- unique(quantile(x, probs = seq(0, 1, length.out = nbins + 1), na.rm = TRUE))
  if (length(qs) < 3) return(0)
  bins <- cut(x, breaks = qs, include.lowest = TRUE, right = TRUE)
  # Build contingency table vs. y and return chisq.test statistic
  tbl <- table(bins, y)
  suppressWarnings(as.numeric(chisq.test(tbl)$statistic))
}

X <- iris[, 1:4]
y <- iris$Species

# Compute chi-square scores for each feature
weights <- vapply(X, chi2_score, numeric(1), y = y, nbins = 5)

# Select the top 2 features
k <- 2
selected_idx <- order(weights, decreasing = TRUE)[seq_len(k)]
selected_features <- names(weights)[selected_idx]

# Print the selected features and their scores
print("Selected Features:")
print(selected_features)
print("Scores:")
print(sort(weights, decreasing = TRUE))