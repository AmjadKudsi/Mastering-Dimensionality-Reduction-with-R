data(iris)

chi2_score <- function(x, y, nbins = 5) {
  qs <- unique(quantile(x, probs = seq(0, 1, length.out = nbins + 1), na.rm = TRUE))
  if (length(qs) < 3) return(0)
  bins <- cut(x, breaks = qs, include.lowest = TRUE, right = TRUE)
  tbl <- table(bins, y)
  suppressWarnings(as.numeric(chisq.test(tbl)$statistic))
}

X <- iris[, 1:4]
y <- iris$Species

weights <- vapply(X, chi2_score, numeric(1), y = y, nbins = 5)

# TODO: Change the selection to keep the top 3 features instead of 2
selected_idx <- order(weights, decreasing = TRUE)[1:3]
selected_features <- names(weights)[selected_idx]

print("Selected Features:")
print(selected_features)
print("Scores:")
print(weights)