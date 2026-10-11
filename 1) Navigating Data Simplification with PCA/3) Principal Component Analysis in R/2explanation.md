The before and after PCA plots look identical because all three features in the dataset are perfectly linearly related. This means the first principal component (PC1) already captures 100% of the variance, leaving 0% for the second principal component (PC2).
Although the updated code correctly calculates and plots two principal components instead of one, PC2 has essentially zero values, so the points remain on a straight horizontal line.

## Key takeaway: 
Adding more principal components does not necessarily capture more variance. It depends on how much independent information exists in the dataset.