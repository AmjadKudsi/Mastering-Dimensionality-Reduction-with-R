suppressPackageStartupMessages({
  # Set CRAN mirror first
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  # Now load libraries
  library(readr)
  library(ggplot2)
  library(MASS)
  library(pROC)
  library(tibble)
  library(dplyr)
  library(caret)
})

# Source solution.R to get the plot object
source("solution.R", local = FALSE)

# Function to save a ggplot object if it exists
save_ggplot_if_exists <- function(var_name) {
  if (exists(var_name) && inherits(get(var_name), "ggplot")) {
    ggsave(filename = "static/images/plot.png", plot = get(var_name), width = 10, height = 6)
    return(TRUE) 
  }
  return(FALSE)  
}

# Get all objects in the environment
all_objects <- ls()

# Iterate over all objects and save the first ggplot object found
for (obj in all_objects) {
  if (save_ggplot_if_exists(obj)) {
    break
  }
}
