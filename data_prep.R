# Load necessary libraries
library(mice)
library(tidyverse)

# Load the dataset
load("../tutorial/data/osf_features.RData")

# Define the feature selection
feature_selection <- c(
  "ar01",
  "edf",
  "lin",
  "mac",
  "mad",
  "median"
)

# Filter and select specific features from the dataset
data <- mice::complete(featFullImp$featuresImp) %>%
  select(ends_with(feature_selection))

# Filter and select specific features from the scaled dataset
scaled_data <- featFullImp$featuresImpZMat %>%
  select(ends_with(feature_selection))

# Save the filtered data and scaled data as CSV files
write.csv(data, "data/features.csv", row.names = FALSE)
write.csv(scaled_data, "data/features_scaled.csv", row.names = FALSE)

# Save both datasets into an R data file
save(data, scaled_data, file = "data/features.RData")
