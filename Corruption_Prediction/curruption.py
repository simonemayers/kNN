# Query point for Russia
query_russia <- data.frame(
  Life_Expectancy = 67.62,
  Top_10_Income = 31.68,
  Infant_Mortality = 10.0,
  Military_Spending = 3.87,
  School_Years = 12.9
)

# Extract features and CPI
features <- df4[, c("Life Expectancy", "Top-10 Income", "Infant Mortality", "Military Spending", "School Years")]
target_cpi <- df4$CPI

# Calculate Euclidean distances between Russia and all countries
distances_russia <- apply(features, 1, function(row) euclidean_distance(query_russia, row))

# Find the 3 nearest neighbors
k <- 3
nearest_neighbors_russia <- order(distances_russia)[1:k]
nearest_cpi_russia <- target_cpi[nearest_neighbors_russia]

# Output the CPI values and the predicted CPI
print(paste("The CPI values of the 3 nearest neighbors are:", nearest_cpi_russia))

predicted_cpi_russia <- mean(nearest_cpi_russia)
print(paste("The predicted CPI for Russia is:", predicted_cpi_russia))

# Avoid division by zero by adding a small value to the distances
distances_russia <- apply(features, 1, function(row) euclidean_distance(query_russia, row))
distances_russia <- ifelse(distances_russia == 0, 1e-6, distances_russia)

# Calculate the weights (reciprocal of squared distances)
weights_russia <- 1 / (distances_russia ^ 2)

# Calculate the weighted CPI prediction
weighted_cpi_russia <- sum(weights_russia * target_cpi) / sum(weights_russia)

# Output the final predicted CPI for Russia
print(paste("The weighted predicted CPI for Russia is:", weighted_cpi_russia))

# Ensure all features are numeric
features <- data.frame(lapply(features, as.numeric))

# Function for range normalization
range_normalize <- function(column) {
  (column - min(column)) / (max(column) - min(column))
}

# Normalize features and the query
features_normalized <- as.data.frame(lapply(features, range_normalize))
query_russia_normalized <- as.data.frame(lapply(1:ncol(query_russia), function(i) {
  range_normalize(c(features[, i], query_russia[, i]))[nrow(features) + 1]
}))
names(query_russia_normalized) <- names(features)
query_russia_normalized <- as.data.frame(t(query_russia_normalized))

# Calculate Euclidean distances using normalized features
distances_russia_normalized <- apply(features_normalized, 1, function(row) euclidean_distance(query_russia_normalized, row))

# Find the 3 nearest neighbors and predict CPI
nearest_neighbors_normalized <- order(distances_russia_normalized)[1:k]
nearest_cpi_normalized <- target_cpi[nearest_neighbors_normalized]
predicted_cpi_russia_normalized <- mean(nearest_cpi_normalized)

# Output results
print(paste("The CPI values of the 3 nearest neighbors are:", nearest_cpi_normalized))

print(paste("The predicted CPI for Russia after normalization is:", predicted_cpi_russia_normalized))

# Normalize features and the query
features_normalized <- as.data.frame(lapply(features, range_normalize))
query_russia_normalized <- as.data.frame(lapply(1:ncol(query_russia), function(i) {
  range_normalize(c(features[, i], query_russia[, i]))[nrow(features) + 1]
}))
names(query_russia_normalized) <- names(features)
query_russia_normalized <- as.data.frame(t(query_russia_normalized))

# Calculate Euclidean distances using normalized features
distances_russia_normalized <- apply(features_normalized, 1, function(row) euclidean_distance(query_russia_normalized, row))
distances_russia_normalized <- ifelse(distances_russia_normalized == 0, 1e-6, distances_russia_normalized)

# Calculate the weights (reciprocal of squared distances)
weights_russia_normalized <- 1 / (distances_russia_normalized ^ 2)

# Calculate the weighted CPI prediction
weighted_cpi_russia_normalized <- sum(weights_russia_normalized * target_cpi) / sum(weights_russia_normalized)

# Output the final predicted CPI for Russia after range normalization
print(paste("The weighted predicted CPI for Russia after normalization is:", weighted_cpi_russia_normalized))

