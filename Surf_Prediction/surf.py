# Function to calculate Euclidean distance between two points
euclidean_distance <- function(x1, x2) {
  sqrt(sum((x1 - x2) ^ 2))
}

# K-nearest neighbor function using df1
knn_predict <- function(query_point, df1, k = 1) {
  # Extract the features from df1
  features <- df1[, c("wave_size", "wave_period", "wind_speed")]
  target <- df1$good_surf
  
  # Calculate distances between query point and all points in df1
  distances <- apply(features, 1, function(row) euclidean_distance(query_point, row))
  
  # Find the indices of the k nearest neighbors
  nearest_neighbors <- order(distances)[1:k]
  
  # Get the labels of the nearest neighbors
  nearest_labels <- target[nearest_neighbors]
  
  # Return the most common label (majority vote)
  prediction <- names(sort(table(nearest_labels), decreasing = TRUE))[1]
  
  return(prediction)
}

# Define query points
query_points <- data.frame(
  wave_size = c(8, 8, 6),
  wave_period = c(15, 2, 11),
  wind_speed = c(2, 18, 4)
)

# Predict the label for each query point using df1
for (i in 1:nrow(query_points)) {
  query <- query_points[i, ]
  prediction <- knn_predict(query, df1, k = 1)  # Using k = 1 (Nearest Neighbor)
  
  cat("Query", i, ": Wave Size =", query$wave_size, "ft, Wave Period =", query$wave_period, "secs, Wind Speed =", query$wind_speed, "MPH/hr \n")
  cat("Prediction (Good to Surf):", prediction, "\n\n")
}
