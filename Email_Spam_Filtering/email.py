# Define the query email's bag-of-words representation
query_email <- data.frame(
  money = 0,
  free = 1,
  of = 1,
  gambling = 0,
  fun = 0,
  machine = 1,
  learning = 1
)

# Extract the feature columns from the dataset
features <- df3[, c("money", "free", "of", "gambling", "fun", "machine", "learning")]
target <- df3$spam  # Spam column is the target

# Calculate the Euclidean distance between the query email and each email in the dataset
distances <- apply(features, 1, function(row) euclidean_distance(query_email, row))

# Find the nearest neighbor (k = 1)
nearest_neighbor <- which.min(distances)

# Output the nearest neighbor and the predicted label
print(paste("The nearest neighbor is email ID:", df3$ID[nearest_neighbor]))

print(paste("Prediction (Spam or Not):", target[nearest_neighbor]))

# Define the query email's bag-of-words representation
query_email <- data.frame(
  money = 0,
  free = 1,
  of = 1,
  gambling = 0,
  fun = 0,
  machine = 1,
  learning = 1
)

# Function to calculate Euclidean distance between two points
euclidean_distance <- function(x1, x2) {
  sqrt(sum((x1 - x2) ^ 2))
}

# Extract the feature columns from the dataset
features <- df3[, c("money", "free", "of", "gambling", "fun", "machine", "learning")]
target <- df3$spam  # Spam column is the target

# Calculate the Euclidean distance between the query email and each email in the dataset
distances <- apply(features, 1, function(row) euclidean_distance(query_email, row))

# Sort distances and get the indices of the k = 3 nearest neighbors
k <- 3
nearest_neighbors <- order(distances)[1:k]

# Get the labels of the nearest neighbors
nearest_labels <- target[nearest_neighbors]

# Output the nearest neighbors' IDs and their labels
print(paste("The nearest neighbors are email IDs:", df3$ID[nearest_neighbors]))

print(paste("The spam status of the 3 nearest neighbors:", nearest_labels))

# Perform majority voting to get the prediction
prediction <- names(sort(table(nearest_labels), decreasing = TRUE))[1]
print(paste("Prediction (Spam or Not):", prediction))

# Calculate the Euclidean distance between the query email and each email in the dataset
distances <- apply(features, 1, function(row) euclidean_distance(query_email, row))

# Sort distances and get the indices of the k = 5 nearest neighbors
k <- 5
nearest_neighbors <- order(distances)[1:k]

# Get the labels and distances of the nearest neighbors
nearest_labels <- target[nearest_neighbors]
nearest_distances <- distances[nearest_neighbors]

# Calculate the weights (reciprocal of squared distances)
weights <- 1 / (nearest_distances ^ 2)

# Output the nearest neighbors' IDs, their labels, and their weights
print(paste("The nearest neighbors are email IDs:", df3$ID[nearest_neighbors]))

print(paste("The spam status of the 5 nearest neighbors:", nearest_labels))

print(paste("Weights of the 5 nearest neighbors:", weights))

# Perform weighted voting
weighted_votes <- tapply(weights, nearest_labels, sum)

# Return the label with the highest weighted vote
prediction <- names(which.max(weighted_votes))
print(paste("Prediction (Spam or Not):", prediction))

# Manhattan distance function
manhattan_distance <- function(x1, x2) {
  sum(abs(x1 - x2))
}

# Apply k-NN with Manhattan distance
manhattan_distances <- apply(features, 1, function(row) manhattan_distance(query_email, row))
k <- 3
nearest_neighbors_manhattan <- order(manhattan_distances)[1:k]
nearest_labels_manhattan <- target[nearest_neighbors_manhattan]

# Output results
print(paste("The nearest neighbors are email IDs:", df3$ID[nearest_neighbors_manhattan]))

print(paste("The spam status of the 3 nearest neighbors:", nearest_labels_manhattan))

# Majority voting
prediction_manhattan <- names(sort(table(nearest_labels_manhattan), decreasing = TRUE))[1]
print(paste("Prediction (Spam or Not):", prediction_manhattan))

# Cosine similarity function
cosine_similarity <- function(x1, x2) {
  dot_product <- sum(x1 * x2)
  magnitude_x1 <- sqrt(sum(x1^2))
  magnitude_x2 <- sqrt(sum(x2^2))
  return(dot_product / (magnitude_x1 * magnitude_x2))
}

# Apply k-NN with cosine similarity
cosine_similarities <- apply(features, 1, function(row) cosine_similarity(query_email, row))
k <- 3
nearest_neighbors_cosine <- order(-cosine_similarities)[1:k]  # Sort by descending cosine similarity
nearest_labels_cosine <- target[nearest_neighbors_cosine]

# Output results
print(paste("The nearest neighbors are email IDs:", df3$ID[nearest_neighbors_cosine]))

print(paste("The spam status of the 3 most similar neighbors:", nearest_labels_cosine))

# Majority voting
prediction_cosine <- names(sort(table(nearest_labels_cosine), decreasing = TRUE))[1]
print(paste("Prediction (Spam or Not):", prediction_cosine))
