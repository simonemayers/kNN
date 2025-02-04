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
