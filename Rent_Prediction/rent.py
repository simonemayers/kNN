# Recursive function to build the k-d tree with proper base case handling
build_kd_tree <- function(data, depth = 0) {
  # Base case: If no data, return NULL
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  # Base case: If only one row, return the row as a leaf node
  if (nrow(data) == 1) {
    return(list(data = data, left = NULL, right = NULL))
  }
  
  # Alternate between Rent (depth % 2 == 0) and Size (depth % 2 == 1)
  feature <- ifelse(depth %% 2 == 0, "Rent", "Size")
  
  # Sort the data by the selected feature
  data <- data[order(data[[feature]]), ]
  
  # Find the median index
  median_index <- floor(nrow(data) / 2) + 1
  
  # Split the data into left and right subtrees
  left <- data[1:(median_index - 1), ]
  right <- data[(median_index + 1):nrow(data), ]
  
  # Recursively build the tree
  node <- list(
    data = data[median_index, ],
    left = build_kd_tree(left, depth + 1),
    right = build_kd_tree(right, depth + 1)
  )
  
  return(node)
}

# Build the k-d tree using your dataset (df7)
kd_tree <- build_kd_tree(df7)

# Function to print the k-d tree with proper recursion limits
print_kd_tree <- function(node, depth = 0) {
  if (is.null(node)) {
    return()
  }
  
  # Indentation based on depth
  indent <- paste(rep("  ", depth), collapse = "")
  
  # Print the current node's data
  print(paste(indent, "Node:", toString(node$data)))
  
  # Recursively print left and right children, if they exist
  if (!is.null(node$left)) {
    print_kd_tree(node$left, depth + 1)
  }
  
  if (!is.null(node$right)) {
    print_kd_tree(node$right, depth + 1)
  }
}

# Print the constructed k-d tree
print_kd_tree(kd_tree)


# Function to calculate Euclidean distance between two points
euclidean_distance <- function(query, point) {
  return(sqrt((query["Size"] - point["Size"])^2 + (query["Rent"] - point["Rent"])^2))
}

# Nearest neighbor search function
find_nearest_neighbor <- function(node, query, depth = 0, best = NULL) {
  if (is.null(node)) {
    return(best)  # Return the best so far if the node is null
  }
  
  # Calculate distance between the query and the current node
  current_distance <- euclidean_distance(query, node$data)
  
  # Update best if the current node is closer
  if (is.null(best) || current_distance < euclidean_distance(query, best$data)) {
    best <- node
  }
  
  # Determine whether to go left or right in the tree
  feature <- ifelse(depth %% 2 == 0, "Rent", "Size")  # Alternate between Rent and Size
  
  # Traverse the tree
  if (query[[feature]] < node$data[[feature]]) {
    best <- find_nearest_neighbor(node$left, query, depth + 1, best)
  } else {
    best <- find_nearest_neighbor(node$right, query, depth + 1, best)
  }
  
  # Check the other side of the tree if necessary
  difference <- abs(query[[feature]] - node$data[[feature]])
  if (difference < euclidean_distance(query, best$data)) {
    if (query[[feature]] < node$data[[feature]]) {
      best <- find_nearest_neighbor(node$right, query, depth + 1, best)
    } else {
      best <- find_nearest_neighbor(node$left, query, depth + 1, best)
    }
  }
  
  return(best)
}

# Query to find nearest neighbor for
query <- data.frame(Size = 1000, Rent = 2200)

# Find nearest neighbor using the kd_tree
nearest_neighbor <- find_nearest_neighbor(kd_tree, query[1, ])

# Print the nearest neighbor found
print("Nearest Neighbor:")

print(nearest_neighbor$data)
