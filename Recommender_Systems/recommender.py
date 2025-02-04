# Function to calculate CP, CA, PA, and AP for two customers
calculate_similarity_values <- function(cust1, cust2) {
  CP <- sum(cust1 == TRUE & cust2 == TRUE)
  CA <- sum(cust1 == FALSE & cust2 == FALSE)
  PA <- sum(cust1 == TRUE & cust2 == FALSE)
  AP <- sum(cust1 == FALSE & cust2 == TRUE)
  
  return(list(CP = CP, CA = CA, PA = PA, AP = AP))
}

# Apply the function to calculate similarity values between the query and each customer
query_customer <- df6[1, 2:6]  # Exclude the ID column
customer_1 <- df5[1, 2:6]  # First customer
customer_2 <- df5[2, 2:6]  # Second customer

# Calculate for customer 1
similarity_cust1 <- calculate_similarity_values(query_customer, customer_1)

# Calculate for customer 2
similarity_cust2 <- calculate_similarity_values(query_customer, customer_2)

# Output the results
print("Similarity values with customer 1:")

print(similarity_cust1)

print("Similarity values with customer 2:")

print(similarity_cust2)

# Function to calculate the similarity indexes
calculate_indexes <- function(similarity_values, P) {
  CP <- similarity_values$CP
  CA <- similarity_values$CA
  PA <- similarity_values$PA
  AP <- similarity_values$AP
  
  # Russell-Rao
  russell_rao <- CP / P
  
  # Sokal-Michener
  sokal_michener <- (CP + CA) / P
  
  # Jaccard
  denominator_jaccard <- CP + PA + AP
  if (denominator_jaccard == 0) {
    jaccard <- 0  # Handle division by zero
  } else {
    jaccard <- CP / denominator_jaccard
  }
  
  return(list(Russell_Rao = russell_rao, Sokal_Michener = sokal_michener, Jaccard = jaccard))
}

# P represents the total number of items, which is 5 in this case.
P <- ncol(df6) - 1  # Total number of items, excluding the ID column

# Calculate similarity indexes for customer 1
indexes_cust1 <- calculate_indexes(similarity_cust1, P)

# Calculate similarity indexes for customer 2
indexes_cust2 <- calculate_indexes(similarity_cust2, P)

# Output similarity indexes
print("Similarity indexes with customer 1:")

print(indexes_cust1)

print("Similarity indexes with customer 2:")

print(indexes_cust2)

# Function to calculate Jaccard Index
calculate_jaccard <- function(similarity_values) {
  CP <- similarity_values$CP
  PA <- similarity_values$PA
  AP <- similarity_values$AP
  
  denominator_jaccard <- CP + PA + AP
  if (denominator_jaccard == 0) {
    return(0)  # Handle division by zero
  } else {
    return(CP / denominator_jaccard)
  }
}

# Calculate Jaccard similarity for both customers
jaccard_cust1 <- calculate_jaccard(similarity_cust1)
jaccard_cust2 <- calculate_jaccard(similarity_cust2)

# Output the Jaccard similarity values
print(paste("Jaccard similarity with customer 1:", jaccard_cust1))

print(paste("Jaccard similarity with customer 2:", jaccard_cust2))

# Find the most similar customer (choose the one with higher Jaccard index)
if (jaccard_cust1 > jaccard_cust2) {
  most_similar_customer <- 1
} else if (jaccard_cust2 > jaccard_cust1) {
  most_similar_customer <- 2
} else {
  # If both are equal, select randomly
  most_similar_customer <- sample(c(1, 2), 1)
}

print(paste("The most similar customer is customer:", most_similar_customer))

# Step 3: Recommend items
# Get the purchase data for the most similar customer and the query customer
similar_customer_purchases <- df5[most_similar_customer, 2:6]  # Exclude the ID column
query_customer_purchases <- df6[1, 2:6]  # Exclude the ID column

# Recommend items that the similar customer has bought but the query customer has not
recommended_items <- names(which(similar_customer_purchases == TRUE & query_customer_purchases == FALSE))

# Output the recommended items
if (length(recommended_items) > 0) {
  print(paste("Items to recommend to the query customer:", paste(recommended_items, collapse = ", ")))
} else {
  print("No items to recommend to the query customer.")
}

