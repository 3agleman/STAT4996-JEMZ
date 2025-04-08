# Set seed for reproducibility
set.seed(123)

# Separate the classes
majority_class <- povertyDepData_cleaned[povertyDepData_cleaned$lifetimeDepression == 0, ]
minority_class <- povertyDepData_cleaned[povertyDepData_cleaned$lifetimeDepression == 1, ]

# (3:1 ratio of no depression and depression)
n_ones <- nrow(minority_class)
n_zeros <- 3 * n_ones

# Randomly sample from the majority class
undersampled_zeros <- majority_class[sample(nrow(majority_class), n_zeros), ]

# Combine
balanced_data <- rbind(minority_class, undersampled_zeros)

balanced_data <- balanced_data[sample(nrow(balanced_data)), ]

# Check new distribution
table(balanced_data$lifetimeDepression)