# Set seed for reproducibility
set.seed(40)

# Sample size
n <- 500

# Number of datasets to simulate
num_datasets <- 10000  

# Number of interim analyses
num_interim <- 4

# Initialize vectors to store p-values and decisions for sequential testing
p_values_sequential <- numeric(num_datasets)
decisions_sequential <- character(num_datasets)

# Simulate data and perform sequential t-test for each dataset
for (i in 1:num_datasets) {
  # Initialize vector to store interim p-values
  interim_p_values <- numeric(num_interim)
  
  for (j in 1:num_interim) {
    # Simulate interim GPA data for both groups
    interim_sample_size <- n / num_interim * j
    gpa_coffee <- rnorm(interim_sample_size, mean = 3.5, sd = 0.5)
    gpa_no_coffee <- rnorm(interim_sample_size, mean = 3.5, sd = 0.5)
    
    # Perform interim t-test
    t_test_interim <- t.test(gpa_coffee, gpa_no_coffee)
    
    # Store interim p-value
    interim_p_values[j] <- t_test_interim$p.value
    
    # Optional stopping if significant result is found
    if (t_test_interim$p.value < 0.05) {
      break
    }
  }
  
  # Store final p-value and decision for sequential testing
  p_values_sequential[i] <- tail(interim_p_values, n = 1)
  decisions_sequential[i] <- ifelse(tail(interim_p_values, n = 1) < 0.05, "Reject H0", "Fail to reject H0")
}

# Display summary of results for sequential testing
cat("Type 1 error rate (sequential testing):", mean(p_values_sequential < 0.05), "\n")
