# Set seed for reproducibility
set.seed(40)

# Sample size for each group
n <- 500

# Number of simulations
num_datasets <- 10000

# Number of dependent variables to simulate
num_dv <- 5

# Initialize vector to store the number of significant tests per dataset
num_significant_tests <- numeric(num_datasets)

# Simulate data and perform t-tests
for (i in 1:num_datasets) {
  significant_dvs <- 0  # Reset counter for significant dependent variables
  for (dv in 1:num_dv) {
    # Simulate GPA data for coffee and no-coffee groups
    # Assuming same mean and sd for all DVs under the null hypothesis
    gpa_coffee <- rnorm(n, mean = 3.5, sd = 0.5)
    gpa_no_coffee <- rnorm(n, mean = 3.5, sd = 0.5)

    # Perform t-test
    t_test_result <- t.test(gpa_coffee, gpa_no_coffee)

    # Check if the p-value is less than 0.05 and count it
    if (t_test_result$p.value < 0.05) {
      significant_dvs <- significant_dvs + 1
    }
  }
  # Store the number of significant DVs for each dataset
  num_significant_tests[i] <- significant_dvs
}

# Display summary of results
cat("Average number of significant tests per dataset:",
    mean(num_significant_tests), "\n")
cat("Number of datasets with at least one significant DV:",
    sum(num_significant_tests > 0), "\n")
