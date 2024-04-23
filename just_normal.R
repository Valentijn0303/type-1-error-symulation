# Set seed for reproducibility
set.seed(5)

# Sample size
n <- 200

# Number of datasets to simulate
num_datasets <- 10000  # You can adjust this number as needed

# Initialize vectors to store p-values and decisions
p_values_no_modification <- numeric(num_datasets)
decisions_no_modification <- character(num_datasets)

# Simulate data and perform t-test for each dataset
for (i in 1:num_datasets) {
  # Simulate GPA data for students who drink coffee (group 1)
  gpa_coffee <- rnorm(n, mean = 3.5, sd = 0.5)  # Mean GPA of 3.5 with standard deviation of 0.5
  
  # Simulate GPA data for students who don't drink coffee (group 2)
  gpa_no_coffee <- rnorm(n, mean = 3.5, sd = 0.5)  # Same mean and standard deviation as group 1
  
  # Perform t-test
  t_test_result <- t.test(gpa_coffee, gpa_no_coffee)
  
  # Store p-value and decision without any modification
  p_values_no_modification[i] <- t_test_result$p.value
  decisions_no_modification[i] <- ifelse(t_test_result$p.value < 0.05, "Reject H0", "Fail to reject H0")
}


# Display summary of results
cat("Type 1 error rate:", mean(p_values_no_modification < 0.05), "\n")

