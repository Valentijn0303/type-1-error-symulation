# Set seed for reproducibility
set.seed(40)

# Sample size
n <- 500

# Number of datasets to simulate
num_datasets <- 10000  

# Initialize vectors to store p-values and decisions
p_values_rounded <- numeric(num_datasets)
decisions_rounded <- character(num_datasets)
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
  
  # Apply QRP: Round p-values down (only if less than or equal to 0.059)
  rounded_p_value <- ifelse(t_test_result$p.value <= 0.059, 0.05, t_test_result$p.value)
    
  # Store rounded p-value and decision
  p_values_rounded[i] <- rounded_p_value
  decisions_rounded[i] <- ifelse(rounded_p_value <= 0.05, "Reject H0", "Fail to reject H0")
  
  # Store unrounded p-value and decision
  p_values_no_modification[i] <- t_test_result$p.value
  decisions_no_modification[i] <- ifelse(t_test_result$p.value <= 0.05, "Reject H0", "Fail to reject H0")
}

# Display summary of results
cat("Type 1 error rate (rounded p-values):", mean(p_values_rounded <= 0.05), "\n")
cat("Type 1 error rate (unrounded p-values):", mean(p_values_no_modification <= 0.05), "\n")
cat(table(decisions_rounded), "\n")
cat(table(decisions_no_modification), "\n")
cat(sum(decisions_no_modification == "Reject H0"), "\n")
cat(sum(decisions_no_modification == "Fail to reject H0"), "\n")

# Create a histogram of the p-values without modification
hist(p_values_no_modification, breaks = 50, col = rgb(0, 0, 1, 0.5), 
     main = "Distribution of p-values", xlab = "P-value", 
     xlim = c(0, 1), ylim = c(0, max(hist(p_values_no_modification, plot = FALSE)$counts, 
                                      hist(p_values_rounded, plot = FALSE)$counts)))

# Add a histogram of the p-values with outlier removal
hist(p_values_rounded, breaks = 50, col = rgb(1, 0, 0, 0.5), add = TRUE)

# Add a legend
legend("topright", legend = c("No modification", "P rounding"), 
       fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))