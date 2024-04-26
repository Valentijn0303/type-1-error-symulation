# Set seed for reproducibility
set.seed(40)

# Sample size
n <- 200

# Number of datasets to simulate
num_datasets <- 10000 

# Initialize vectors to store p-values and decisions
p_values_outliers <- numeric(num_datasets)
decisions_outliers <- character(num_datasets)
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
  
  # Check for outliers and remove them based on the t-test results
  if (t_test_result$p.value < 0.05) {
    # If p-value is less than 0.05, remove outliers based on a more lenient criterion
    gpa_coffee <- gpa_coffee[abs(gpa_coffee - mean(gpa_coffee)) < 1.5 * sd(gpa_coffee)]
    gpa_no_coffee <- gpa_no_coffee[abs(gpa_no_coffee - mean(gpa_no_coffee)) < 1.5 * sd(gpa_no_coffee)]
  } else {
    # If p-value is not less than 0.05, remove outliers based on a stricter criterion
    gpa_coffee <- gpa_coffee[abs(gpa_coffee - mean(gpa_coffee)) < 1 * sd(gpa_coffee)]
    gpa_no_coffee <- gpa_no_coffee[abs(gpa_no_coffee - mean(gpa_no_coffee)) < 1 * sd(gpa_no_coffee)]
  }
  
  # Perform t-test after outlier removal
  t_test_result_outliers <- t.test(gpa_coffee, gpa_no_coffee)
  
  # Store p-value and decision
  p_values_outliers[i] <- t_test_result_outliers$p.value
  decisions_outliers[i] <- ifelse(t_test_result_outliers$p.value < 0.05, "Reject H0", "Fail to reject H0")
}


# Display summary of results
cat("Type 1 error rate (outlier removal):", mean(p_values_outliers < 0.05), "\n")
cat("Type 1 error rate (no modification):", mean(p_values_no_modification < 0.05), "\n")
cat(table(decisions_outliers), "\n")
cat(table(decisions_no_modification), "\n")

# Create a histogram of the p-values without modification
hist(p_values_no_modification, breaks = 50, col = rgb(0, 0, 1, 0.5), 
     main = "Distribution of p-values", xlab = "P-value", 
     xlim = c(0, 1), ylim = c(0, max(hist(p_values_no_modification, plot = FALSE)$counts, 
                                      hist(p_values_outliers, plot = FALSE)$counts)))

# Add a histogram of the p-values with outlier removal
hist(p_values_outliers, breaks = 50, col = rgb(1, 0, 0, 0.5), add = TRUE)

# Add a legend
legend("topright", legend = c("No modification", "Outlier removal"), 
       fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))