# Test 1: Normal distribution
set.seed(123)
normal_data <- rnorm(100)
normal_result <- shapiro_wilk_test(normal_data)
cat("Test 1 (Normal distribution):\n")
cat("W statistic:", normal_result, "\n\n")

# Test 2: Uniform distribution
set.seed(456)
uniform_data <- runif(100)
uniform_result <- shapiro_wilk_test(uniform_data, qq_plot = TRUE)
cat("Test 2 (Uniform distribution):\n")
cat("W statistic:", uniform_result, "\n")

# Test 3: NA values
na_data <- c(1, 2, NA, 4, 5)
tryCatch({
  shapiro_wilk_test(na_data)
}, error = function(e) {
  cat("Test 3 (NA values):\n")
  cat("Error message:", e$message, "\n\n")
})

# Test 4: Infinite values
inf_data <- c(1, 2, Inf, 4, 5)
tryCatch({
  shapiro_wilk_test(inf_data)
}, error = function(e) {
  cat("Test 4 (Infinite values):\n")
  cat("Error message:", e$message, "\n\n")
})

# Test 5: Wrong format (non-numeric)
wrong_format_data <- c("a", "b", "c")
tryCatch({
  shapiro_wilk_test(wrong_format_data)
}, error = function(e) {
  cat("Test 5 (Wrong format):\n")
  cat("Error message:", e$message, "\n\n")
})

# Test 6: Wrong dimensions (too few elements)
short_data <- c(1, 2)
tryCatch({
  shapiro_wilk_test(short_data)
}, error = function(e) {
  cat("Test 6 (Wrong dimensions - too few elements):\n")
  cat("Error message:", e$message, "\n\n")
})

# Test 7: Wrong dimensions (too many elements)
long_data <- rnorm(5001)
tryCatch({
  shapiro_wilk_test(long_data)
}, error = function(e) {
  cat("Test 7 (Wrong dimensions - too many elements):\n")
  cat("Error message:", e$message, "\n\n")
})