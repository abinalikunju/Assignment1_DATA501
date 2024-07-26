shapiro_wilk_test <- function(data, qq_plot = FALSE) {
  # validation
  if (!is.vector(data) || !is.numeric(data)) {
    stop("Input must be a numeric vector")
  }
  
  if (length(data) < 3 || length(data) > 5000) {
    stop("Input vector must have between 3 and 5000 elements")
  }
  
  if (any(is.na(data))) {
    stop("Data contains NA values")
  }
  
  if (any(is.infinite(data))) {
    stop("Data contains infinite values")
  }
  
  # Calculate W statistic
  n <- length(data)
  sorted_data <- sort(data)
  m <- qnorm((1:n - 0.375) / (n + 0.25))
  m <- m / sqrt(sum(m^2))
  W <- (sum(m * sorted_data))^2 / sum((sorted_data - mean(sorted_data))^2)
  
  # Optional QQ-plot
  if (qq_plot) {
    qqnorm(data)
    qqline(data, col = "red")
  }
  
  return(W)
}