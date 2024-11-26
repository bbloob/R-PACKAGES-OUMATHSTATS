#' Confidence Interval for the Mean
#'
#' @param x A numeric vector representing the sample data.
#'
#' @return A numeric vector of length 2 containing the lower and upper bounds of the 95% confidence interval for the mean.
#' @export
#'
#' @examples
#' x <- rnorm(30, mean = 10, sd = 12)
#' myci(x)
myci <- function(x) {
  # Calculate sample mean and sample size
  sample_mean <- mean(x)
  sample_sd <- sd(x)
  n <- length(x)

  # Calculate the t critical value for a 95% confidence level
  t_value <- qt(0.975, df = n - 1)

  # Calculate the margin of error
  margin_of_error <- t_value * (sample_sd / sqrt(n))

  # Calculate the confidence interval
  lower_bound <- sample_mean - margin_of_error
  upper_bound <- sample_mean + margin_of_error

  # Return the confidence interval as a vector
  c(lower_bound, upper_bound)
}
