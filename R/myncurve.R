#' Normal Distribution Curve
#'
#' This function plots a normal distribution curve with mean `mu` and standard deviation `sigma`,
#' and shades the area under the curve from negative infinity to a specified value `a`.
#' It also calculates and returns the cumulative probability `P(X <= a)`.
#' @param mu Numeric. The mean of the normal distribution.
#' @param sigma Numeric. The standard deviation of the normal distribution.
#' @param a Numeric. The x-value up to which the area under the curve will be shaded.
#' @return A list of mean, standard deviation, and probability.
#' @export
#'
#' @examples myncurve(2, 4, 3)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean = mu,sd = sigma), xlim = c(mu- 3*sigma, mu + 3*sigma),
        main = "NORMAL DISTRIBUTION", ylab = "DENSITY")

  xcurve <- seq(mu - 3 * sigma, a, length = 1000)
  ycurve <- dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(mu - 3 * sigma, xcurve, a), c(0, ycurve, 0), col = "aquamarine")

  prob <- pnorm(a, mean = mu, sd = sigma)
  prob <- round(prob, 4)
  text((a + mu - 3 * sigma) / 2, .05, paste0("AREA = ", prob))


  list(mu = mu, sigma = sigma, prob)
}
