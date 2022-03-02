#' Lab 6 Function: myncurve()
#'
#' @param mu Mean of the normal distribution
#' @param sigma Standard deviation of the normal distribution
#' @param a Upper bound of the lower tail
#'
#' @return Displays the curve, shaded area between the curve and x axis from -∞ to x=a, and calculate the area (probability, P(X<=a)) which is released to the command-line in a list.
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), col = "black", lwd = 2, ylab = "Density")
  xcurve <- seq(qnorm(0.000001, mu, sigma), a, length.out = 1000)
  ycurve <- dnorm(xcurve, mu, sigma)
  polygon(c(qnorm(0.000001, mu ,sigma), xcurve, a), c(0, ycurve, 0), col = "plum")
  prob <- round(pnorm(a, mu, sigma), 4)
  text(a, dnorm(a, mu, sigma) / 2, paste0("Area = ", prob))
  return(prob)
}
