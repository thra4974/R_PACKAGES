#' Produce a normal curve with shaded region
#'
#' myncurve takes in a mean,  std dev. and an a limit
#'
#' @name myncurve
#'
#' @param mu mean
#'
#' @param sigma standard deviation
#'
#' @param a region limit
#'
#' @return area
#'
#' @examples
#' myncurve(mu=10, sigma=5, a=6)
#'
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#'
#' @export
globalVariables("x")

myncurve <- function(mu, sigma, a) {
  curve(dnorm(x,mean=mu, sd=sigma), xlim=c(mu-3*sigma,
                                           mu+3*sigma))
  xcurve = seq(mu-3*sigma, a, length = 1000) #x values
  ycurve = dnorm(xcurve, mu, sigma) #y values corresponding to x curve
  polygon(c(mu-3*sigma, xcurve, a), c(0, ycurve, 0), col="slateblue1") #fill in area
  title(main = "my n curve")

  #calculate area
  area = round(pnorm(a, mu, sigma) - pnorm(mu-3*sigma, mu, sigma),4)
  text(x=a, y=0.5*dnorm(a, mu, sigma),
       paste0("Area = ", area))
  return(area)
}
