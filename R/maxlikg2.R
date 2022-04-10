#' Produce maximum likelihood estimates for joint density
#'
#' maxlikg2 takes in a joint density function (binomialxpoisson) and returns max likelihood est.
 #'
#' @name maxlikg2
#'
#' @param theta1 parameter for binomial
#'
#' @param theta2 parameter for poisson
#'
#' @param lfun logbinpois function
#'
#' @param ... arguments
#'
#' @examples
#' logbinpois=function(theta1,theta2) log(dbinom(3,size=10,prob=theta1)) + log(dpois(4,lambda=theta2))
#' maxlikg2(theta1=seq(0,1,length=1000),theta2=seq(0,10,length=1000),nlevels=10)
#'
#' @importFrom graphics curve hist abline contour
#'
#' @export
maxlikg2=function(theta1=seq(0,1,length=1000),theta2=seq(0,10,length=1000),lfun="logbinpois",...){
  n1=length(theta1)
  n2=length(theta2)
  z=outer(theta1,theta2,lfun)
  contour(theta1,theta2,exp(z),...) # exp(z) gives the lik
  maxl=max(exp(z))    # max lik
  coord=which(exp(z)==maxl,arr.ind=TRUE)  # find the co-ords of the max
  th1est=theta1[coord[1]] # mxlik estimate of theta1
  th2est=theta2[coord[2]]
  abline(v=th1est,h=th2est)
  axis(3,th1est,round(th1est,2))
  axis(4,th2est,round(th2est,2),las=1)
  list(th1est=th1est,th2est=th2est)
}
