#' Calculate an EDF statistic
#'
#' Calculate test statistic value (and P-value?) for testing fit to specified distribution.
#'
#' @param statfun Name of one of the EDF statistics (see Details)
#' @param x Vector of data to be tested for fit to the distribution
#' @param dist Cumulative distribution function of distribution to be fitted
#' @param ... parameters to distribution
#'
#' @details Allowable test statistics are \code{d} (Kolmogorov D), \code{v} (Kuiper V), \code{w2} (Cramer-von Mises W-squared),
#'   \code{a2} (Anderson-Darling A-squared), \code{u2} (Watson U-squared). Statistics \code{v} and \code{u2} are invariant to choice
#'   of origin, so can be used for data on a circle (for example, for testing uniformity of times of day). Also modified versions
#'   \code{dmod}, \code{vmod}, \code{w2mod}, \code{u2mod} for use with Table 4.3 of D'Agostino and Stephens (1986).
#'
#' @examples
#' data(leghorn)
#' # test the transformed data for uniformity (default)
#' calc.stat(a2,leghorn$z1)
#' # test original data for normality with mean 200 and SD 35
#' calc.stat("a2",leghorn$x,pnorm,200,35)
#' # obtain all statistics
#' my.stats=c("d","v","w2","a2","u2","dmod","vmod","w2mod","u2mod")
#' sapply(my.stats,calc.stat,leghorn$x,pnorm,200,35)
#' # data that are actually beta-distributed
#' data(beta_data)
#' sapply(my.stats,calc.stat,beta_data)
#' @export

calc.stat=function(statfun,x,dist=punif,...) {
  # dist needs to be a p cdf, with parameters on the end
  f=match.fun(statfun)
  z=sort(dist(x,...))
  f(z)
}

# simulation

#' Simulate EDF statistics under Case 0
#'
#' Obtain simulations of test statistic under assumption that parameters known (for getting P-values).
#' Case 3 (all parameters unknown) coming up later. Add examples.
#'
#' @param statfun Name of statistic to be simulated (as in help for \code{calc.stat})
#' @param n Sample size for each simulated test statistic
#' @param nsim Number of simulations to run (default 10000)
#' @param dist Name of R function to draw random samples from distribution (eg. \code{rnorm}). Defaults to uniform (\code{runif}).
#' @param ... parameters for distribution

sim.stat=function(statfun,n,nsim=10000,dist=runif,...) {
  # dist this time has r on front
  # how to handle case 3? Need to estimate parameters from each simulated data set
  # have to handle it case by case, I think
  z=sort(dist(n,...))
  calc.stat(statfun,z)
}


# do I enter a string or a function name?
