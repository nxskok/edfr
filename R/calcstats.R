#' Calculate an EDF statistic
#'
#' Calculate test statistic value (and P-value?) for testing fit to specified distribution.
#'
#' @param statfun Name of one of the EDF statistics (see Details)
#' @param x Vector of data to be tested for fit to the distribution
#' @param dist Cumulative distribution function of distribution to be fitted
#' @param ... parameters to distribution
#'
#' @details Allowable test statistics are \code{d} (Kolmogorov D), \code{v} (Kuiper V),
#' \code{w2} (Cramer-von Mises W-squared),
#'   \code{a2} (Anderson-Darling A-squared), \code{u2} (Watson U-squared).
#'   Statistics \code{v} and \code{u2} are invariant to choice
#'   of origin, so can be used for data on a circle (for example, for testing
#'   uniformity of times of day). Also modified versions
#'   \code{dmod}, \code{vmod}, \code{w2mod}, \code{u2mod} for use with Table 4.2
#'   of D'Agostino and Stephens (1986).
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
#' @references D'Agostino and Stephens (1986) Goodness of Fit Techniques, Chapter 4.

calc.stat=function(statfun,x,dist=punif,...) {
  # dist needs to be a p cdf, with parameters on the end
  f=match.fun(statfun)
  z=sort(dist(x,...))
  f(z)
}

# simulation

#' One simulation of a test statistic from given distribution with given parameters (case 0)
#'
#' @param statfun Name of statistic to be simulated (as in help for \code{calc.stat})
#' @param n Sample size for each simulated test statistic
#' @param sim_dist Name of R function to draw random samples from distribution (eg. \code{rnorm}). Defaults to uniform (\code{runif}).
#' @param calc_dist Name of R cumulative distribution to calculate test statistic for (eg. \code{pnorm}). Defaults to uniform (\code{punif}).
#' @param ... parameters for distribution, apparently same for simulation and calculation
#' @export

sim.stat.1=function(statfun,n,sim_dist=runif,calc_dist=punif,...) {
  r=sim_dist(n,...)
  stat=calc.stat(statfun,r,calc_dist,...)
  stat
}


#' Simulate EDF statistics under Case 0
#'
#' Obtain simulations of test statistic under assumption that parameters known (for getting P-values).
#' Case 3 (all parameters unknown) coming up later. Add examples.
#'
#' @param statfun Name of statistic to be simulated (as in help for \code{calc.stat})
#' @param n Sample size for each simulated test statistic
#' @param nsim Number of simulations to run (default 10000)
#' @param sim_dist Name of R function to draw random samples from distribution (eg. \code{rnorm}). Defaults to uniform (\code{runif}).
#' @param calc_dist Name of R function to do probability integral transform with (eg. \code{pnorm}). Defaults to uniform (\code{punif}).
#' @param ... parameters for distribution
#' @export

sim.stat=function(statfun,n,nsim=10000,sim_dist=runif,calc_dist=punif,...) {
  replicate(nsim,sim.stat.1(statfun,n,sim_dist,calc_dist,...))
}

#' EDF test statistic and P-value
#'
#' Calculate EDF test statistic and P-value by simulation
#'
#' @param statfun Character string naming test to do
#' @param x Vector of data
#' @param nsim Number of  simulations to run for P-value (default 10,000)
#' @param sim Distribution to simulate from for P-value (r-function)
#' @param calc Distribution to calculate test statistic for (p-function)
#' @param ... Parameters for distribution
#'
#' @export
p.val=function(statfun,x,nsim=1e4,sim=runif,calc=punif,...) {
  v=calc.stat(statfun,x,dist=calc,...)
  vv=sim.stat(statfun,length(x),sim_dist=sim,calc_dist=calc,...)
  tab=table(vv>=v)
  p.value=tab[2]/sum(tab)
  list(test.stat=v,p.value=p.value)
}

#' Test statistics and P-values under case 0
#'
#' Obtain test statistics and P-values for all statistics under case 0, by simulation
#'
#' @param x vector of data
#' @param nsim number of simulations to obtain P-value (default 10,000)
#' @param calc Cumulative distribution function of null-hypothesis distribution
#' @param sim Function  to generate random sample of values from null-hypothesis distribution
#' @param ... additional parameters for distribution(s)
#'
#' @return vector of all test statistic values, labelled by which statistic each one is
#' @export
test0=function(x,nsim=1e4,calc=punif,sim=runif,...) {
  stat.list=c("d","v","w2","u2","a2")
  v=sapply(stat.list,p.val,x,nsim,calc=calc,sim=sim,...)
  v
}


