# compute P-values for case 0 from table 4.2

#' Case 0 P-value (interval) from test statistic, list of critical values and list of corresponding P-values.
#' Helper function, with specific for each test statistic.
#'
#' @param stat Test statistic value
#' @param cv Vector of critical values to look test statistic up in
#' @param pv Vector of P-values corresponding to the critical values
#'
#' @return P-value interval (as a level of a factor)
#' @export
pval.tab=function(stat,cv,pv) {
  cvs=c(0,cv,1e10)
  pvs=c(1,pv,0)
  i=findInterval(stat,cvs)
  if (i==0) 1 else cut(pvs,pvs)[i]
}

#' P-value for A-squared from table 4.2
#'
#' @param stat Test statistic value
#' @return P-value interval (as level of factor)
#' @examples
#' pval.a2(1)
#' pval.a2(2)
#' pval.a2(3)
#' pval.a2(5)
#' pval.a2(10)
#' @export

pval.a2=function(stat) {
  cv=c(1.248,1.610,1.933,2.492,3.070,3.880,4.500,6.000)
  pv=c(0.25,0.15,0.10,0.05,0.025,0.01,0.005,0.001)
  pval.tab(stat,cv,pv)
}

#' P-value for D (modified) from table 4.2
#'
#' @param stat Test statistic value
#' @return P-value interval (as level of factor)
#' @examples
#' pval.d(1)
#' pval.d(1.25)
#' pval.d(1.5)
#' pval.d(1.75)
#' pval.d(2)
#' @export

pval.d=function(stat) {
  cv=c(1.019,1.138,1.224,1.358,1.480,1.628,1.731,1.950)
  pv=c(0.25,0.15,0.10,0.05,0.025,0.01,0.005,0.001)
  pval.tab(stat,cv,pv)
}
#' P-value for V (modified) from table 4.2
#'
#' @param stat Test statistic value
#' @return P-value interval (as level of factor)
#' @examples
#' pval.v(1.4)
#' pval.v(1.65)
#' pval.v(1.9)
#' pval.v(2.15)
#' pval.v(2.4)
#' @export

pval.v=function(stat) {
  cv=c(1.420,1.537,1.620,1.747,1.862,2.001,2.098,2.303)
  pv=c(0.25,0.15,0.10,0.05,0.025,0.01,0.005,0.001)
  pval.tab(stat,cv,pv)
}

#' P-value for W-squared (modified) from table 4.2
#'
#' @param stat Test statistic value
#' @return P-value interval (as level of factor)
#' @examples
#' pval.w2(0.2)
#' pval.w2(0.45)
#' pval.w2(0.7)
#' pval.w2(0.95)
#' pval.w2(1.2)
#' @export

pval.w2=function(stat) {
  cv=c(0.209,0.284,0.347,0.461,0.581,0.743,0.869,1.147)
  pv=c(0.25,0.15,0.10,0.05,0.025,0.01,0.005,0.001)
  pval.tab(stat,cv,pv)
}
#' P-value for U-squared (modified) from table 4.2
#'
#' @param stat Test statistic value
#' @return P-value interval (as level of factor)
#' @examples
#' pval.u2(0.1)
#' pval.u2(0.175)
#' pval.u2(0.25)
#' pval.u2(0.325)
#' pval.u2(0.4)
#' @export

pval.u2=function(stat) {
  cv=c(0.105,0.131,0.152,0.187,0.222,0.268,0.304,0.385)
  pv=c(0.25,0.15,0.10,0.05,0.025,0.01,0.005,0.001)
  pval.tab(stat,cv,pv)
}

