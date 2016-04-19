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
  if (i==0) "1" else as.character(cut(pvs,pvs)[i])
}

#' P-value for A-squared from table 4.2
#'
#' @param stat Test statistic value
#' @return P-value interval (as character representation of interval)
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
#' @return P-value interval (as character representation of interval)
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
#' @return P-value interval (as character representation of interval)
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
#' @return P-value interval (as character representation of interval)
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
#' @return P-value interval (as character representation of interval)
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

#' P-value for D (unmodified) for normal case 3
#'
#' P-value for D for normality when mean and SD estimated
#' @param stat Test statistic value
#' @param n sample size
#' @return P-value interval (as text)
#' @export
pval.d.norm3=function(stat,n) {
  stat.mod=stat*(sqrt(n)-0.01+0.85/sqrt(n))
  cv=c(0.775,0.819,0.895,0.995,1.035)
  pv=c(0.15,0.10,0.05,0.025,0.01)
  pval.tab(stat.mod,cv,pv)
}

#' P-value for V (unmodified) for normal case 3
#'
#' P-value for V for normality when mean and SD estimated
#' @param stat Test statistic value
#' @param n sample size
#' @return P-value interval (as text)
#' @export
pval.v.norm3=function(stat,n) {
  stat.mod=stat*(sqrt(n)+0.05+0.82/sqrt(n))
  cv=c(1.32,1.386,1.489,1.585,1.693)
  pv=c(0.15,0.10,0.05,0.025,0.01)
  pval.tab(stat.mod,cv,pv)
}


#' P-value for W-squared (unmodified) for normal case 3
#'
#' P-value for W-squared for normality when mean and SD estimated
#' @param stat Test statistic value
#' @param n sample size
#' @return P-value interval (as text)
#' @export

pval.w2.norm3=function(stat,n) {
  stat.mod=stat*(1+0.05/n)
  cv=c(0.051,0.074,0.091,0.104,0.126,0.148,0.179,0.201)
  pv=c(0.50,0.25,0.15,0.10,0.05,0.025,0.01,0.005)
  pval.tab(stat.mod,cv,pv)
}


#' P-value for U-squared (unmodified) for normal case 3
#'
#' P-value for U-squared for normality when mean and SD estimated
#' @param stat Test statistic value
#' @param n sample size
#' @return P-value interval (as text)
#' @export

pval.u2.norm3=function(stat,n) {
  stat.mod=stat*(1+0.05/n)
  cv=c(0.048,0.070,0.085,0.096,0.117,0.136,0.164,0.183)
  pv=c(0.50,0.25,0.15,0.10,0.05,0.025,0.01,0.005)
  pval.tab(stat.mod,cv,pv)
}

#' P-value for A-squared (unmodified) for normal case 3
#'
#' P-value for A-squared for normality when mean and SD estimated
#' @param stat Test statistic value
#' @param n sample size
#' @return P-value interval (as text)
#' @export

pval.a2.norm3=function(stat,n) {
  stat.mod=stat*(1+0.75/n+2.25/n^2)
  cv=c(0.341,0.470,0.561,0.631,0.752,0.873,1.035,1.159)
  pv=c(0.50,0.25,0.15,0.10,0.05,0.025,0.01,0.005)
  pval.tab(stat.mod,cv,pv)
}




#' Test statistics and P-values from table
#'
#' Calculate test statistic and obtain P-value from table 4.2 of D'Agostino and Stephens (1986).
#'
#' @param x vector of data
#' @param calc CDF function of distribution to be tested
#' @param ... parameters for distribution
#'
#' @return data frame of test statistics and P-values
#' @export
p.val.tab=function(x,calc,...){
  stat.list=c("d","v","w2","u2","a2")
  mod.stat.list=c("dmod","vmod","w2mod","u2mod","a2")
  v=sapply(stat.list,calc.stat,x,dist=calc,...)
  # get modified stats
  w=sapply(mod.stat.list,calc.stat,x,dist=calc,...)
  # get P-values from table
  pvals=c(pval.d(w[1]),
          pval.v(w[2]),
          pval.w2(w[3]),
          pval.u2(w[4]),
          pval.a2(w[5]))
  data.frame(statistic=stat.list,value=v,mod.value=w,p.value=pvals,row.names=NULL)
}

#' Test statistics and table P-values for normality, parameters estimated
#'
#' calculate test statistics and P-values from table 4.7 of D'Agostino and Stephens (1986)
#' (normal, case 3)
#'
#' @param x vector of data
#'
#' @return data frame of test statistics and P-values
#' @export
p.val.tab.norm3=function(x){
  xbar=mean(x)
  s=sd(x)
  n=length(x)
  stat.list=c("d","v","w2","u2","a2")
  w=sapply(stat.list,calc.stat,x,dist=pnorm,xbar,s)
  # get P-values from table
  pvals=c(pval.d.norm3(w[1],n),
          pval.v.norm3(w[2],n),
          pval.w2.norm3(w[3],n),
          pval.u2.norm3(w[4],n),
          pval.a2.norm3(w[5],n))
  data.frame(statistic=stat.list,value=w,p.value=pvals,row.names=NULL)
}


