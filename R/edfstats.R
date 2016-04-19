# worker functions that compute each of the statistics as described in D'Agostino & Stephens (1986) p. 101

#' Kolmogorov D+ statistic
#'
#' Calculate D+ statistic (from transformed data) as per formula given in D'Agostino and Stephens (1986) page 101
#'
#' @param x vector of data that have been probability-integral-transformed
#' @return test statistic value
#'
#' @export
dplus=function(x) {
  n=length(x)
  v=1:n/n-sort(x)
  max(v)
}

#' Kolmogorov D- statistic
#'
#' Calculate D- statistic (from transformed data) as per formula given in D'Agostino and Stephens (1986) page 101
#'
#' @param x vector of data that have been probability-integral-transformed
#' @return test statistic value
#'
#' @export

dminus=function(x) {
  n=length(x)
  v=sort(x)-(0:(n-1))/n
  max(v)
}


#' Kolmogorov D statistic
#'
#' Calculate D statistic (from transformed data) as per formula given in D'Agostino and Stephens (1986) page 101
#'
#' @param x vector of data that have been probability-integral-transformed
#' @return test statistic value
#'
#' @export
d=function(x) {
  max(dplus(x),dminus(x))
}

#' Kuiper V statistic
#'
#' Calculate V statistic (from transformed data) as per formula given in D'Agostino and Stephens (1986) page 101
#'
#' @param x vector of data that have been probability-integral-transformed
#' @return test statistic value
#'
#' @export


#' @export

v=function(x) {
  dplus(x)+dminus(x)
}

#' Cramer-von Mises W-squared statistic
#'
#' Calculate W-squared statistic (from transformed data) as per formula given in D'Agostino and Stephens (1986) page 101
#'
#' @param x vector of data that have been probability-integral-transformed
#' @return test statistic value
#'
#' @export
w2=function(x) {
  n=length(x)
  I=1:n
  v=(2*I-1)/2/n
  w=(x-v)^2
  sum(w)+1/12/n
}


#' Watson U-squared statistic
#'
#' Calculate U-squared statistic (from transformed data) as per formula given in D'Agostino and Stephens (1986) page 101
#'
#' @param x vector of data that have been probability-integral-transformed
#' @return test statistic value
#'
#' @export
u2=function(x) {
  n=length(x)
  zbar=mean(x)
  w2(x)-n*(zbar-0.5)^2
}

#' Anderson-Darling A-squared statistic
#'
#' Calculate A-squared statistic (from transformed data) as per formula given in D'Agostino and Stephens (1986) page 101
#'
#' @param x vector of data that have been probability-integral-transformed
#' @return test statistic value
#'
#' @export
a2=function(x) {
  n=length(x)
  I=1:n
  v=2*I-1
  w=2*n+1-2*I
  total=v*log(x)+w*log(1-x)
  -n-sum(total)/n
}


#' Modified D statistic
#'
#' Calculate modified D statistic (from transformed data) as per formula given in D'Agostino and Stephens (1986) page 105
#'
#' @param x vector of data that have been probability-integral-transformed
#' @return test statistic value
#'
#' @export
dmod=function(x){
  n=length(x)
  d(x)*(sqrt(n)+0.12+0.11/sqrt(n))
}


#' Modified V statistic
#'
#' Calculate modified V statistic (from transformed data) as per formula given in D'Agostino and Stephens (1986) page 105
#'
#' @param x vector of data that have been probability-integral-transformed
#' @return test statistic value
#'
#' @export
vmod=function(x) {
  n=length(x)
  v(x)*(sqrt(n)+0.155+0.24/sqrt(n))
}

#' Modified W-squared statistic
#'
#' Calculate modified W-squared statistic (from transformed data) as per formula given in D'Agostino and Stephens (1986) page 105
#'
#' @param x vector of data that have been probability-integral-transformed
#' @return test statistic value
#'
#' @export
w2mod=function(x) {
  n=length(x)
  (w2(x)-0.4/n+0.6/n^2)/(1+1/n)
}

#' Modified U-squared statistic
#'
#' Calculate modified U-squared statistic (from transformed data) as per formula given in D'Agostino and Stephens (1986) page 105
#'
#' @param x vector of data that have been probability-integral-transformed
#' @return test statistic value
#'
#' @export
u2mod=function(x) {
  n=length(x)
  (u2(x)-0.1/n+0.1/n^2)/(1+0.8/n)
}
