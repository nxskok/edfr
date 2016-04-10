# worker functions that compute each of the statistics as described in D'Agostino & Stephens (1986) p. 101

#' @export

dplus=function(x) {
  n=length(x)
  v=1:n/n-sort(x)
  max(v)
}

#' @export

dminus=function(x) {
  n=length(x)
  v=sort(x)-(0:(n-1))/n
  max(v)
}

#' @export

d=function(x) {
  max(dplus(x),dminus(x))
}

#' @export

v=function(x) {
  dplus(x)+dminus(x)
}

#' @export

w2=function(x) {
  n=length(x)
  I=1:n
  v=(2*I-1)/2/n
  w=(x-v)^2
  sum(w)+1/12/n
}

#' @export


u2=function(x) {
  n=length(x)
  zbar=mean(x)
  w2(x)-n*(zbar-0.5)^2
}

#' Anderson-Darling A-squared
#'
#' @param x sorted, transformed data
#' @export

a2=function(x) {
  n=length(x)
  I=1:n
  v=2*I-1
  w=2*n+1-2*I
  total=v*log(x)+w*log(1-x)
  -n-sum(total)/n
}

#' @export

dmod=function(x){
  n=length(x)
  d(x)*(sqrt(n)+0.12+0.11/sqrt(n))
}

#' @export

vmod=function(x) {
  n=length(x)
  v(x)*(sqrt(n)+0.155+0.24/sqrt(n))
}

#' @export

w2mod=function(x) {
  n=length(x)
  (w2(x)-0.4/n+0.6/n^2)/(1+1/n)
}

#' @export

u2mod=function(x) {
  n=length(x)
  (u2(x)-0.1/n+0.1/n^2)/(1+0.8/n)
}
