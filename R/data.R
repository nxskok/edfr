#' @title Leghorn data
#' @description Weights of 20 Leghorn chicks. Series A.
#' @format A data frame with three variables:
#' \describe{
#'   \item{x}{Weight, in grams}
#'   \item{z1}{Transformed values based on normal with mean 200 and SD 35}
#'   \item{z2}{Transformed values based on normal using sample mean and SD}
#' }
#' @source D'Agostino and Stephens (1986) page 546
"leghorn"

#' @title Beta-distributed data
#' @description Some randomly-generated Beta(1.2,1) data for testing against normality
#' @format Vector of 50 observations
#' @source Generated by package author
"beta_data"

#' @title Data on a circle
#' @description TIny sample of values on circle of circumference 1.
#' Used to illustrate that moving these values around the circle (eg.
#' by subtracting 0.25 from all of them, which, in a clockwise orientation, will change the origin from
#' North to East) the statistics V and U-squared will not change, even though the others
#' change.
#' @format Vector of 4 observations
#' @source D'Agostino and Stephens (1986) page 107
"circle"

#' @title Bliss leghorn data series B
#' @description Weights of 21 leghorn chicks (grams, measured at 21 days). Series A is data \code{leghorn}.
#' @format Vector of 21 values
#' @source D'Agostino and Stephens (1986) page 546.
"blis.b"

#' @title Lethal dose of cinobufagin
#' @description Lethal dose of drug cinobufagin in units of 10 mg/kg, as determined by titration
#' to cardiac arrest in individual etherized cats
#' @format Vector of 25 values
#' @source D'Agostino and Stephens (1986) page 547.
"chen"

#' @title Heights of maize plants
#' @description Heights of maize plants in decimeters (rounded to nearest decimeter)
#' @format Vector of 530 values
#' @source D'Agostino and Stephens (1986) page 548.
"emea"

#' @title Differences in flood stages between two stations
#' @description Differences in flood stages for two stations on the Fox River, Wisconsin. For testing for Laplace distribution.
#' @format  Vector of 33 values
#' @source D'Agostino and Stephens (1986) page 549.
"baen"
