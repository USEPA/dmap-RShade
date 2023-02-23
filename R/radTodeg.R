# Function for converting from radians to degrees
# input =  x [Angle in radians]
# return = an angle in degrees


#' Radians to degrees
#'
#' @param x Radians.
#'
#' @return numeric (degrees)
#' @export
#' @family Data wranling
radToDeg <- function(x){return(x * 180 / pi)}
