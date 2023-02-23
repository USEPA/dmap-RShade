# Function for converting from degrees to radians
# input = x [Angle in degrees]
# return = an angle in radians

#' Degrees to Radians
#' @description Function for converting from degrees to radians.
#' @param x Angle in degrees
#' @return numeric (an angle in radians)
#' @family Data wrangling
#' @export
degToRad <- function(x){return(x * pi / 180)}
