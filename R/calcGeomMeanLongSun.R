# calcGeomMeanLongSun Function

# **********************************************************************/
#  Name:    calGeomMeanLongSun
#  Type:    Function
#  Purpose: calculate the Geometric Mean Longitude of the Sun
#  Arguments:
#    t : number of Julian centuries since J2000.0
#  Return value:
#    the Geometric Mean Longitude of the Sun in degrees
# **********************************************************************/

#' Calculate the Geometric Mean Longitude of the Sun
#' @description Calculates the Geometric Mean Longitude of the Sun.
#' @param t Number of Julian centuries since J2000.0
#' @return numeric (the Geometric Mean Longitude of the Sun in degrees)
#' @family Solar system
#' @export
calcGeomMeanLongSun <- function(t) {

  l0 = 280.46646 + t * (36000.76983 + 0.0003032 * t)

  if(l0 <= 360 & l0 >= 0) {
    l0 = l0
  } else if(l0 > 360) {
    l0 = l0 - 360
  } else {
    l0 = l0 + 360
  }

  return(l0)

}
