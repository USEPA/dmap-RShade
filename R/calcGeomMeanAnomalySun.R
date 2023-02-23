# Function calcGeomMeanAnomalySun(t)

# **********************************************************************/
#  Name:    calGeomAnomalySun
#  Type:    Function
#  Purpose: calculate the Geometric Mean Anomaly of the Sun
#  Arguments:
#    t : number of Julian centuries since J2000.0
#  Return value:
#    the Geometric Mean Anomaly of the Sun in degrees
# **********************************************************************/

#' Calculate Geometric Mean Anomaly of the Sun
#' @description Calculates the Geometric Mean Anomaly of the Sun.
#' @param t Number of Julian centuries since J2000.0
#' @return numeric (the Geometric Mean Anomaly of the Sun in degrees)
#' @family Solar system
#' @export
calcGeomMeanAnomalySun <- function(t) {
  m = 357.52911 + t * (35999.05029 - 0.0001537 * t)

  return(m)

}
