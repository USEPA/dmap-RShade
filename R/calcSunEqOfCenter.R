# Function calcSunEqOfCenter(t)

# **********************************************************************/
#  Name:    calcSunEqOfCenter
#  Type:    Function
#  Purpose: calculate the equation of center for the sun
#  Arguments:
#    t : number of Julian centuries since J2000.0
#  Return value:
#    in degrees
# **********************************************************************/

#' Calculate Sun Equation of Of Center for the Sun
#' @description Calculate the equation of center for the Sun.
#' @param t Number of Julian centuries since J2000.0.
#' @return numeric (degrees)
#' @family Solar system
#' @export
calcSunEqOfCenter <- function(t) {
  m = calcGeomMeanAnomalySun(t)

  mrad = degToRad(m)
  sinm = sin(mrad)
  sin2m = sin(mrad + mrad)
  sin3m = sin(mrad + mrad + mrad)

  suncen = sinm * (1.914602 - t * (0.004817 + 0.000014 * t)) + sin2m * (0.019993 - 0.000101 * t) + sin3m * 0.000289

  return(suncen)
}
