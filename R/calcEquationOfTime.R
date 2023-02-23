# Function calcEquationOfTime(t)

# '***********************************************************************/
# '* Name:    calcEquationOfTime
# '* Type:    Function
# '* Purpose: calculate the difference between true solar time and mean
# '*     solar time
# '* Arguments:
#   '*   t : number of Julian centuries since J2000.0
# '* Return value:
#   '*   equation of time in minutes of time
# '***********************************************************************/

#' Calculate equation of time
#' @description Calculates the difference between true solar time and mean
#  solar time.
#' @param t Number of Julian centuries since J2000.0
#'
#' @return Equation of time in minutes of time
#' @family Solar system
#' @export
calcEquationOfTime <- function(t) {
  vepsilon = calcObliquityCorrection(t)
  l0 = calcGeomMeanLongSun(t)
  ve = calcEccentricityEarthOrbit(t)
  vm = calcGeomMeanAnomalySun(t)

  y = (tan(degToRad(vepsilon) / 2))^2

  sin2l0 = sin(2 * degToRad(l0))
  sinm = sin(degToRad(vm))
  cos2l0 = cos(2 * degToRad(l0))
  sin4l0 = sin(4 * degToRad(l0))
  sin2m = sin(2 * degToRad(vm))
  Etime = y * sin2l0 - 2 * ve * sinm + 4 * ve * y * sinm * cos2l0 - 0.5 * y * y * sin4l0 - 1.25 * ve * ve * sin2m

  return(radToDeg(Etime) * 4)

}
