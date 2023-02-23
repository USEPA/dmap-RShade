# Function calcSunDeclination(t)

# '***********************************************************************/
# '* Name:    calcSunDeclination
# '* Type:    Function
# '* Purpose: calculate the declination of the sun
# '* Arguments:
# '*   t : number of Julian centuries since J2000.0
# '* Return value:
# '*   sun's declination in degrees
# '***********************************************************************/
#' Calculate Sun Declination
#' @description Calculate the declination of the sun.
#' @param t Number of Julian centuries since J2000.0.
#' @return numeric (sun's declination in degrees)
#' @family Solar system
#' @export
calcSunDeclination <- function(t) {
  ve = calcObliquityCorrection(t)
  vlambda = calcSunApparentLong(t)

  vsint = sin(degToRad(ve)) * sin(degToRad(vlambda))
  vtheta = radToDeg(asin(vsint))

  return(vtheta)
}
