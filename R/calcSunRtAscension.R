# Function calcSunRtAscension(t)

# '***********************************************************************/
# '* Name:    calcSunRtAscension (not used by sunrise, solarnoon, sunset)
# '* Type:    Function
# '* Purpose: calculate the right ascension of the sun
# '* Arguments:
# '*   t : number of Julian centuries since J2000.0
# '* Return value:
# '*   sun's right ascension in degrees
# '***********************************************************************/

#' Calculate Sun Right Ascension
#' @description Calculate the right ascension of the sun.
#' @details (Not used by sunrise, solar noon, sunset.)
#' @param t Number of Julian centuries since J2000.0
#' @return numeric (Sun's right ascension in degrees)
#' @family Earth
#' @export
calcSunRtAscension <- function(t) {
  ve = calcObliquityCorrection(t)
  vlambda = calcSunApparentLong(t)

  vtananum = (cos(degToRad(ve)) * sin(degToRad(vlambda)))
  vtanadenom = (cos(degToRad(vlambda)))

  valpha = radToDeg(atan2(vtanadenom, vtananum))

  return(valpha)
}
