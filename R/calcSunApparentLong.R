# Function calcSunApparentLong(t)

# '***********************************************************************/
# '* Name:    calcSunApparentLong (not used by sunrise, solarnoon, sunset)
# '* Type:    Function
# '* Purpose: calculate the apparent longitude of the sun
# '* Arguments:
# '*   t : number of Julian centuries since J2000.0
# '* Return value:
# '*   sun's apparent longitude in degrees
# '***********************************************************************/

#' Calculate Sun Apparent Longitude
#' @description Calculate the apparent longitude of the sun.
#' @param t Number of Julian centuries since J2000.0.
#' @details Not used by sunrise, solar noon, sunset.
#' @return numeric (sun's apparent longitude in degrees)
#' @family Earth
#' @export
calcSunApparentLong <- function(t) {
  tlsun = calcSunTrueLong(t)

  vomega = 125.04 - 1934.136 * t
  vlambda = tlsun - 0.00569 - 0.00478 * sin(degToRad(vomega))

  return(vlambda)
}
