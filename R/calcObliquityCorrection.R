# Function calcObliquityCorrection(t)

# '***********************************************************************/
# '* Name:    calcObliquityCorrection
# '* Type:    Function
# '* Purpose: calculate the corrected obliquity of the ecliptic
# '* Arguments:
# '*   t : number of Julian centuries since J2000.0
# '* Return value:
# '*   corrected obliquity in degrees
# '***********************************************************************/

#' Calculate Obliquity Correction
#' @description Calculate the corrected obliquity of the ecliptic.
#' @param t Number of Julian centuries since J2000.0.
#' @return numeric (corrected obliquity in degrees)
#' @family Solar system
#' @export
calcObliquityCorrection <- function(t) {
  e0 = calcMeanObliquityOfEcliptic(t)

  vomega = 125.04 - 1934.136 * t
  ve = e0 + 0.00256 * cos(degToRad(vomega))

  return(ve)
}
