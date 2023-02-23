# Function calcMeanObliquityOfEcliptic(t)

# '***********************************************************************/
# '* Name:    calcMeanObliquityOfEcliptic
# '* Type:    Function
# '* Purpose: calculate the mean obliquity of the ecliptic
# '* Arguments:
# '*   t : number of Julian centuries since J2000.0
# '* Return value:
# '*   mean obliquity in degrees
# '***********************************************************************/

#' Calculate mean obliquity of ecliptic
#' @description Calculate the mean obliquity of the ecliptic.
#' @param t Number of Julian centuries since J2000.0.
#' @return numeric (mean obliquity in degrees)
#' @family Solar system
#' @export
calcMeanObliquityOfEcliptic <- function(t) {
  seconds = 21.448 - t * (46.815 + t * (0.00059 - t * (0.001813)))
  e0 = 23 + (26 + (seconds / 60)) / 60

  return(e0)
}
