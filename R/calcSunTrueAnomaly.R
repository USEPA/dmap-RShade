# Function calcSunTrueAnomaly(t)

# '***********************************************************************/
# '* Name:    calcSunTrueAnomaly (not used by sunrise, solarnoon, sunset)
# '* Type:    Function
# '* Purpose: calculate the true anamoly of the sun
# '* Arguments:
# '*   t : number of Julian centuries since J2000.0
# '* Return value:
# '*   sun's true anamoly in degrees
# '***********************************************************************/

#' Calculate Sun True Anomaly
#' @description Calculate the true anomaly of the Sun.
#' @param t Number of Julian centuries since J2000.0.
#' @return numeric (Sun's true anomaly in degrees)
#' @family Solar system
#' @export
calcSunTrueAnomaly <- function(t) {
  mas = calcGeomMeanAnomalySun(t)
  suncen = calcSunEqOfCenter(t)

  val = mas + suncen

  return(val)
}
