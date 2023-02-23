# Function calcHourAngleSunset(lat, SolarDec)

# '***********************************************************************/
# '* Name:    calcHourAngleSunset
# '* Type:    Function
# '* Purpose: calculate the hour angle of the sun at sunset for the
# '*         latitude
# '* Arguments:
#   '*   lat : latitude of observer in degrees
# '* solarDec : declination angle of sun in degrees
# '* Return value:
# '*   hour angle of sunset in radians
# '***********************************************************************/

#' Calculate the Hour Angle of Sunset
#' @description Calculates the hour angle of the sun at sunset for the latitude.
#' @param lat Latitude of observer in degrees.
#' @param SolarDec Declination angle of sun in degrees
#' @return numeric (hour angle of sunset in radians)
#' @family Earth
#' @export
calcHourAngleSunset <- function(lat, SolarDec) {
  latRad = degToRad(lat)
  sdRad = degToRad(SolarDec)

  HAarg = (cos(degToRad(90.833)) / (cos(latRad) * cos(sdRad)) - tan(latRad) * tan(sdRad))

  HA = (acos(cos(degToRad(90.833)) / (cos(latRad) * cos(sdRad)) - tan(latRad) * tan(sdRad)))

  return(-HA)
}
