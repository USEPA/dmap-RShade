#Function calcEccentricityEarthOrbit(t)

# **********************************************************************/
#  Name:    calcEccentricityEarthOrbit
#  Type:    Function
#  Purpose: calculate the eccentricity of earth's orbit
#  Arguments:
#    t : number of Julian centuries since J2000.0
#  Return value:
#    the unitless eccentricity
# **********************************************************************/


#' Calculate Eccentricity of Earth Orbit
#' @description Calculates the eccentricity of earth's orbit.
#' @param t Number of Julian centuries since J2000.0.
#'
#' @return number (unit-less eccentricity)
#' @family Solar system
#' @export
calcEccentricityEarthOrbit <- function(t) {

  exc = 0.016708634 - t * (0.000042037 + 0.0000001267 * t)

  return(exc)

}
