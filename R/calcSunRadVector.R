# Function calcSunRadVector(t)

# '***********************************************************************/
# '* Name:    calcSunRadVector (not used by sunrise, solarnoon, sunset)
# '* Type:    Function
# '* Purpose: calculate the distance to the sun in AU
# '* Arguments:
# '*   t : number of Julian centuries since J2000.0
# '* Return value:
# '*   sun radius vector in AUs
# '***********************************************************************/

#' Calculate Sun Radius Vector
#' @description Calculate the distance to the sun in AU.
#' @param t Number of Julian centuries since J2000.0.
#' @details (Not used by sunrise, solar noon, sunset.)
#' @return numeric (Sun radius vector in AUs)
#' @family Solar system
#' @export
calcSunRadVector <- function(t) {
  val = calcSunTrueAnomaly(t)
  exc = calcEccentricityEarthOrbit(t)

  Radvec = (1.000001018 * (1 - exc * exc)) / (1 + exc * cos(degToRad(val)))

  return(Radvec)
}
