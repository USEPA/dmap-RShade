# Function calcSunTrueLong(t)

# '***********************************************************************/
# '* Name:    calcSunTrueLong
# '* Type:    Function
# '* Purpose: calculate the true longitude of the sun
# '* Arguments:
# '*   t : number of Julian centuries since J2000.0
# '* Return value:
# '*   sun's true longitude in degrees
# '***********************************************************************/

#' Calculate Sun True Longitude
#' @description Calculate the true longitude of the Sun.
#' @param t Number of Julian centuries since J2000.0.
#' @return numeric (Sun's true longitude in degrees)
#' @family Earth
#' @export
calcSunTrueLong <- function(t) {
  l0 = calcGeomMeanLongSun(t)
  c = calcSunEqOfCenter(t)

  tlsun = l0 + c

  return(tlsun)
}
