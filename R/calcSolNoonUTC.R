# Function calcSolNoonUTC(t, longitude)

# '***********************************************************************/
# '* Name:    calcSolNoonUTC
# '* Type:    Function
# '* Purpose: calculate the Universal Coordinated Time (UTC) of solar
# '*     noon for the given day at the given location on earth
# '* Arguments:
#   '*   t : number of Julian centuries since J2000.0
# '*   longitude : longitude of observer in degrees
# '* Return value:
# '*   time in minutes from zero Z
# '***********************************************************************/

#' Calculate Solar Noon UTC
#' @description Calculate the Universal Coordinated Time (UTC) of solar
#' noon for the given day at the given location on earth.
#' @param t Number of Julian centuries since J2000.0
#' @param longitude Longitude of observer in degrees.
#' @family Earth
#' @return numeric (time in minutes from zero Z)
#' @export
calcSolNoonUTC <- function(t, longitude) {

  newt = calcTimeJulianCent(calcJDFromJulianCent(t) + 0.5 + longitude / 360)

  eqtime = calcEquationOfTime(newt)
  solarNoonDec = calcSunDeclination(newt)
  solNoonUTC = 720 + (longitude * 4) - eqtime

  return(solNoonUTC)

}
