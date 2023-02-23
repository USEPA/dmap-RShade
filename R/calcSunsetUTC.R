# Function calcSunsetUTC(jd, Latitude, longitude)

# '***********************************************************************/
# '* Name:    calcSunsetUTC
# '* Type:    Function
# '* Purpose: calculate the Universal Coordinated Time (UTC) of sunset
# '*         for the given day at the given location on earth
# '* Arguments:
#   '*   JD  : julian day
# '*   latitude : latitude of observer in degrees
# '*   longitude : longitude of observer in degrees
# '* Return value:
#   '*   time in minutes from zero Z
# '***********************************************************************/

#' Calculate Sunset UTC
#' @description Calculate the Universal Coordinated Time (UTC) of sunset
#' for the given day at the given location on Earth.
#' @param jd Julian day.
#' @param Latitude Latitude of observer in degrees.
#' @param longitude Longitude of observer in degrees.
#' @return numeric (time in minutes from zero Z)
#' @family Earth, Calendar
#' @export
calcSunsetUTC <- function(jd, Latitude, longitude) {
  t = calcTimeJulianCent(jd)

  # First calculates sunrise and approx length of day

  eqtime = calcEquationOfTime(t)
  SolarDec = calcSunDeclination(t)
  hourangle = calcHourAngleSunset(Latitude, SolarDec)

  delta = longitude - radToDeg(hourangle)
  timeDiff = 4 * delta
  timeUTC = 720 + timeDiff - eqtime

  # first pass used to include fractional day in gamma calc

  newt = calcTimeJulianCent(calcJDFromJulianCent(t) + timeUTC / 1440)
  eqtime = calcEquationOfTime(newt)
  SolarDec = calcSunDeclination(newt)
  hourangle = calcHourAngleSunset(Latitude, SolarDec)

  delta = longitude - radToDeg(hourangle)
  timeDiff = 4 * delta
  timeUTC = 720 + timeDiff - eqtime  # in minutes

  return(timeUTC)
}
