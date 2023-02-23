# Function calcSunriseUTC(jd, Latitude, longitude)

# '***********************************************************************/
# '* Name:    calcSunriseUTC
# '* Type:    Function
# '* Purpose: calculate the Universal Coordinated Time (UTC) of sunrise
# '*         for the given day at the given location on earth
# '* Arguments:
#   '*   JD  : julian day
# '*   latitude : latitude of observer in degrees
# '*   longitude : longitude of observer in degrees
# '* Return value:
#   '*   time in minutes from zero Z
# '***********************************************************************/

#' Calculate the Sunrise UTC
#' @description Calculates the Universal Coordinated Time (UTC) of sunrise
#' for the given day at the given location on earth
#' @param jd Julian day.
#' @param Latitude Latitude of observer in degrees.
#' @param longitude Longitude of observer in degrees.
#' @return numeric (time in minutes from zero Z)
#' @family Earth, Calendar
#' @export
calcSunriseUTC <- function(jd, Latitude, longitude) {
  t = calcTimeJulianCent(jd)

  # *** First pass to approximate sunrise

  eqtime = calcEquationOfTime(t)
  SolarDec = calcSunDeclination(t)
  hourangle = calcHourAngleSunrise(Latitude, SolarDec)

  delta = longitude - radToDeg(hourangle)
  timeDiff = 4 * delta  # in minutes of time
  timeUTC = 720 + timeDiff - eqtime  # in minutes

  # *** Second pass includes fractional jday in gamma calc
  newt = calcTimeJulianCent(calcJDFromJulianCent(t) + timeUTC / 1440)
  eqtime = calcEquationOfTime(newt)
  SolarDec = calcSunDeclination(newt)
  hourangle = calcHourAngleSunrise(Latitude, SolarDec)
  delta = longitude - radToDeg(hourangle)
  timeDiff = 4 * delta
  timeUTC = 720 + timeDiff - eqtime # in minutes

  return(timeUTC)
}
