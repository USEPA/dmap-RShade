# Function solarnoon(lat, lon, year, month, day, timezone, dlstime)

# '***********************************************************************/
# '* Name:    solarnoon
# '* Type:    Main Function called by spreadsheet
# '* Purpose: calculate the Universal Coordinated Time (UTC) of solar
# '*     noon for the given day at the given location on earth
# '* Arguments:
#   '    year
# '    month
# '    day
# '*   longitude : longitude of observer in degrees
# '    NOTE: longitude is negative for western hemisphere for input cells
# '          in the spreadsheet for calls to the functions named
# '          sunrise, solarnoon, and sunset. Those functions convert the
# '          longitude to positive for the western hemisphere for calls to
# '          other functions using the original sign convention
# '          from the NOAA javascript code.
# '* Return value:
# '*   time of solar noon in local time days
# '***********************************************************************/

#' Solar noon
#' @description Calculate the Universal Coordinated Time (UTC) of solar
#' noon for the given day at the given location on Earth.
#' @param lat Latitude.
#' @param lon Longitude.
#' @param year Year.
#' @param month Month.
#' @param day Day.
#' @param timezone Time zone.
#' @param dlstime Daylight saving time,
#' @details  Main Function called by spreadsheet.
#' @return numeric (time of solar noon in local time days)
#' @family Calendar
#' @export
solarnoon <- function(lat, lon, year, month, day, timezone, dlstime) {
  # ' change sign convention for longitude from negative to positive in western hemisphere
  longitude = lon * -1
  Latitude = lat
  if(Latitude > 89.8) {Latitude = 89.8}
  if(Latitude < -89.8) {Latitude = -89.8}

  jd = calcJD(year, month, day)
  t = calcTimeJulianCent(jd)

  newt = calcTimeJulianCent(calcJDFromJulianCent(t) + 0.5 + longitude / 360)

  eqtime = calcEquationOfTime(newt)
  solarNoonDec = calcSunDeclination(newt)
  solNoonUTC = 720 + (longitude * 4) - eqtime

  # adjust for time zone and daylight savings time in minutes
  solarnoon = solNoonUTC + (60 * timezone) + (dlstime * 60)

  # convert to days and return the value
  return(solarnoon / 1440)
}
