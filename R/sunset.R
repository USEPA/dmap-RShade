# Function sunset(lat, lon, year, month, day, timezone, dlstime)

# '***********************************************************************/
# '* Name:    sunset
# '* Type:    Main Function called by spreadsheet
# '* Purpose: calculate time of sunrise and sunset for the entered date
# '*     and location.
# '* For latitudes greater than 72 degrees N and S, calculations are
# '* accurate to within 10 minutes. For latitudes less than +/- 72?
# '* accuracy is approximately one minute.
# '* Arguments:
# '   latitude = latitude (decimal degrees)
# '   longitude = longitude (decimal degrees)
# '    NOTE: longitude is negative for western hemisphere for input cells
# '          in the spreadsheet for calls to the functions named
# '          sunrise, solarnoon, and sunset. Those functions convert the
# '          longitude to positive for the western hemisphere for calls to
# '          other functions using the original sign convention
# '          from the NOAA javascript code.
# '   year = year
# '   month = month
# '   day = day
# '   timezone = time zone hours relative to GMT/UTC (hours)
# '   dlstime = daylight savings time (0 = no, 1 = yes) (hours)
# '* Return value:
# '*   sunset time in local time (days)
# '***********************************************************************/

#' Sunset
#' @description Calculate time of sunrise for the entered date and location.
#' Main Function called by spreadsheet.
#' @details
#' For latitudes greater than 72 degrees N and S, calculations are
#' accurate to within 10 minutes. For latitudes less than +/- 72?
#' accuracy is approximately one minute.
#' (The argument) longitude is negative for western hemisphere for input cells
#' in the spreadsheet for calls to the functions named
#' sunrise, solarnoon, and sunset. Those functions convert the
#' longitude to positive for the western hemisphere for calls to
#' other functions using the original sign convention
#' from the NOAA javascript code.
#' @param lat Latitude (decimal degrees).
#' @param lon Longitude (decimal degrees).
#' @param year Year.
#' @param month Month
#' @param day Day.
#' @param timezone Time zone hours relative to GMT/UTC (hours).
#' @param dlstime Daylight savings time (0 = no, 1 = yes) (hours).
#' @return numeric (sunset time in local time (days))
#' @family Earth, Calendar
#' @export
sunset <- function(lat, lon, year, month, day, timezone, dlstime) {

  # ' change sign convention for longitude from negative to positive in western hemisphere
  longitude = lon * -1
  Latitude = lat
  if(Latitude > 89.8) {Latitude = 89.8}
  if(Latitude < -89.8) {Latitude = -89.8}

  jd = calcJD(year, month, day)

  # Calculate sunset for this date
  setTimeGMT = calcSunsetUTC(jd, Latitude, longitude)

  # adjust for time zone and daylight savings time in minutes
  setTimeLST = setTimeGMT + (60 * timezone) + (dlstime * 60)

  # convert to days and return value
  return(setTimeLST / 1440)
}
