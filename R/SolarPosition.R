# SolarPosition Function

# '***********************************************************************/
# '* Name:    SolarPosition
# '* Type:    Main Function
# '* Purpose: calculate solar azimuth (deg from north) for the entered
# '*          date, time and location. Returns -999999 if darker than twilight
# '*
#   '* Arguments:
# '*   latitude, longitude, year, month, day, hour, minute, second,
# '*   timezone, daylightsavingstime
# '* Return value:
#   '*   solar azimuth in degrees from north
# '*
#   '* Note: solarelevation and solarazimuth functions are identical
# '*       and could converted to a VBA subroutine that would return
# '*       both values.
# '*
#   '***********************************************************************/

#' Solar Position
#' @details solarelevation and solarazimuth functions are identical
#' and could converted to a VBA subroutine that would return
#' both values.
#' @param lat Latitude.
#' @param lon Longitude.
#' @param date_year Year.
#' @param date_month Month.
#' @param date_day Day.
#' @param thours Hours.
#' @param tminutes Minutes.
#' @param tseconds Seconds.
#' @param timezone Time zone.
#' @param dlstime Daylight saving time.
#' @return numeric (solar azimuth in degrees from north)
#' @family Earth
#' @export
SolarPosition <- function(lat, lon, date_year, date_month, date_day, thours, tminutes, tseconds,
                          timezone, dlstime) {
  # change sign convention for longitude from negative to positive in western hemisphere
  longitude = lon * -1
  Latitude = lat
  if(Latitude > 89.8) {Latitude = 89.8}
  if(Latitude < -89.8) {Latitude = -89.8}

  # change time zone to positive hours in western hemisphere
  Zone = timezone * -1
  daySavings = dlstime * 60
  hh = thours - (daySavings / 60)
  mm = tminutes
  ss = tseconds

  # timenow is GMT time for calculation in hours since 0Z
  timenow = hh + mm / 60 + ss / 3600 + Zone

  jd = calcJD(date_year, date_month, date_day)
  t = calcTimeJulianCent(jd + timenow / 24)
  R = calcSunRadVector(t)
  alpha = calcSunRtAscension(t)
  theta = calcSunDeclination(t)
  Etime = calcEquationOfTime(t)
  earthRadVec = R
  eqtime = Etime
  SolarDec = theta # in degrees


  solarTimeFix = eqtime - 4 * longitude + 60 * Zone
  trueSolarTime = hh * 60 + mm + ss / 60 + solarTimeFix  # in minutes

  trueSolarTime = trueSolarTime - 1440
  while(trueSolarTime > 1440) {trueSolarTime = trueSolarTime - 1440}

  hourangle = trueSolarTime / 4 - 180
  if(hourangle < -180) {hourangle = hourangle + 360}

  harad = degToRad(hourangle)

  csz <- sin(degToRad(Latitude)) * sin(degToRad(SolarDec)) +
    cos(degToRad(Latitude)) * cos(degToRad(SolarDec)) * cos(harad)

  if(csz > 1) {
    csz = 1
  } else if(csz < -1) {
    csz = -1
  }

  zenith = radToDeg(acos(csz))

  azDenom = (cos(degToRad(Latitude)) * sin(degToRad(zenith)))

  if(abs(azDenom) > 0.001) {
    azRad = ((sin(degToRad(Latitude)) * cos(degToRad(zenith))) -
               sin(degToRad(SolarDec))) / azDenom

    if(abs(azRad) > 1) {
      if(azRad < 0) {
        azRad = -1
      } else {
        azRad = 1
      }
    }

    azimuth = 180 - radToDeg(acos(azRad))

    if(hourangle > 0) {
      azimuth = -azimuth
    }

  } else {
    if(Latitude > 0) {
      azimuth = 180
    } else {
      azimuth = 0
    }
  }

  if(azimuth < 0) {azimuth = azimuth + 360}

  exoatmElevation = 90 - zenith


  # beginning of simplified expression
  if(exoatmElevation > 85) {
    refractionCorrection = 0
  } else {
    te = tan(degToRad(exoatmElevation))

    if(exoatmElevation > 5) {
      refractionCorrection = 58.1 / te - 0.07 / (te * te * te) +
        0.000086 / (te * te * te * te * te)
    } else if(exoatmElevation > -0.575) {
      step1 = (-12.79 + exoatmElevation * 0.711)
      step2 = (103.4 + exoatmElevation * (step1))
      step3 = (-518.2 + exoatmElevation * (step2))
      refractionCorrection = 1735 + exoatmElevation * (step3)
    } else {
      refractionCorrection = -20.774 / te
    }
    refractionCorrection = refractionCorrection / 3600

  }  # end of simplified expression

  solarzen = zenith - refractionCorrection

  solarazimuth = azimuth
  solarelevation = 90 - solarzen

  return(list(solarazimuth=solarazimuth,
              solarelevation=solarelevation,
              earthRadVec = earthRadVec))
}
