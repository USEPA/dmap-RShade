# hs7Solar(theTime, Hour_DST, theElevation, SolarAltitude, Jsnt, JsntTOA)
# 'HeatSource version 7 solar flux calculation before shade from topo and veg

#' HS 7 solar
#' @description HeatSource version 7 solar flux calculation before shade from topo and veg.
#' @param theTime Time.
#' @param Hour_DST Hour.
#' @param theElevation Elevation.
#' @param SolarAltitude Solar altitude.
#' @return list
#' @export
hs7Solar <- function(theTime, Hour_DST, theElevation, SolarAltitude) {
  # set Pi constant value
  Pival <- 3.14159265358979

  # **************************
  # '0 - Edge of atmosphere
  # **************************
  # get the day of year using lubridate::yday()
  JulianDay = lubridate::yday(theTime)-1
  Rad_Vec = 1 + 0.017 * cos((2 * Pival / 365) * (186 - JulianDay + Hour_DST / 24))
  Solar_Constant = 1367 # W/m2

  # Global Direct Solar Radiation
  FLUX_Direct = (Solar_Constant / Rad_Vec^2) * sin(SolarAltitude * Pival / 180)
  FLUX_Diffuse = 0

  # *******************************************
  # '1 - Above Topography
  # *******************************************
  Dummy1 = 35 / sqrt(1224 * sin(SolarAltitude * Pival / 180) + 1)
  Air_Mass = Dummy1 * exp(-0.0001184 * theElevation)
  Trans_Air = 0.0685 * cos((2 * Pival / 365) * (JulianDay + 10)) + 0.8

  # 'Calculate Diffuse Fraction
  FLUX_Direct[2] = FLUX_Direct[1] * (Trans_Air^Air_Mass)      # moved cloud correction to main sub calc
  if(FLUX_Direct[1] == 0) {
    Clearness_Index = 1
  } else {
    Clearness_Index = FLUX_Direct[2] / FLUX_Direct[1]
  }
  Dummy = FLUX_Direct[2]
  Dummy1 = 0.938 + 1.071 * Clearness_Index
  Dummy2 = 5.14 * Clearness_Index^2
  Dummy3 = 2.98 * Clearness_Index^3
  Dummy4 = sin(2 * Pival * (JulianDay - 40) / 365)
  Dummy5 = (0.009 - 0.078 * Clearness_Index)
  Diffuse_Fraction = Dummy1 - Dummy2 + Dummy3 - Dummy4 * Dummy5
  FLUX_Direct[2] = Dummy * (1 - Diffuse_Fraction)
  FLUX_Diffuse[2] = Dummy * (Diffuse_Fraction)      # moved cloud correction to main sub calc

  # extraterrestrial in cal/cm^2/day
  JsntTOA = FLUX_Direct[1] / (4.184 * 100 * 100 / 86400)

  # cal/cm^2/day
  Jsnt = (FLUX_Direct[2] + FLUX_Diffuse[2])/ (4.184 * 100 * 100 / 86400)

  return(list(Jsnt=Jsnt,JsntTOA=JsntTOA))
}
