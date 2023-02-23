# RyanStolzSolar Function
#Ryan-Stolzenbach method for atmospheric attenuation of solar radiation from a clear sky

#input variables
#  jd      julian day (Jan 1=1 etc)
#  year = current year
#  dayfrac = current time of day as a fraction of the day (0-1)
#  el      solar elevation deg from horizon
#  R = distance from earth to sun in AU
#  atc     atmospheric transmission coefficient (0.70-0.91, default 0.8)
#  z       elevation, metres -- required if imthd=2

#output variables
#  I0      clear-sky solar radiation at top of atmosphere, W m-2
#  Iclear  clear-sky solar radiation, W m-2




#' Ryan-Stolzenbach method
#' @description Ryan-Stolzenbach method for atmospheric attenuation of solar radiation from a clear sky.
#' @param el Solar elevation deg from horizon.
#' @param R Distance from earth to sun in AU.
#' @param atc Atmospheric transmission coefficient (0.70-0.91, default 0.8)
#' @param z Elevation, metres -- required if imthd=2.
#' @param Iclear Clear-sky solar radiation, W m-2.
#' @param I0 Clear-sky solar radiation at top of atmosphere, W m-2.
#' @return list
#' @family Earth
#' @export
RyanStolzSolar <- function(el, R, atc, z, Iclear, I0) {
  pii <- 3.14159265358979

  # NREL solar constant, W m-2
  r0 = 1367

  # atmospheric transmission coefficient (0.70-0.91)
  # from ryan et al. MIT publication
  at = atc

  sinal = sin(degToRad(el))     # Sine of the solar elevation angle

  if(sinal < 0) {
    Iclear <- 0
    I0 <- 0

  } else {
    al = asin(sinal)
    a0 = radToDeg(al)  # convert the radians to degree

    rm = (((288 - 0.0065 * z) / 288) ^ 5.256) / (sinal + 0.15 * (a0 + 3.885) ^ (-1.253))

    I0 = r0 * sinal / (R^2)       # solar rad on the top of atmosphere W/m2
    Iclear = I0 * (at^rm)           # solar rad on the ground W/m2

  }  # end if-else statements

  return(list(Iclear=Iclear,I0=I0))

}  # end RyanStolzSolar Function


