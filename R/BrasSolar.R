# BrasSolar Function
# Called from Rshadexls script to get the Bras Solar equations calculations

# 'Bras method for atmpospheric attenuation of solar radiation from a clear sky
#
# 'inputs:
# 'jd = julian day (Jan 1=1, etc.)
# 'year = current year
# 'dayfrac = current time of day as a fraction of the day (0-1)
# 'el = solar elevation (deg from horizon)
# 'R = distance from earth to sun in AU
# 'nfac = atmospheric turbidity paramter (2=clear, 4-5=smoggy")
#
# 'output:
# 'Iclear = clear-sky solar radiation at input solar elevation (W/m^2)

#' Bras solar
#' @description Bras method for atmospheric attenuation of solar radiation from a clear sky.
#' @param el Solar elevation (deg from horizon).
#' @param R Distance from earth to sun in AU.
#' @param nfac Atmospheric turbidity parameter (2=clear, 4-5=smoggy").
#' @details Called from Rshadexls script to get the Bras Solar equations calculations.
#' @return list
#' @family Earth
#' @export
BrasSolar <- function(el, R, nfac) {
  # NREL solar constant (W/m^2)
  W0 = 1367

  # solar radiation on horizontal surface at top of atmosphere (Bras eqn 2.9)
  I0 = (W0 / R ^ 2) * sin(degToRad(el))

  # optical air mass (Bras eqn 2.22)
  m = (sin(degToRad(el)) + 0.15 * (el + 3.885) ^ -1.253) ^ -1  # Bras eqn 2.22

  # molecular scattering coeff (Bras eqn 2.26)
  a1 = 0.128 - 0.054 * log(m) / log(10)

  # clear-sky solar radiation at earth surface on horizontal surface (W/m^2) (Bras eqn 2.25)
  Iclear = I0 * exp(-nfac * a1 * m)

  return(list(Iclear=Iclear,I0=I0))
}
