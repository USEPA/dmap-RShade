# calcTimeJulianCent Function



# **********************************************************************/
#  Name:    calcTimeJulianCent
#  Type:    Function
#  Purpose: convert Julian Day to centuries since J2000.0.
#  Arguments:
#    jd : the Julian Day to convert
#  Return value:
#    the T value corresponding to the Julian Day
# **********************************************************************/

#' Calculate Time Julian Centuries
#' @description Convert Julian Day to centuries since J2000.0.
#' @param jd Julian Day to convert
#' @return numeric (the T value corresponding to the Julian Day)
#' @family Calendar
#' @export
calcTimeJulianCent <- function(jd) {

  t = (jd - 2451545) / 36525

  return(t)

}
