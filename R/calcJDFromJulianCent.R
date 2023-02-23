# Function calcJDFromJulianCent(t)

# '***********************************************************************/
# '* Name:    calcJDFromJulianCent
# '* Type:    Function
# '* Purpose: convert centuries since J2000.0 to Julian Day.
# '* Arguments:
# '*   t : number of Julian centuries since J2000.0
# '* Return value:
# '*   the Julian Day corresponding to the t value
# '***********************************************************************/

#' Calculate Julian Day From Julian Century
#' @description Convert centuries since J2000.0 to Julian Day.
#' @param t Number of Julian centuries since J2000.0.
#' @return numeirc (the Julian Day corresponding to the t value)
#' @export
#' @family Calendar
calcJDFromJulianCent <- function(t) {
  jd = t * 36525 + 2451545

  return(jd)
}


