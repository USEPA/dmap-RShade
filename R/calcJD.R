# calcJD function


# ***********************************************************************
#  Name:    calcJD
#  Type:    Function
#  Purpose: Julian day from calendar day
#  Arguments:
#    year : 4 digit year
#    month: January = 1
#    day  : 1 - 31
#  Return value:
#    The Julian day corresponding to the date
#  Note:
#    Number is returned for start of day.  Fractional days should be
#    added later.
# ***********************************************************************

#' Julian day from calendar day
#' @description Julian day from calendar day.
#' @param date_year Numeric (4 digit year)
#' @param date_month Numeric (January = 1)
#' @param date_day Numeric (1 - 31)
#' @details Number is returned for start of day. Fractional days should be
#  added later.
#' @return numeric (the Julian day corresponding to the date)
#' @export
#'
#' @family Calendar
calcJD <- function(date_year, date_month, date_day){

  if (date_month <= 2) {
    date_year = date_year - 1
    date_month = date_month + 12
  }

  A = floor(date_year / 100)
  B = 2 - A + floor(A / 4)

  jd = floor(365.25 * (date_year + 4716)) + floor(30.6001 * (date_month + 1)) + date_day + B - 1524.5

  return(jd)
}
