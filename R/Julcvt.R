# Function Julcvt(mo, da, yr)

#' Julcvt
#' @description Calculates the day of the year.
#' @param mo Month.
#' @param da Day.
#' @param yr Year.
#' @return
#' @family Calendar
#' @export
Julcvt <- function(mo, da, yr) {

  # use lubridate::yday() function to get day of year integer
  Julcvt <- lubridate::yday(paste(yr,mo,da,sep = "-"))

  return(Julcvt)
}

# original calculation in shade.xls VBA code used something similar to below:
# A leap year is exactly divisible by 4 except for century years (years
# ending with 00). The century year is a leap year only if it is perfectly
# divisible by 400. https://www.datamentor.io/r-programming/examples/leap-year/
# if((yr %% 4) == 0) {
#   if((yr %% 100) == 0) {
#     if((yr %% 400) == 0) {
#       leap = 1
#     } else {
#       leap = 0
#     }
#   } else {
#     leap = 1
#   }
# } else {
#   leap = 0
# }
#
# if(mo == 1) {
#   Julcvt = 0
# } else if(mo == 2) {
#   Julcvt = 31
# } else if(mo == 3) {
#   Julcvt = 59 + leap
# } else if(mo == 4) {
#   Julcvt = 90 + leap
# } else if(mo == 5) {
#   Julcvt = 120 + leap
# } else if(mo == 6) {
#   Julcvt = 151 + leap
# } else if(mo == 7) {
#   Julcvt = 181 + leap
# } else if(mo == 8) {
#   Julcvt = 212 + leap
# } else if(mo == 9) {
#   Julcvt = 243 + leap
# } else if(mo == 10) {
#   Julcvt = 273 + leap
# } else if(mo == 11) {
#   Julcvt = 304 + leap
# } else if(mo == 12) {
#   Julcvt = 334 + leap
# }
#
# Julcvt = Julcvt + da

