% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sunrise.R
\name{sunrise}
\alias{sunrise}
\title{Sunrise}
\usage{
sunrise(lat, lon, year, month, day, timezone, dlstime)
}
\arguments{
\item{lat}{Latitude (decimal degrees).}

\item{lon}{Longitude (decimal degrees).}

\item{year}{Year.}

\item{month}{Month}

\item{day}{Day.}

\item{timezone}{Time zone hours relative to GMT/UTC (hours).}

\item{dlstime}{Daylight savings time (0 = no, 1 = yes) (hours).}
}
\value{
numeric ( sunrise time in local time (days))
}
\description{
Calculate time of sunrise for the entered date and location.
Main Function called by spreadsheet.
}
\details{
For latitudes greater than 72 degrees N and S, calculations are
accurate to within 10 minutes. For latitudes less than +/- 72?
accuracy is approximately one minute.
(The argument) longitude is negative for western hemisphere for input cells
in the spreadsheet for calls to the functions named
sunrise, solarnoon, and sunset. Those functions convert the
longitude to positive for the western hemisphere for calls to
other functions using the original sign convention
from the NOAA javascript code.
}
\seealso{
Other Earth, Calendar: 
\code{\link{calcSunriseUTC}()},
\code{\link{calcSunsetUTC}()},
\code{\link{sunset}()}
}
\concept{Earth, Calendar}
