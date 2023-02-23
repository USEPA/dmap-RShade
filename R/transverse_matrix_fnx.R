
#' Transverse distance to vegetation zone calculations
#' @param lbk_vwidth A vector holding vegetation zone widths for left bank zones.
#' @param rbk_vwidth A vector holding vegetation zone widths for right bank zones.
#' @return matrix holding left/right bank transverse distances to veg zone edges
#' @family Data wrangling
#' @export
transverse_matrix_fnx <- function(lbk_vwidth, rbk_vwidth, vegzones) {

  # left bank transverse distance calculations
  # set values for left bank zones 0 and 1
  lbk <- c(disfromcentertolb,disfromcentertolb)

  # right bank transverse distance calculations
  # set values for left bank zones 0 and 1
  rbk <- c((bfwidth - disfromcentertolb), (bfwidth - disfromcentertolb))


  # set values for zones 2-plus
  if(vegzones > 1) {
    lbk[3:(vegzones+1)] <- disfromcentertolb + cumsum(t(lbk_vwidth))
    rbk[3:(vegzones+1)] <- bfwidth - disfromcentertolb + cumsum(t(rbk_vwidth))
  }

  return(rbind(lbk,rbk))
}
