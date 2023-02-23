
#' Elevation of vegetation in each zone
#' @param lbk_grdelev A vector holding vegetation zone ground elevation for left bank zones.
#' @param rbk_grdelev A vector holding vegetation zone ground elevation for right bank zones.
#' @param lbk_ripHght A vector holding vegetation zone vegetation height for left bank zones.
#' @param rbk_ripHght A vector holding vegetation zone vegetation height for left bank zones.
#' @param vegzones A numerical value holding the number of vegetation zones for the site.
#' @param elevation A numerical value holding the elevation of the site.
#' @return matrix holding left/right bank transverse distances to veg zone edges
#' @family Data wranling
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
veg_elev_matrix_fnx <- function(lbk_grdelev, rbk_grdelev,
                                lbk_ripHght, rbk_ripHght,
                                vegzones, elevation) {
  # empty matrix to populate
  veg_elev <- matrix(data = 0, nrow = 2, ncol = vegzones+1)

  # zone zero left/right bank both the same (= incision)
  veg_elev[ ,1] <- incision

  if(vegzones == 1) {
    veg_elev[1, 2] <- lbk_grdelev - elevation + lbk_ripHght
    veg_elev[2, 2] <- rbk_grdelev - elevation + rbk_ripHght
  } else {
    veg_elev[1, 2:(vegzones+1)] <- unlist(lbk_grdelev - elevation + lbk_ripHght)
    veg_elev[2, 2:(vegzones+1)] <- unlist(rbk_grdelev - elevation + rbk_ripHght)

  }

  return(veg_elev)
}
