
#' Sky Opening function
#' @param veg_elev_matrix Matrix rows = 2 1-left, 2-right; cols = # vegzones;
#' values = ground elevation for each zone.
#' @param transverse_matrix Matrix rows = 2 1-left, 2-right; cols = # vegzones;
#' values = distance from center of stream to the edge of each zone.
#' @return numeric (SKOP value)
#' @export
SKOP_fnx <- function(veg_elev_matrix, transverse_matrix){
  # check to make sure the matrices are identical
  if(!identical(dim(veg_elev_matrix),dim(transverse_matrix))) {
    print("Input SKOP_fnx() matrices are not same dimensions. Check inputs and rerun.")
  }

  VTS <- 90 - (180 / 3.14159265359) * atan(veg_elev_matrix/transverse_matrix)
  # VTS[transverse_matrix <= 0] <- 90

  return(sum(apply(VTS,1,min)) / 180)
}
