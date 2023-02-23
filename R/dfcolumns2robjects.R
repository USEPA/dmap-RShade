# function to pull site data from data frame and load them as R objects

#' Data frame columns to R-objects
#' @description Function to pull site data from data frame and load them as R objects.
#' @param data_frame_sub Data frame
#' @param ... Not used
#' @return environment
#' @family Data wrangling
#' @export
dfcolumns2robjects <- function(data_frame_sub = site_dat, ...) {
  ls1 <- lapply(colnames(data_frame_sub),
         function(x){x <- data_frame_sub[1, x]})
  names(ls1) <- colnames(data_frame_sub)
  list2env(ls1, globalenv())
}

