# intputs_fixed function to load all fixed site input variables/values
# dependent on libraries 'magrittr', 'tidyverse', and 'chron'
# FNX INPUTS:
#    site_data_frame: holds all the site details necessary for the inputs_fixed()
#         fnx to process and develop the necessary objects for shade
#              [this data frame has the following fields:
#                   site_id: numerical vlaue to identify each site
#                   lat: latitude of site in decimal degrees
#                   lon: longitude of site in decimal degrees
#                   zone_elev[1:X]: reach-specific ground elevation for each
#                        riparian vegetation buffer zone at a site
#                   zone_wid[1:X]: reach-specific riparian zone widths for each
#                        riparian vegetation buffer zone at a site
#
#                   ]
#    startdate: start date for integrating shade as a text string "YYYY-MM-DD"
#    ndays: number of days to integrate/simulate shade starting on 'startdate'
#         For example, to simulate shade only on the start date enter 1.
#         To simulate shade for the entire month of August 2012 enter 'startdate'
#         "2012-08-01" and 'ndays' = 31.
#         nday of 1 corresponds to simulating just the 'startdate'
#    timezone: Time zone is the difference between UTC and the local standard
#         time in hours. A map of world time zones may be found at:
#         http://aa.usno.navy.mil/graphics/TimeZoneMap2001.pdf
#         PST is -8 for the Pacific Northwest USA
#    dlstime: is a daylight savings time indicator variable where you Select '1'
#         if you want times calculated and reported as local daylight savings
#         time or "0" to report times in local standard time.
#    VWidthGlobal: Each of the 9 riparian vegetation zones on each bank are
#         assumed to have the same width (meters) as specified here. set to NA
#         if specific zone widths are desired for the
#    JsntMethod: Select the solar model to generate input radiation.
#         options selected from: 0="Bras" 3="ODEQ" 4="Ryan-Stolz"
#    cloudfrac_global: global value for cloud fraction of total incoming energy
#         that will be applied to all times. If continuous hourly values are
#         entered in the site_data_frame, then they override this global value.
#         Value must be between 0 and 1
#    nfac: Atmospheric turbidity factor for calculation of solar radiation that
#         is used if the Bras option for the solar radiation model is selected.
#         [2 = clear, 5 = smog]
#    atc: Atmospheric transmission factor for calculation of solar radiation
#         used if the Ryan-Stolzenbach option for 'JsntMethod' is selected.

#
# FNX OUTPUTS:
#     fixed_df: data frame holding all the site variables for the shade models
#


#' @import magrittr
#' @import tidyverse
#' @import chron
NULL

#' Inputs for "fixed star"
#' @description Function to load all fixed site input variables/values.
#' @param site_data_frame Holds all the site details necessary for the inputs_fixed()
#'         fnx to process and develop the necessary objects for shade
#'              [this data frame has the following fields:
#'                   site_id: numerical vlaue to identify each site
#'                   lat: latitude of site in decimal degrees
#'                   lon: longitude of site in decimal degrees
#'                   zone_elev[1:X]: reach-specific ground elevation for each
#'                        riparian vegetation buffer zone at a site
#'                   zone_wid[1:X]: reach-specific riparian zone widths for each
#'                        riparian vegetation buffer zone at a site
#' @param site_id_chr site_id_chr
#' @param lat_chr Latitude.
#' @param lon_chr Longitude.
#' @param startdate Start date for integrating shade as a text string "YYYY-MM-DD"
#' @param ndays Number of days to integrate/simulate shade starting on 'startdate'
#' For example, to simulate shade only on the start date enter 1.
#' To simulate shade for the entire month of August 2012 enter 'startdate'
#' "2012-08-01" and 'ndays' = 31.
#' nday of 1 corresponds to simulating just the 'startdate'
#' @param timezone Time zone is the difference between UTC and the local standard
#' time in hours. A map of world time zones may be found at:
#' http://aa.usno.navy.mil/graphics/TimeZoneMap2001.pdf
#' PST is -8 for the Pacific Northwest USA.
#' @param dlstime This is a daylight savings time indicator variable where you Select '1'
#' if you want times calculated and reported as local daylight savings
#' time or "0" to report times in local standard time.
#' @param VWidthGlobalEach of the 9 riparian vegetation zones on each bank are
#' assumed to have the same width (meters) as specified here. set to NA
#' if specific zone widths are desired for the ...
#' @param JsntMethod Select the solar model to generate input radiation.
#' options selected from: 0="Bras" 3="ODEQ" 4="Ryan-Stolz".
#' @param cloudfrac_global Global value for cloud fraction of total incoming energy
#' that will be applied to all times. If continuous hourly values are
#' entered in the site_data_frame, then they override this global value.
#' Value must be between 0 and 1.
#' @param nfac Atmospheric turbidity factor for calculation of solar radiation that
#' is used if the Bras option for the solar radiation model is selected.
#' [2 = clear, 5 = smog].
#' @param atc Atmospheric transmission factor for calculation of solar radiation
#'         used if the Ryan-Stolzenbach option for 'JsntMethod' is selected.
#' @param vegzones vegzones
#' @param star_veg star_veg
#' @param ripgelev ripgelev
#'
#' @family Data ingestion
#' @return Data frame
#' @export
inputs_fixed_star <-
  function(site_data_frame = site_data, site_id_chr = "pid", lat_chr = "lat",
           lon_chr = "lon", startdate = "2001-08-01", ndays = 31,
           timezone = -8, dlstime = 1, VWidthGlobal = 30.48, JsntMethod = "Bras",
           cloudfrac_global=0, nfac = 2, atc = 0.8, vegzones = "9",
           star_veg = TRUE,
           ripgelev = FALSE) {
    # Load libraries for reading and manipulating input variables
    library(magrittr)
    library(tidyverse)
    library(chron)

    # *************************************************************s****************
    # *****************************************************************************
    #       1. Read in input data for the shade.xls model                      ----
    # *****************************************************************************
    # *****************************************************************************

    # read in site data for analysis
    # site_data <- read_csv(file = "../data/site_data.csv")
    # the sourced input df should already be loaded, but added here for additional
    # consistency or if needing to reload again
    # source shade functions by first gathering all thier file names
    helper_fnxs_ls <-
      list.files(path = "scripts/helperfnx/", pattern = ".R", full.names = TRUE)

    # run the list of helper fnxs through lapply() to read each one into the GlbEnv
    lapply(helper_fnxs_ls, source)

    # all the directions for the star direction sampling
    star_dir_vec <- c("NN","NE","EE","SE","SS","SW","WW","NW")

    # set number of rows for input data
    # i <- nrow(site_data_frame)

    # read in riparian code data frame
    #  column descriptions:
    #     code: provide three digit index for referring to riparian vegetation type
    #     source: is ECY which refers to the Washington State Ecology Department
    #     description: provides characteristics of riparian veg. as text
    #     height_m: the height of riparian veg. in meters
    #     dens_perc: the density of riparian veg. as a percent (range: 0-100)
    #     ovrhng_m: overhang distance (meters) of rip. veg. if in zone 1 along bank
    # riparian_code_ref_df <-
    #   read_csv(file = "data/riparian_codes.csv", col_types = "cccddd")
    # chose to ignore this data input version. It's easy to take codes and
    # distribute them out to a full-site data frame in R using joins.

    # Enter as text string "yyyy-mm-dd"
    startdate = lubridate::as_date(startdate)  # shade.xls Sheet Main Menu("d4")


    # Enter the number of days to simulate shade starting with the start date.
    # For example, to simulate shade only on the start date enter 1.
    # To simulate shade for the entire month of August 2012 enter a start date
    # of "2012-08-01" and number of days = 31, etc.
    # Day 1 in the output sheets corresponds to the start date labeled here
    # ndays = ndays        # shade.xls Sheet Main Menu("d5")

    # Time zone is the difference between UTC and the local standard time in
    # hours. A map of world time zones may be found at:
    # http://aa.usno.navy.mil/graphics/TimeZoneMap2001.pdf
    # PST = -8
    # timezone = timezone    # shade.xls Sheet Main Menu ("d6")

    # Select '1' if you want times calculated and reported as local daylight
    # savings time or "0" to report times in local standard time.
    # dlstime = dlstime        # shade.xls Sheet Main Menu("d7")

    # The sign convention for latitude is positive decimal degrees for the northern hemisphere.
    # shade.xls Sheet Main Menu("d8"): (+) Dec Deg

    #The sign convention for longitude is negative decimal degrees for the western hemisphere.
    # shade.xls Sheet Main Menu("d9"): (-) Dec Deg

    # elevation must be greater than zero meters
    site_data_frame[ , "elevation"][site_data_frame[ , "elevation"] < 0] <- 0


    # Each of the 9 riparian vegetation zones on each bank are assumed to have
    # the same width (meters) as specified here.
    # set to NA if specific zone widths are desired for the
    # VWidthGlobal = VWidthGlobal  # Main Menu("d10") 30.48m is 100ft


    # the following data frame provides set-width vegetation zones if reach-
    # specific data are unavailable. [shade.xls Sheet Main Menu AH9:AY9]
    # Left bank zones appended with "LZ#" and the right bank are "RZ#"
    # the default is to set the veg. zone width to the global width VWidthGlobal
    # if no global width is selected, then it's expected that these columns
    # are populated in the "site_data" data frame the user specifies
    if(VWidthGlobal > 0) {
      # loop through and add global width to each vegzone
      for(i in 1:vegzones) {
        lnames <- paste0("VWidthL",i)
        rnames <- paste0("VWidthR",i)
        site_data_frame[ ,lnames] <- VWidthGlobal
        site_data_frame[ ,rnames] <- VWidthGlobal
      }
    }


    # disfromcentertolb conditional statements from General_Inputs submodel
    site_data_frame[i,"disfromcentertolb"][site_data_frame[i,"disfromcentertolb"] == 0] <-
      (site_data_frame[i,"bfwidth"]/2)
    site_data_frame[i,"disfromcentertolb"][site_data_frame[i,"disfromcentertolb"] < (site_data_frame[i,"wwidth"]/2)] <-
      (site_data_frame[i,"wwidth"]/2)
    site_data_frame[i,"disfromcentertolb"][site_data_frame[i,"disfromcentertolb"] > (site_data_frame[i,"bfwidth"] - (site_data_frame[i,"wwidth"]/2))] <-
      (site_data_frame[i,"bfwidth"] - (site_data_frame[i,"wwidth"]/2))
    # [for-looped version of above vectorized conditional statements]
    # for (i in 1:nrow(site_data_frame)) {
    #   if (site_data_frame[i,"disfromcentertolb"] == 0) {
    #     site_data_frame[i,"disfromcentertolb"] <- (site_data_frame[i,"bfwidth"]/2)
    #   }
    #   if (site_data_frame[i,"disfromcentertolb"] < (site_data_frame[i,"wwidth"]/2)) {
    #     site_data_frame[i,"disfromcentertolb"] <- (site_data_frame[i,"wwidth"]/2)
    #   }
    #   if (site_data_frame[i,"disfromcentertolb"] >
    #       (site_data_frame[i,"bfwidth"] - (site_data_frame[i,"wwidth"]/2))) {
    #     site_data_frame[i,"disfromcentertolb"] <-
    #       (site_data_frame[i,"bfwidth"] - (site_data_frame[i,"wwidth"]/2))
    #   }
    # }



    # If veg codes are 'On', then the riparian codes entered in the 'Main Menu'
    # sheet will be used with the codes in the 'Riparian Codes' sheet to assign
    # vegetation overhang, height, and density. The optional direct input of
    # overhang, height, and density are not used if veg codes are 'On'.
    #
    # If veg codes are 'Off', then  the optional direct input of vegetation
    # overhang, height, and density on the 'Main Menu' sheet is used. The
    # riparian codes entered in the 'Main Menu' sheet and the 'Riparian Codes'
    # sheet are not used of veg codes are 'Off'.


    # If no ground elevations are given for each riparian zone then it is
    # assumed to be equal to water surface elevation (site elevation in meters)
    # plus incision for the site
    if (isFALSE(ripgelev)) {
      # default ground elevation values are incision plus channel elevation
      site_data_frame[ ,paste0(rep(star_dir_vec,each=vegzones),"grndelevVZ",1:vegzones)] <-
        site_data_frame$incision + site_data_frame$elevation

    } else if(isTRUE(ripgelev)){
      # correction for any user input ground elevations lower than the default
      # values are to set at default values
      for (i in 1:nrow(site_data_frame)) {
        for (j in 1:vegzones) {
          for (k in (star_dir_vec)) {
            if (site_data_frame[i,paste0(k,"grndelevVZ",j)] < (site_data_frame[i,"incision"] + site_data_frame[i,"elevation"])) {
              site_data_frame[i,paste0(k,"grndelevVZ",j)] <- site_data_frame[i,"incision"] + site_data_frame[i,"elevation"]
            }
          }
        }
      }
    }





    # Bras solar model:
    # Bras, R.L. 1990. Hydrology: an introduction to hydrologic science. Addison-Wesley
    #
    # Ryan-Stolzenbach model:
    # Ryan, P.J. and K.D. Stolzenbach.1972. Engineering aspects of heat disposal
    # from power generation, (D.R.F. Harleman, ed.). R.M. Parson Laboratory for
    # Water Resources and Hydrodynamics, Department of Civil Engineering,
    # Massachusetts Institute of Technology, Cambridge, MA
    #
    # ODEQ solar model:
    # Boyd, M. and B. Kasper, 2003. Analytical methods for dynamic open channel
    # heat and mass transfer, methodology for the Heat Source model version 7.0.
    # Oregon Department of Environmental Quality and Carollo Engineers.
    # Portland, OR. (www.heatsource.info)
    # Select from: "Bras" "ODEQ" "Ryan-Stolz"
    # options are: 0 = "Bras"
    #              1 = "CE-QUAL-W2"
    #              2 = "Bird"
    #              3 = "ODEQ"
    #              4 = "Ryan-Stolz"
    #              5 = "Iqbal"
    # JsntMethod = JsntMethod  # Main Menu("I4")


    # Enter a global value here that will be applied to all times. If continuous
    # hourly values are entered in row 5 of the 'Diel Solar Output' sheet then
    # they will override the global value.
    # value must be between 0 and 1
    # cloudfrac_global = cloudfrac_global  # Main Menu("i5")

    # cloud fraction can override the global value with  a cloud time series input
    # Read this in as hourly cloud data for each day/hour at each site if desired
    # cloudfrac_hourly <- data.frame(iday = NA, jhr = NA)


    # Atmospheric turbidity factor for calculation of solar radiation that is used
    # if the Bras option for the solar radiation model is selected.
    # (2 = clear, 5 = smog)
    # nfac = nfac  # Main Menu("i6")

    # Atmospheric transmission factor for calculation of solar radiation that is
    # used if the Ryan-Stolzenbach option for solar radiation model is selected.
    # atc = atc   # Main Menu("i7")


    # set vegetation height and density values based on General_Input sub
    # conditional statements in the shade.xls model
    # for (i in 1:nrow(site_data_frame)) {
    #   for (j in 1:vegzones) {
    #     for (k in (star_dir_vec)) {
    #       if (site_data_frame[i,paste0(k,"hghtVZ",j)] == 0 ) {
    #         site_data_frame[i,paste0(k,"densVZ",j)] <- 0
    #       }
    #     }
    #   }
    # }

    # any zone 1 buffers with a vegetation height of 0 have an overhang of 0
    site_data_frame[ ,"NNovrhng"][site_data_frame[ ,"NNhghtVZ1"] == 0] <- 0
    site_data_frame[ ,"NEovrhng"][site_data_frame[ ,"NEhghtVZ1"] == 0] <- 0
    site_data_frame[ ,"EEovrhng"][site_data_frame[ ,"EEhghtVZ1"] == 0] <- 0
    site_data_frame[ ,"SEovrhng"][site_data_frame[ ,"SEhghtVZ1"] == 0] <- 0
    site_data_frame[ ,"SSovrhng"][site_data_frame[ ,"SShghtVZ1"] == 0] <- 0
    site_data_frame[ ,"SWovrhng"][site_data_frame[ ,"SWhghtVZ1"] == 0] <- 0
    site_data_frame[ ,"WWovrhng"][site_data_frame[ ,"WWhghtVZ1"] == 0] <- 0
    site_data_frame[ ,"NWovrhng"][site_data_frame[ ,"NWhghtVZ1"] == 0] <- 0



    fixed_df <- site_data_frame %>%
      rename(site_id = all_of(site_id_chr),
             lat = all_of(lat_chr),
             lon = all_of(lon_chr))  %>%
      mutate(startdate = startdate,
             timezone = timezone,
             ndays = ndays,
             dlstime = dlstime,
             VWidthGlobal = VWidthGlobal,
             JsntMethod = JsntMethod,
             cloudfrac_global = cloudfrac_global,
             nfac = nfac,
             atc = atc) %>% data.frame()

    # return the list holding all named variables for a site
    return(fixed_df)

  }  # end of inputs_fixed() fnx
