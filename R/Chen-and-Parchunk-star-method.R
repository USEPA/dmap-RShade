# Chen et al 1998 SHADE model methods derived from the Washington Ecology
# Department 'shade.xls' model. Code translated from VBA Calc_FLUX sub model.
# optional calculation using Chen or ODEQ method (0=Chen method, 1=ODEQ method)

# The Chen method is recommended for use with the QUAL2K model. The Chen
# method is documented in the PhD dissertation by Y.D. Chen and publications
# in the Journal of Environmental Engineering:
#
# Chen, Y.D. (1996). Hydrologic and water quality modeling for aquatic
# ecosystem protection and restoration in forest watersheds: a case study
# of stream temperature in the Upper Grande Ronde River, Oregon.
# PhD dissertation. University of Georgia. Athens, GA.
#
# Chen, Y.D.,  Carsel, R.F., McCutcheon, S.C., and Nutter, W.L. (1998).
# Stream temperature simulation of forested riparian areas: I.
# watershed-scale model development. Journal of Environmental Engineering.
# April 1998. pp 304-315.
#
# Chen, Y.D.,  Carsel, R.F., McCutcheon, S.C., and Nutter, W.L. (1998).
# Stream temperature simulation of forested riparian areas: II. model
# application. Journal of Environmental Engineering. April 1998. pp 316-328.

# input list:
#    extOpt:
#       - Select 0 ("On") if the canopy density (DEN) codes/data in the
#           riparian_code_ref_df correspond to either densiometer readings or
#           "1-VisSky" data from Hemiview collection. Shadow density is then
#           calculated from path length and Beer's law using riparian
#           extinctionof Ln(1-DEN)/HABS where HABS = "vegetation height".
#       - Select 1 ("Off") if the canopy density (DEN) codes/data in the
#           riparian_code_ref_df corresponds to either angular canopy density
#           (ACD) or effective shade from hemispherical photography (1-GSF
#           from Hemiview). Shadow density is then estimated to be equal to DEN.
#       extOpt = 0 for "On" to use Beer's law
#       extOpt = 1 for "Off" to set input value of DEN = shade density

# site_i <- day_i <- 1; time_i <- 60; fixed_site_df = fixed_site_inputs_meduxnekeag

#' @import chron
#' @import magrittr
#' @import tidyverse
#' @import foreach
#' @import doParallel
NULL

#' Chen and Parchunk star method
#'
#' @param fixed_site_df Sample data is given as a default value.
#' @param time_interval Integration interval in minutes (60 = hourly).
#' @param extOpt Use Beer's Law.
#' @param cloudfrac_flag Default uses global cloud fraction.
#' @param shddenadd Chen et al. 1998 model 20% constant pg311.
#' @param ovhvegadd ovhvegadd
#' @param smpl_dir smpl_dir
#' @param star_veg star_veg
#' @param vegzones vegzones
#' @param par_processes Number of parallel processes to run.
#' @param profilingQ Should profiling information be printed out or not.
#' @param ...
#' @details
#' Chen et al 1998 SHADE model methods derived from the Washington Ecology
#' Department 'shade.xls' model. Code translated from VBA Calc_FLUX sub model.
#' optional calculation using Chen or ODEQ method (0=Chen method, 1=ODEQ method)
#'
#' The Chen method is recommended for use with the QUAL2K model. The Chen
#' method is documented in the PhD dissertation by Y.D. Chen and publications
#' in the Journal of Environmental Engineering:
#'
#' Chen, Y.D. (1996). Hydrologic and water quality modeling for aquatic
#' ecosystem protection and restoration in forest watersheds: a case study
#' of stream temperature in the Upper Grande Ronde River, Oregon.
#' PhD dissertation. University of Georgia. Athens, GA.
#'
#' Chen, Y.D.,  Carsel, R.F., McCutcheon, S.C., and Nutter, W.L. (1998).
#' Stream temperature simulation of forested riparian areas: I.
#' watershed-scale model development. Journal of Environmental Engineering.
#' April 1998. pp 304-315.
#'
#' Chen, Y.D.,  Carsel, R.F., McCutcheon, S.C., and Nutter, W.L. (1998).
#' Stream temperature simulation of forested riparian areas: II. model
#' application. Journal of Environmental Engineering. April 1998. pp 316-328.
#'
#' @return data frame
#' @family Method
#' @export
method_chen98_parchunk_star <-                 # default value comments below
  function(fixed_site_df = fixed_site_inputs,  # sample data
           time_interval = 60,  # integration interval in minutes (60 = hourly)
           extOpt = 0, # use Beer's Law
           cloudfrac_flag = "global",  # default uses global cloud fraction
           shddenadd = 0.2, # Chen et al. 1998 model 20% constant pg311
           ovhvegadd = 1,
           smpl_dir = "three",
           star_veg = FALSE,
           vegzones = 1,
           par_processes = 2, # number of parallel processes to run
           profilingQ = TRUE,
           ...) {
    # set function start time for tracking elapsed function run time
    start_time_running <- Sys.time()
    if( profilingQ ) {
      print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      print("              FNX RUNNING TIME               ")
      print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      print(paste("   Start at: ",start_time_running))
    }

    # set up parallel processing back end
    cl <- parallel::makeCluster(par_processes)
    doParallel::registerDoParallel(cl)

    clusterCall(cl, function(x) .libPaths(x), .libPaths())

    # iteration set up for sites in fixed_site_df
    site_iter <- 1:nrow(fixed_site_df)

    # number of rows for each data frame chunk to be run on each parallel process
    chunk_len <- ceiling(length(site_iter)/par_processes)

    # set the group values for each parallel data frame chunk
    fixed_site_df$parchunk <- rep(1:par_processes, each = chunk_len)[site_iter]

    # Build a list of data frames to be used in each parallel process
    site_ls <- split(fixed_site_df, f=fixed_site_df$parchunk)


    site_df <-
      foreach::foreach(i = 1:length(site_ls),
              .combine = 'rbind',
              .packages = c("magrittr","tidyverse")) %dopar% {

                devtools::load_all()
                library(EPAShader)

                # provide the subset of the data frame for the parallel process
                par_dat <- data.frame(site_ls[[i]])

                site_par_iter <- 1:nrow(par_dat)


                # source shade helper functions by gathering their file names
                helper_fnxs_ls <-
                  list.files(path = "scripts/helperfnx/", pattern = ".R", full.names = TRUE)

                # run list of helper fnxs through lapply() to load each into the GlbEnv
                lapply(helper_fnxs_ls, source)


                # Select "On"  if the canopy density (DEN) on the 'Riparian Codes' sheet
                # corresponds to densiometer readings or 1-VisSky from Hemiview. Shadow density
                # is then calculated from path length and Beer's law using riparian extinction
                # of Ln(1-DEN)/HABS where HABS is vegetation height.
                #
                # Select "Off" if the canopy density (DEN) on the 'Riparian Codes' sheet
                # corresponds to angular canopy density (ACD) or effective shade from
                # hemispherical photography (1-GSF from Hemiview). Shadow density is then
                # estimated to be equal to DEN.
                #
                # This option only affects results using Chen's shade calculation method.
                # extOpt = 0 for "On" to use Beer's law
                # extOpt = 1 for "Off" to set input value of DEN = shade density
                # extOpt <- extOpt  # shade.xls sheet Main Menu("i9")



                # iteration vector for time integration points within a day [86400 sec/day]
                # only use the hours of the day that may contain sunlight (4AM to 10PM)
                time_iter <- seq(14400,79200, by = (time_interval*60))



                dir7_df <-
                  data.frame(tran = 1:7,
                             topo_dir = c("topoNE","topoEE","topoSE","topoSS",
                                          "topoSW","topoWW","topoNW"),
                             card_dir = c("NE","EE","SE","SS", "SW","WW","NW"))

                # intercard_angle_deg <-
                #   c(0,22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5,360.000000001)
                # intercard_dir <-
                #   c("NN","NE","EE","SE","SS","SW","WW","NW","NN")
                # card_dir_df <-
                #   data.frame(angle_interval = intercard_angle_deg,
                #              card_dir = intercard_dir)
                #
                # bank_dir_keydf <-
                #   data.frame(aspect_tran = 1:9,
                #              Rbank_dir = c("EE","SE","SS","SW","WW","NW","NN","NE","EE"),
                #              Lbank_dir = c("WW","NW","NN","NE","EE","SE","SS","SW","WW"))


                # empty data frame to hold site output integrated across day/time intervals
                site_solar_df <- data.frame(site_id = NA, Solar_FLUX_above = NA,
                                            Solar_FLUX_below = NA, Solar_FLUX_inh2o = NA)

                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # iterate across all sites
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                for (site_i in site_par_iter) {

                  # grab an individual site data row from the par_dat
                  site_dat <- par_dat[site_i, ]

                  # load variables for the site into the global environment
                  ls1 <- lapply(colnames(site_dat),function(x){x <- site_dat[1, x]})
                  names(ls1) <- colnames(site_dat)
                  list2env(ls1, globalenv())

                  # Set the start date for the day iterations
                  startdate <- lubridate::as_date(startdate)

                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # 'Start - Calculate Riparian Boundaries and View to Sky
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # 'Veg_Elev... = Riparian height above stream (meters)
                  # 'Transverse... = Distance from center line to edge of zone (meters)
                  # 'View_To_Sky = SKOP = fraction of the sky that is not blocked
                  # '              by vegetation, riparian topography or incision.
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  #   Bank index 0 is left bank, 1 is right bank
                  #   Zones 1 through 9 below are zones 1 through 9 on the 'Main Menu' sheet.
                  #   Zone 0 below is the NSDZ.
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # set the number of vegetation zones in the analysis
                  # vegzones <- vegzones


                  Transverse <-
                    transverse_matrix_fnx(site_dat[1, paste0("VWidthL",2:vegzones)],
                                          site_dat[1, paste0("VWidthR",2:vegzones)],
                                          vegzones)


                  Veg_Elev <- matrix(data = 0, nrow = 8, ncol = vegzones+1)
                  # zone 0 = incision
                  Veg_Elev[ ,1] <- incision
                  # Zone 1 and greater = ground elev - channel elev + veg height
                  if(vegzones == 1) {
                    for(d in 1:length(dir7_df$card_dir)) {
                      Veg_Elev[d, 2] <-
                        get(paste0(dir7_df$card_dir[d],"grndelevVZ1")) -
                        elevation + get(paste0(dir7_df$card_dir[d],"hghtVZ1"))
                    }

                  } else {
                    for (v in 2:(vegzones+1)) {
                      for(d in 1:length(dir7_df$card_dir)) {
                        Veg_Elev[d, v] <-
                          get(paste0(dir7_df$card_dir[d],"grndelevVZ",(v-1))) -
                          elevation + get(paste0(dir7_df$card_dir[d],"hghtVZ",(v-1)))
                      }
                    }
                  }

                  VTS1 <- 90 - (180 / 3.14159265359) * atan(Veg_Elev[c(1,5),]/Transverse)
                  VTS2 <- 90 - (180 / 3.14159265359) * atan(Veg_Elev[c(2,6),]/Transverse)
                  VTS3 <- 90 - (180 / 3.14159265359) * atan(Veg_Elev[c(3,7),]/Transverse)
                  VTS4 <- 90 - (180 / 3.14159265359) * atan(Veg_Elev[c(4,8),]/Transverse)

                  SKOP1 <- sum(apply(VTS1,1,min)) / 180
                  SKOP2 <- sum(apply(VTS2,1,min)) / 180
                  SKOP3 <- sum(apply(VTS3,1,min)) / 180
                  SKOP4 <- sum(apply(VTS4,1,min)) / 180

                  SKOP <- mean(SKOP1,SKOP2,SKOP3,SKOP4)

                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # calculated distances and vegetation heights for zones for each site bank
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # DIS   = distance (meters) from the edge of stream wetted-perimeter
                  #         to the near stream polygon (rip. veg. buffer zone) boundary
                  # WID   = width of vegetation polygon (buffer zone width)
                  # HABS  = absolute value of the mean veg. height (m) of buffer zone
                  # HDEM  = mean veg. height (m) of polygon (buffer zone) in reference
                  #         to the stream surface elevation (stream surface elevation is
                  #         the sum of HABS and difference between the ground elevation
                  #         and the stream channel elevation)
                  # DEN   = mean canopy density of the polygon (veg. buffer zone)
                  # SHDWID = shadow width measured perpendicularly to the stream in meters
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  DIS_mx <- WID_mx <- matrix(0,2,vegzones+1)
                  HABS8_mx <- DEN8_mx <- matrix(0,8,vegzones+1)

                  DIS_mx[1, 1:2] <- disfromcentertolb - (wwidth / 2)
                  DIS_mx[2, 1:2] <- bfwidth - (disfromcentertolb + (wwidth / 2))
                  if(vegzones > 1) {
                    DIS_mx[1, 3:(vegzones+1)] <-
                      DIS_mx[1,2] + cumsum(t(site_dat[1, paste0("VWidthL",2:vegzones)]))
                    DIS_mx[2, 3:(vegzones+1)] <-
                      DIS_mx[2,2] + cumsum(t(site_dat[1, paste0("VWidthR",2:vegzones)]))
                  }
                  # if any DIS values are zero or very small, set them equal to 0.01
                  DIS_mx[DIS_mx < 0.01] <- 0.01

                  WID_mx[1, ] <- c(0, t(site_dat[1, paste0("VWidthL",1:vegzones)]))
                  WID_mx[2, ] <- c(0, t(site_dat[1, paste0("VWidthL",1:vegzones)]))



                  for (v in 2:(vegzones+1)) {
                    for(d in 1:length(dir7_df$card_dir)) {
                      HABS8_mx[d, v] <- get(paste0(dir7_df$card_dir[d],"hghtVZ",(v-1)))
                    }
                  }

                  for (v in 2:(vegzones+1)) {
                    for(d in 1:length(dir7_df$card_dir)) {
                      DEN8_mx[d, v] <- get(paste0(dir7_df$card_dir[d],"densVZ",(v-1)))
                    }
                  }

                  HDEM8_mx <- Veg_Elev



                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # 'End - Calculate Riparian Boundaries and View to Sky
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # object value tracking for solar fluxes by ndays
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # Solar_Flux_ndays_list <-
                  #   list(ndaysSolar_FLUX_inh2o = rep(0,times = ndays),
                  #        ndaysSolar_FLUX_above = rep(0,times = ndays),
                  #        ndaysSolar_FLUX_below = rep(0,times = ndays))
                  # vectors to hold time interval values in next for-loop for time_iter
                  ndaysSolar_FLUX_inh2o <- ndaysSolar_FLUX_above <- ndaysSolar_FLUX_below <-
                    rep(0,times = ndays)
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # iterate across all days in the ndays field from site_dat data frame
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  for (day_i in 1:ndays) {

                    # set the date based on ndays vector
                    tdate <- startdate + (day_i - 1)
                    tyear <- lubridate::year(tdate)
                    tmonth <- lubridate::month(tdate) %>% as.integer()
                    tday <- lubridate::day(tdate)

                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # calculate solar noon for diffuse radiation fraction calculation/day
                    tsolarnoon <-
                      solarnoon(lat, lon, tyear, tmonth, tday, timezone, dlstime)
                    tsolarhh <-
                      lubridate::hour(lubridate::hms(paste0(chron::times(tsolarnoon))))
                    tsolarmm <-
                      lubridate::minute(lubridate::hms(paste0(chron::times(tsolarnoon))))
                    tsolarss <-
                      lubridate::second(lubridate::hms(paste0(chron::times(tsolarnoon))))

                    SolarPosition_DIFFRC <-
                      SolarPosition(lat, lon, tyear, tmonth, tday, tsolarhh, tsolarmm,
                                    tsolarss, timezone, dlstime)

                    if (JsntMethod == "Bras") {
                      # use Chapra's QUAL2K method based on Bras model with solar
                      # elevation from SolarPosition fnx and NREL solar constant
                      brassolar_DIFFRC <-  # clear sky Iclear units of W/m^2
                        BrasSolar(SolarPosition_DIFFRC$solarelevation,
                                  SolarPosition_DIFFRC$earthRadVec,
                                  nfac)

                      # convert Iclear and I0 from W/m^2 to cal/cm^2/d
                      Jsnt_DIFFRC <- brassolar_DIFFRC$Iclear / (4.184 * 100 * 100 / 86400)
                      JsntTOA_DIFFRC <- brassolar_DIFFRC$I0 / (4.184 * 100 * 100 / 86400)

                    }  else if (JsntMethod == "ODEQ") {
                      # use HeatSource version 7 method
                      hs7Solar_DIFFRC <- hs7Solar(tdate, tsolarhh, elevation, SolarPosition_DIFFRC$solarelevation)

                      # Jsnt and JsntTOA in cal/cm^2/d in this hs7solar
                      Jsnt_DIFFRC <- hs7Solar_DIFFRC$Jsnt
                      JsntTOA_DIFFRC <- hs7Solar_DIFFRC$JsntTOA

                    }  else if (JsntMethod == "Ryan-Stolz") {  # use W2Met method
                      # use Ryan-Stolzenbach method
                      RyanStolzSolar_DIFFRC <-
                        RyanStolzSolar(SolarPosition_DIFFRC$solarelevation,
                                       SolarPosition_DIFFRC$earthRadVec,
                                       atc, elevation)

                      # convert from W/m^2 to cal/cm^2/d
                      Jsnt_DIFFRC <- RyanStolzSolar_DIFFRC$Iclear / (4.184 * 100 * 100 / 86400)
                      JsntTOA_DIFFRC <- RyanStolzSolar_DIFFRC$I0 / (4.184 * 100 * 100 / 86400)
                    }

                    # select between user-supplied global cloud fraction or hourly cloud
                    # fraction data. These hourly data would be supplied as ... need to
                    # implement this portion still ...
                    if (cloudfrac_flag == "hourly") {
                      cloudfrac <- cloudfrac_hourly[day_i, tsolarhh]

                    } else if (cloudfrac_flag == "global") {
                      cloudfrac <- cloudfrac_global

                    } else {
                      print("Please select cloud fraction measure (either 'hourly' or 'global').")
                    }

                    # cloud cover adjustment of solar radiation
                    Jsnt_DIFFRC_ccadj <- Jsnt_DIFFRC * (1 - 0.65 * cloudfrac^2)



                    # calculate the daily average diffuse fraction of solar radiation
                    # assuming daily average solar radiation is proportional to solar
                    # radiation at solar noon from the clearness index KT for the
                    # Pacific Northwest based on:
                    #     Vignola F. and D.K. McDaniels, 1984. Diffuse-global correlation:
                    #         seasonal variations. Solar Energy 33(5):397-402.
                    KTindex <- Jsnt_DIFFRC_ccadj/JsntTOA_DIFFRC
                    if ( is.na(KTindex) ) {KTindex <- 0}

                    # See page 309 section 'Disaggregation of Global Solar Radiation' in
                    # Chen et al 1998 for this equation and its explanation [EQ 2]
                    diffuseFraction <-
                      0.938 +
                      1.071 * KTindex -
                      5.146 * (KTindex^2) +
                      2.982 * (KTindex^3) -
                      (0.009 - 0.078 * KTindex) * sin(0.017214 * (lubridate::yday(tdate) - 40))

                    if (diffuseFraction > 1) {diffuseFraction <- 1}
                    if (diffuseFraction < 0) {diffuseFraction <- 0}
                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



                    # vectors to hold time interval values in next for-loop for time_iter
                    tSolar_FLUX_inh2o <- tSolar_FLUX_above <- tSolar_FLUX_below <-
                      rep(0,times = length(time_iter))


                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # iterate across all time intervals in a day
                    for (time_i in 1:length(time_iter)) {


                      # set the time of day in seconds using the time_iter vector
                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      #     Start - Temporal Solar Calculated Parameters
                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      thh <- lubridate::hour(lubridate::hms(paste0(chron::times(time_iter[time_i] / 86400))))
                      tmm <- lubridate::minute(lubridate::hms(paste0(chron::times(time_iter[time_i] / 86400))))
                      tss <- lubridate::second(lubridate::hms(paste0(chron::times(time_iter[time_i] / 86400))))

                      # calculate solar position based on date/time
                      SolarPosition_out <-
                        SolarPosition(lat, lon, tyear, tmonth, tday, thh, tmm, tss,
                                      timezone, dlstime)

                      # aznoaa <- SolarPosition_out$solarazimuth
                      # elnoaa <- SolarPosition_out$solarelevation
                      # erv <- SolarPosition_out$earthRadVec

                      # assign solar position to Chen's solar variables
                      # AZIM <- degToRad(SolarPosition_out$solarazimuth)   # azimuth in radians
                      AZDEG <- SolarPosition_out$solarazimuth            # azimuth in degrees
                      # ZEN <- degToRad(90 - SolarPosition_out$solarelevation) #zenith in radians
                      # ZENDEG <- 90 - SolarPosition_out$solarelevation        #zenith in degrees
                      ALT <- degToRad(SolarPosition_out$solarelevation)      #altitude in radians
                      if(ALT < 0) {ALT <- 0}

                      ALTDEG <- SolarPosition_out$solarelevation             #altitude in degrees
                      # SOSP <- degToRad(aspect)        #stream aspect in radians

                      #   assign solar position to shadealator's solar variables
                      # Solar_Azimuth <- degToRad(SolarPosition_out$solarazimuth)



                      # Solar_Zenith <- degToRad(90) - ALT
                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      #     End - Temporal Solar Calculated Parameters
                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      #    Start - set topographic sampling direction transect
                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      tran <-
                        findInterval(x = SolarPosition_out$solarazimuth,
                                     vec = c(0,67.5,112.5,157.5,202.5,247.5,292.5,360))

                      # set TopoShade transect for this time-stamp's solar angle
                      TopoShade <- site_dat[1, dir7_df[tran, "topo_dir"]]
                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      #    End - Topo Direction Setting
                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      # 'Start - Determine which star direction is sunward
                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      VegTShade_dir <- dir7_df[tran, "card_dir"]
                      # generate df for all the data for this direction
                      VegTShade <-
                        select(site_dat, starts_with(dir7_df[tran, "card_dir"]))

                      if (tran < 5) {
                        HABS_mx <- HABS8_mx[c(tran,(tran+4)),]
                        DEN_mx <-  DEN8_mx[c(tran,(tran+4)),]
                        HDEM_mx <- Veg_Elev[c(tran,(tran+4)),]
                      } else {
                        HABS_mx <- HABS8_mx[c(tran,(tran-4)),]
                        DEN_mx <-  DEN8_mx[c(tran,(tran-4)),]
                        HDEM_mx <- Veg_Elev[c(tran,(tran-4)),]
                      }

                      if(aspect >= 0 & aspect <= 180) {
                        if(AZDEG > aspect & AZDEG <= aspect + 180) {
                          Bank_Flag <- 1
                        } else {
                          Bank_Flag <- 0
                        }
                      } else {
                        if(AZDEG < aspect & AZDEG >= aspect - 180) {
                          Bank_Flag <- 0
                        } else {
                          Bank_Flag <- 1
                        }
                      }
                      # Grab overhanging vegetation bank ID
                      # OHB <- ifelse(Bank_Flag == 0, "L1", "R1")
                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      # 'End - Determine which bank is sunward
                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



                      # *******************************************************************
                      #               START - Shadow Casting
                      # *******************************************************************
                      # SHDWID = shadow width perpendicular to stream aspect that extends past wetted channel edge
                      #         for each contributing buffer zone
                      # SHDDEN = effective shade density for each contributing buffer zone
                      # VEGSHD = accumulated effective length of the shadow formed by single or
                      #         multiple contributing buffers. The shadows of all contributing
                      #         buffers are sorted and renumbered based on SHDWID in
                      #         increasing order. The accumulated effective shade from multiple
                      #         contributing buffers is estimated as as a composite shadow whose
                      #         density tends to decrease from the sun-ward bank to the other bank
                      #         of the stream. Each segment of the composite shadow has an actual
                      #         length of SHDWID(i) - SHDWID(i-1) and an accumulated density that
                      #         is assumed to equal the maximum SHDDEN of the contributing buffers
                      #         to that segment plus 20% of the other contributing buffers SHDDEN
                      #         provided that the accumulated density (ACDEN) never exceeds 1.
                      #         The effective length of the segment is equal to its actual length
                      #         multiplied by its accumulated density. The sum of all effective lengths
                      #         of all segments is VEGSHD.
                      # OVHSHD = Effective length of shading from overhanging vegetation from the first
                      #         vegetation zone if it covers part or all of the stream.
                      # FRCSHD = Fraction of stream width completely blocked from beam radiation due
                      #         to VEGSHD and OVHSHD or topographic shading.
                      # BankShade_Flag = 0 if left bank provides shade, 1 if right bank
                      # DIS   = distance (meters) from the edge of stream wetted-perimeter
                      #         to the near stream polygon (rip. veg. buffer zone) boundary
                      # WID   = width of vegetation polygon (buffer zone width)
                      # HABS  = absolute value of the mean veg. height (m) of buffer zone
                      # HDEM  = mean veg. height (m) of polygon (buffer zone) in reference
                      #         to the stream surface elevation (stream surface elevation is
                      #         the sum of HABS and difference between the ground elevation
                      #         and the stream channel elevation)
                      # DEN   = mean canopy density of the polygon (veg. buffer zone)
                      # SHDWID = shadow width measured perpendicularly to the stream in meters
                      # LAMDA = extinction coefficient of vegetation buffer
                      # AVEPL = average path length of direct beam radiation through each
                      #         buffer zone in meters
                      # SHDDEN = effective shade density (unitless)
                      # PLHMIN = The minimum HDEM of a vegetation buffer that can produce
                      #         stream shade from the near size of the buffer in meters
                      # PLHMIN2 = The minimum HDEM of a vegetation buffer that can produce
                      #         stream shade from the far side of the buffer in meters
                      # PLHFCV = The HDEM required for generating a shadow of full cover on
                      #         the stream surface from the near side of the buffer (meters)
                      # PLHFCV2 = The HDEM required for generating a shadow of full cover on
                      #         the stream surface from the far side of the buffer (meters)
                      # *******************************************************************
                      # set the following vectors based on the sunward bank Bank_Flag
                      if(Bank_Flag == 0) {
                        WID <- WID_mx[1,]
                        HABS <-HABS_mx[1,]
                        HDEM <-HDEM_mx[1,]
                        DEN <- DEN_mx[1,]
                        DIS <- DIS_mx[1, ]
                      } else if(Bank_Flag == 1) {
                        WID <- WID_mx[2,]
                        HABS <-HABS_mx[2,]
                        HDEM <-HDEM_mx[2,]
                        DEN <- DEN_mx[2,]
                        DIS <- DIS_mx[2, ]
                      }



                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      # Set default values for objects when time falls during night (no sun)
                      VEGSHD <- 0
                      FRCSHD <- 0
                      SHDWID_scb <- -9999
                      SHDDEN_scb <- -9999
                      SINFAC <- 0
                      OVHSHD <- 0
                      LOH <- 0
                      PLHMIN <- -9999
                      PLHMIN2 <- -9999
                      PLHFCV <- -9999
                      PLHFCV2 <- -9999
                      LAMDA <- -9999
                      AVEPL <- -9999
                      PLCASE <- -9999
                      ACDEN_ls <- -9999
                      NCB <- -9999
                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      # do next calcs if daytime
                      if (ALTDEG > 0) {

                        # identify and characterize shade-contributing buffers using VBA
                        # translation of Chen's Fortran code
                        # Set this value to avoid repeated long code in later lines
                        SINFAC <-
                          abs(sin(abs(degToRad(SolarPosition_out$solarazimuth) -
                                        degToRad(aspect))))

                        # min HDEM that can produce shade from near side of buffer
                        PLHMIN <- DIS * tan(ALT) / SINFAC
                        # min HDEM that can produce shade from far side of buffer
                        PLHMIN2 <- (DIS + WID) * tan(ALT) / SINFAC

                        # min HDEM that can shade entire stream from near side of buffer
                        PLHFCV <- (wwidth + DIS) * tan(ALT) / SINFAC

                        # min HDEM that can shade entire stream from far side of buffer
                        PLHFCV2 <- (wwidth + DIS + WID) * tan(ALT) / SINFAC


                        # Tally the number of buffer zones contributing shade to the stream (NCB)
                        CB_Z_numb <- seq(1,(vegzones+1),by = 1)[HDEM>PLHMIN]
                        NCB <- length(CB_Z_numb)

                        # compute effective shade width perpendicular to stream
                        SHDWID <- ((HDEM * SINFAC) / tan(ALT)) - DIS  # chen et al 1998 EQ15
                        SHDWID[SHDWID < 0] <- 0
                        SHDWID[SHDWID > wwidth] <- wwidth

                        # if(sum(SHDWID) > 0) {
                        # 'compute actual shade using Beer's law
                        # 'first, compute extinction coefficient (LAMDA):

                        LAMDA <- -log(1 - DEN) / HABS
                        LAMDA[HABS == 0] <- 0 #Maybe this one????
                        LAMDA[DEN == 0] <- 0
                        LAMDA[DEN == 1] <- 1
                        LAMDA[SHDWID == 0] <- 0
                        # LAMDA[is.infinite(LAMDA)] <- 0

                        # 'second, compute average path length of sunbeam:
                        # function to calculate average path lengths through each zone
                        avg_path_length_fnx <- function(i) {
                          # set up if-else statements for six different shade cases

                          if (HDEM[i] >= PLHFCV[i] & HDEM[i] >= PLHFCV2[i]) {
                            # case 1) near and far side of buffer shade entire stream:
                            if (i == 2) {
                              PLMAX <- (WID[i] + site_dat[1,paste0(VegTShade_dir,"ovrhng")] ) / (SINFAC * cos(ALT))
                            } else {
                              PLMAX <- WID[i] / (SINFAC * cos(ALT))
                            }
                            PLMIN <- PLMAX

                          } else if (HDEM[i] >= PLHFCV[i] & HDEM[i] < PLHFCV2[i] & HDEM[i] >= PLHMIN2[i]) {
                            # case 2) near side shades entire stream, far side shades only a portion of the stream:
                            if (i == 2) {
                              PLMAX <- (WID[i] + site_dat[1,paste0(VegTShade_dir,"ovrhng")]) / (SINFAC * cos(ALT))
                            } else {
                              PLMAX <- WID[i] / (SINFAC * cos(ALT))
                            }
                            PLMIN <- (HDEM[i] - PLHFCV[i]) / sin(ALT)

                          } else if (HDEM[i] >= PLHFCV[i] & HDEM[i] < PLHMIN2[i]) {
                            # case 3) near side shades entire stream, far side doesn't shade any of the stream:
                            PLMAX <- (HDEM[i] - PLHMIN[i]) / sin(ALT)
                            PLMIN <- (HDEM[i] - PLHFCV[i]) / sin(ALT)

                          } else if (HDEM[i] < PLHFCV[i] & HDEM[i] >= PLHMIN[i] & HDEM[i] < PLHFCV2[i] & HDEM[i] >= PLHMIN2[i]) {
                            # case 4) near side shades only a portion, far side shades only a portion:
                            if (i == 2) {
                              PLMAX <- (WID[i] + site_dat[1,paste0(VegTShade_dir,"ovrhng")]) / (SINFAC * cos(ALT))
                            } else {
                              PLMAX <- WID[i] / (SINFAC * cos(ALT))
                            }
                            PLMIN <- 0

                          } else if (HDEM[i] < PLHFCV[i] & HDEM[i] >= PLHMIN[i] & HDEM[i] < PLHMIN2[i]) {
                            # case 5) near side shades only a portion of the stream, far side doesn't shade any of the stream:
                            PLMAX <- (HDEM[i] - PLHMIN[i]) / sin(ALT)
                            PLMIN <- 0

                          } else {
                            # case 6) the buffer doesn't provide any shade:
                            PLMAX <- 0
                            PLMIN <- 0

                          }

                          if (PLMIN > PLMAX) {PLMIN <- PLMAX}

                          AVEPL <- (PLMAX + PLMIN) / 2

                          if (AVEPL > HABS[i]) {AVEPL <- HABS[i]}

                          return(AVEPL)

                        }
                        path_length_case_fnx <- function(i) {
                          # set up if-else statements for six different shade cases
                          if (HDEM[i] >= PLHFCV[i] & HDEM[i] >= PLHFCV2[i]) {
                            # case 1) near and far side of buffer shade entire stream:
                            PLCASE <- 1
                          } else if (HDEM[i] >= PLHFCV[i] & HDEM[i] < PLHFCV2[i] & HDEM[i] >= PLHMIN2[i]) {
                            # case 2) near side shades entire stream, far side shades only a portion of the stream:
                            PLCASE <- 2
                          } else if (HDEM[i] >= PLHFCV[i] & HDEM[i] < PLHMIN2[i]) {
                            # case 3) near side shades entire stream, far side doesn't shade any of the stream:
                            PLCASE <- 3
                          } else if (HDEM[i] < PLHFCV[i] & HDEM[i] >= PLHMIN[i] & HDEM[i] < PLHFCV2[i] & HDEM[i] >= PLHMIN2[i]) {
                            # case 4) near side shades only a portion, far side shades only a portion:
                            PLCASE <- 4
                          } else if (HDEM[i] < PLHFCV[i] & HDEM[i] >= PLHMIN[i] & HDEM[i] < PLHMIN2[i]) {
                            # case 5) near side shades only a portion of the stream, far side doesn't shade any of the stream:
                            PLCASE <- 5
                          } else {
                            # case 6) the buffer doesn't provide any shade:
                            PLCASE <- 6
                          }

                          return(PLCASE)

                        }
                        # run the funciton by providing sapply() with a vector from 1 through
                        # the length of vegzones (+ 1 to accomodate zone 0)
                        AVEPL <- sapply(1:(vegzones+1),
                                        avg_path_length_fnx)
                        PLCASE <- sapply(1:(vegzones+1),
                                         path_length_case_fnx)

                        # 'third, compute shade density:
                        SHDDEN <- 1 - exp(-AVEPL * LAMDA)


                        if(extOpt == 1) {SHDDEN <- DEN}
                        SHDDEN[HABS == 0] <- 0
                        SHDDEN[DEN == 0] <- 0
                        SHDDEN[DEN == 1] <- 1


                        # use these if we are eliminating the NSDZ shade as noted in VBA
                        # SHDDEN[1] <- 0
                        # SHDWID[1] <- 0

                        # sorting of shade-contributing buffers based on shade width
                        # (SHDWID) in increasing order
                        SHDWID_sorted <- sort(SHDWID)

                        # match sorted order to SHDDEN vector too
                        SHDDEN_sorted <- SHDDEN[order(SHDWID)]

                        # Selection of shade contributing buffers only
                        SHDWID_scb <- SHDWID_sorted[SHDWID_sorted > 0]

                        SHDDEN_scb <- SHDDEN_sorted[SHDWID_sorted > 0]
                        NCB <- length(SHDWID_scb)

                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # compute accumulated vegetation shade of all contributing buffers
                        # optional variable shddenadd factor for additivity of overlapping
                        # buffers. Chen et al 1998 assumed a default value of 20%, therefore
                        # shddenadd = 0.2 in function inputs.
                        # See pg 311 of Chen et al '98 paragraph after EQ 16f for details
                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # gather the maximum SHDDEN value for contributing shadows
                        SHDDENsegmax <- rev(cummax(rev(SHDDEN_scb)))

                        # gather cumulative shade density values for contributing shadows
                        # that exclude the maximum shadow density for the segment
                        SHDDENcsum <- rev(cumsum(rev(SHDDEN_scb))) - SHDDENsegmax

                        # calculate effective shade density for each shade segment
                        # values cannot exceed 1
                        ACDEN_ls <- SHDDENsegmax + (shddenadd * (SHDDENcsum))

                        ACDEN_ls[ACDEN_ls > 1] <- 1


                        # calculate the widths of each composite shadow segment
                        SHDWIDsegwidth <- c(SHDWID_scb[1],diff(SHDWID_scb))


                        # calculated the accumulated effective shade width (VEGSHD)
                        VEGSHD <- sum(SHDWIDsegwidth * ACDEN_ls)


                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # }


                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        #   compute shade contributed by overhanging canopy
                        # [Chen et al 1998 EQ 17a,17b]
                        LOH <- site_dat[1,paste0(VegTShade_dir,"ovrhng")] - DIS[2] # DIS[2] is Zone 1
                        if(LOH > 0) {
                          if (LOH >= wwidth) {
                            OVHSHD <- wwidth * DEN[2]
                          } else {
                            OVHSHD <- LOH * DEN[2]
                          }
                        } else {
                          OVHSHD <- 0
                        }
                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # shaded fraction of stream surface from bank, veg, overhang, topo
                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # if either VEGSHD or OVHSHD are greater than wwidth, reset = wwidth

                        if (VEGSHD > wwidth) {VEGSHD <- wwidth}
                        if (OVHSHD > wwidth) {OVHSHD <- wwidth}

                        # determine whether VEGSHD or OVHSHD is larger and use the largest
                        # of the two values to estimate FRCSHD

                        # ovhvegadd is an optional variable for additivity of veg buffer
                        # and overhang.Tthe original Chen et al. 1998 methods assume that
                        # ovhvegadd = 1 as a default value. See paragraph beginning with
                        # 'Fourth, VEGSHD + OVHSHD is divided ...' on page 311 for details.
                        if (VEGSHD >= OVHSHD) {
                          FRCSHD <- (VEGSHD + (ovhvegadd * OVHSHD) ) / wwidth
                        } else {
                          FRCSHD <- (OVHSHD + (ovhvegadd * VEGSHD) ) / wwidth
                        }


                        # fractional shade cannot be greater than 1.
                        if (FRCSHD > 1) {FRCSHD <- 1}
                        if (ALTDEG <= TopoShade) {FRCSHD <- 1}

                      }

                      # *******************************************************************
                      #               END - Shadow Casting
                      # *******************************************************************


                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      # Start - Solar Routing
                      # Stream surface reflection equations are a function of solar altitude &
                      # must be treated differently once solar altitude is greater than 80 deg.
                      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      if (ALTDEG <= 0) {
                        JsntTOA <- 0
                        Jsnt <- 0
                      } else {
                        # calculate clear sky solar rad at solar noon to estimate diffuse fraction
                        #   optional solar radiation codes for clear sky
                        if (JsntMethod == "Bras") {
                          # use Chapra's QUAL2K method based on Bras model with solar
                          # elevation from SolarPosition fnx and NREL solar constant
                          brassolar_out <-  # clear sky Iclear units of W/m^2
                            BrasSolar(ALTDEG, SolarPosition_out$earthRadVec, nfac)

                          # convert Iclear and I0 from W/m^2 to cal/cm^2/d
                          Jsnt <- brassolar_out$Iclear / (4.184 * 100 * 100 / 86400)
                          JsntTOA <- brassolar_out$I0 / (4.184 * 100 * 100 / 86400)

                        }  else if (JsntMethod == "ODEQ") {
                          # use HeatSource version 7 method
                          hs7Solar_out <- hs7Solar(tdate, thh, elevation, ALTDEG)

                          # Jsnt and JsntTOA in cal/cm^2/d in this hs7solar
                          Jsnt <- hs7Solar_out$Jsnt
                          JsntTOA <- hs7Solar_out$JsntTOA

                        }  else if (JsntMethod == "Ryan-Stolz") {  # use W2Met method
                          # use Ryan-Stolzenbach method
                          RyanStolzSolar_out <-
                            RyanStolzSolar(ALTDEG, SolarPosition_out$earthRadVec, atc, elevation)

                          # convert from W/m^2 to cal/cm^2/d
                          Jsnt <- RyanStolzSolar_out$Iclear / (4.184 * 100 * 100 / 86400)
                          JsntTOA <- RyanStolzSolar_out$I0 / (4.184 * 100 * 100 / 86400)
                        }
                      }

                      # if hourly cloud data are input by user, then update cloudfrac here
                      # ...  need to implement hourly portion still  ...
                      if (cloudfrac_flag == "global") {
                        cloudfrac <- cloudfrac_global

                      } else if (cloudfrac_flag == "hourly") {
                        cloudfrac <- cloudfrac_hourly[day_i, tsolarhh]

                      } else {
                        print("Please select cloud fraction measure (either 'hourly' or 'global').")
                      }

                      # cloud cover adjustment of solar radiation
                      Jsnt_ccadj <- Jsnt * (1 - 0.65 * cloudfrac^2)

                      # calculate beam (RADB) and diffuse (RADD) components of total solar
                      # radiation above the canopy using the diffuse fraction from the
                      # Vignola and McDaniels 1984 equation [units of cal/cm^2/day]
                      if (ALTDEG > 0) {
                        if (ALTDEG > TopoShade) {  # Topo shade is not shading entire stream
                          RADG <- Jsnt_ccadj
                          RADD <- Jsnt_ccadj * diffuseFraction
                          RADB <- RADG - RADD
                        } else {  # topo shade shades entire stream so no direct beam radiation
                          RADG <- Jsnt_ccadj * diffuseFraction
                          RADD <- Jsnt_ccadj * diffuseFraction
                          RADB <- 0
                        }
                      } else {  # night time radiation values set to 0
                        RADG <- 0
                        RADD <- 0
                        RADB <- 0
                      }


                      # calculate fraction of solar radiation reflected from the water surface
                      # using Anderson (1954) as reported by Brown and Barnwell (1987) for QUAL2e
                      if (ALTDEG > 0) {
                        if (cloudfrac < 0.1) {
                          ALDO <- 1.18 * ALTDEG ^ -0.77
                        } else if (cloudfrac >= 0.1 & cloudfrac < 0.5) {
                          ALDO <- 2.2 * ALTDEG ^ -0.97
                        } else if (cloudfrac >= 0.5 & cloudfrac < 0.9) {
                          ALDO <- 0.95 * ALTDEG ^ -0.75
                        } else if (cloudfrac >= 0.9) {
                          ALDO <- 0.35 * ALTDEG ^ -0.45
                        } else {
                          ALDO <- 0
                        }
                      } else {
                        ALDO <- 1
                      }
                      if (ALDO < 0) {ALDO <- 0}
                      if (ALDO > 1) {ALDO <- 1}


                      # Calculate the total solar radiation before shade from topo and veg
                      Solar2 <- Jsnt_ccadj / (24 * 60)   # convert from cal/cm^2/day to cal/cm^2/min

                      # calculate beam (RADBSP) and diffuse (RADDSP) components of radiation
                      # to the stream surface after shading by vegetation and topography
                      # Direct beam radiation corrected for overhang and vegetation shade
                      RADBSP <- RADB * (1 - FRCSHD)   # units of cal/cm^2/day

                      # diffuse radiation from only sky opening angle
                      RADDSP <- RADD * SKOP   # units of cal/cm^2/day

                      # add diffuse radiation transmitted through the veg buffer according to the
                      # '1-SKOP and 1-the average density of the vegetation buffer that blocks
                      # beam radiation <- {1 - (VEGSHD/max(SHDWID_scb)) }
                      # browser()

                      if (SHDWID_scb[1] == -9999 | purrr::is_empty(SHDWID_scb)) {
                        RADDSP <- RADDSP
                      } else if(SHDWID_scb[NCB] != 0) {
                        RADDSP <-
                          RADDSP + (1 - SKOP) * (1 - (VEGSHD / SHDWID_scb[NCB]) ) * RADD
                      }


                      # Solar flux after shade from topo and veg and before reflection:
                      Solar3 <- (RADBSP + RADDSP) / (24 * 60)  # convert cal/cm^2/day to cal/cm^2/min


                      # calculate the cumulative total radiation entering the water (Solar4)
                      # after reflection from stream surface (albedo). See Chen et al. 1998
                      # page 312 EQ 21 for details
                      Solar4 <-   # units cal/cm^2/min
                        ( ((1 - ALDO) * RADBSP) + (0.91 * RADDSP) ) / (24 * 60)


                      # Entering Stream before reflection from surface W/m^2
                      tSolar_FLUX_inh2o[time_i] <-
                        Solar4 * time_interval * (4.184*100^2/86400)


                      # excluded reflection from Solar_FLUX_below
                      # Below Topo and Riparian Vegetation in W/m^2 in time_interval minutes
                      tSolar_FLUX_below[time_i] <-
                        Solar3 * time_interval * (4.184*100^2/86400)

                      # Before shade from topo and veg in W/m^2 in time_interval minutes
                      tSolar_FLUX_above[time_i] <-
                        Solar2 * time_interval * (4.184*100^2/86400)
                      # 'End - Calculate Solar FLUX



                    }  # end of within-day "time_i" iteration for-loop

                    # save the daily value to the ndaysSolar_FLUX vectors
                    ndaysSolar_FLUX_above[day_i] <- sum(tSolar_FLUX_above, na.rm = TRUE)
                    ndaysSolar_FLUX_below[day_i] <- sum(tSolar_FLUX_below, na.rm = TRUE)
                    ndaysSolar_FLUX_inh2o[day_i] <- sum(tSolar_FLUX_inh2o, na.rm = TRUE)


                  }  # end of "ndays" iteration for-loop

                  ndaysSolar_df <-
                    data.frame(
                      site_id = site_id,
                      Solar_FLUX_inh2o = mean(ndaysSolar_FLUX_inh2o, na.rm = TRUE),
                      Solar_FLUX_above = mean(ndaysSolar_FLUX_above, na.rm = TRUE),
                      Solar_FLUX_below = mean(ndaysSolar_FLUX_below, na.rm = TRUE)
                    )

                  site_solar_df <- bind_rows(site_solar_df, ndaysSolar_df)

                  message(paste("successful loop: ", site_i, sep = ""))
                }  # end of site iteration for-loop



                site_solar_out <- site_solar_df %>%
                  filter(!is.na(site_id)) %>%
                  mutate(effshd_TV = round(abs(Solar_FLUX_above-Solar_FLUX_below)/
                                             Solar_FLUX_above, 4)*100,
                         effshd_TVR = round(abs(Solar_FLUX_above-Solar_FLUX_inh2o)/
                                              Solar_FLUX_above, 4)*100) %>%
                  data.frame()

                site_solar_out
      }

    # stop the cluster
    parallel::stopCluster(cl)

    # calculate the time to run the function and print out when run completes
    end_time_running <- Sys.time()
    total_time_running <- round(end_time_running - start_time_running,2)

    if( profilingQ ) {
      print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      print(paste("     End at: ",end_time_running))
      print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      print(paste("    Elapsed: ",
                  total_time_running,
                  attr(total_time_running,"units")))
      print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    }

    return(site_df)
  }
