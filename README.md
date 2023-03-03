# RShade-R

## Parameters for `method_chen98_parchunk_star()`

- site_data_frame: data frame that holds all the site details necessary for the inputs_fixed_star() function using the other input parameters to prepare a data frame for the actual shade calculations.

- site_id_chr: point or site ID value as text. Numbers can be used, but the format of the column must be set to character or text. 

- startdate: start date for integrating shade as a text string "YYYY-MM-DD". If calculating average shade across a month, use first of month as start date value here and use the total number of days in that month in the “ndays” input parameter

- ndays: number of days to integrate/simulate shade starting on 'startdate'. An ndays value of “1” calculates shade for only the start date noted in “startdate” input parameter. To simulate shade for the entire month of August 2012 enter 'startdate = "2012-08-01" ‘ and “ndays = 31” for 31 days in August.

- timezone: Time zone is the difference between UTC and the local standard time in hours. A map of world time zones may be found at: http://aa.usno.navy.mil/graphics/TimeZoneMap2001.pdf. PST is -8 for the Pacific Northwest, USA.

- dlstime: is a daylight savings time indicator variable where you Select '1' if you want times calculated and reported as local daylight savings time or "0" to report times in local standard time.

- VWidthGlobal: Each of the 9 riparian vegetation zones on each bank are assumed to have the same width (meters) as specified here. set to “NA” if zone widths are variable in width for the shad estimates.

- JsntMethod: Select the solar model to generate input radiation. options to select from are: 0="Bras" 3="ODEQ" 4="Ryan-Stolz"

- cloudfrac_global: global value for cloud fraction of total incoming energy that will be applied to all times. If continuous hourly values are entered in the site_data_frame, then they override this global value. Value must be between 0 and 1.

- vegzones: is the number of vegetation zones that are being applied in the shade calculations as text or a character value. For example, if the shade calculations are estimating shade using a 50m wide riparian vegetation buffer that has 10m wide zones to assess variation in riparian vegetation, then there will be five total vegetation zones in the shade calculations that are each 10m wide (vegzones = “5”). 

- star_veg: TRUE/FALSE input parameter. If TRUE, then use the 8-direction star sampling of vegetation height and canopy density for shade calculations. If FALSE, then use a 3-direction (East, South, West) sampling of vegetation height and canopy density.
ripgelev: TRUE/FALSE input parameter. If TRUE, the ground elevation (meters) of each vegetation zone has been entered into the “site_data_frame” with a column header including the “grndelevVZ” text for tracking. If FALSE, then the ground elevation for each vegetation zone is the sum of the channel elevation and the incision value at each site.

- nfac: Atmospheric turbidity factor for calculation of solar radiation that is used if the Bras option for the solar radiation model is selected. [2 = clear, 5 = smog]

- atc: Atmospheric transmission factor for calculation of solar radiation used if the Ryan-Stolzenbach option for 'JsntMethod' is selected.


----- 

## Create package file

- Go to the directory that contains the directory of this README. 
  (It the parent directory of the directory "EPAShader".)

- Run the shell command `R CMD build RShade`. For example:

```shell
RShade> cd ..
r-packages> R CMD build EPAShader
```

-----

## Installation

### From local directory 

```r
install.packages(<path/to/package/folder>, repos = NULL, type="source")
```


Of, if you have (and prefer) the package 
[`devtools`](https://www.r-project.org/nosvn/pandoc/devtools.html):

```r
devtools::install(<path/to/package/folder>)
```

### Using .tar.zip file
 
- Copy the tar/zip file "EPAShader_x.x.x.tar.zip".
   
   - If you do not have that zip file see above how to create it.

- Install the R package `RShade` using Terminal:

```shell
R CMD INSTALL RShade_x.x.x.tar.gz 
```


## References

[YDC1] Chen, Y.D. (1996). Hydrologic and water quality modeling for aquatic
ecosystem protection and restoration in forest watersheds: a case study
of stream temperature in the Upper Grande Ronde River, Oregon.
PhD dissertation. University of Georgia. Athens, GA.

[YDC2] Chen, Y.D., Carsel, R.F., McCutcheon, S.C., and Nutter, W.L. (1998).
"Stream temperature simulation of forested riparian areas: I. watershed-scale model development",
Journal of Environmental Engineering.
April 1998. pp 304-315.

[YDC3] Chen, Y.D., Carsel, R.F., McCutcheon, S.C., and Nutter, W.L. (1998).
"Stream temperature simulation of forested riparian areas: II. model application",
Journal of Environmental Engineering. April 1998. pp 316-328.
