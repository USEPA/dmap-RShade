# RShade-R

## In brief

EPA shading modeling R-package.

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
