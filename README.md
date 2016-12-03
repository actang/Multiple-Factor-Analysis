# Multiple-Factor-Analysis

By Saurabh Belsare, Ankush Desai, Jieun Kim and Allen Tang.

This is R package that implements Multiple Factor Analysis, which is introduced in *[Multiple factor analysis: principal component analysis for multitable and multiblock data sets](https://www.utdallas.edu/~herve/abdi-WiresCS-mfa-2013.pdf)* by Herve Abdi, Lynne Williams, and Domininique Valentin.

A brief introduction of the MFA and the package can be found in the repo.

To install the package in R, you can use the following codes:
```
install.packages("devtools")
library(devtools)
install_github("actang/Multiple-Factor-Analysis")
```

## Building the package:
```
library(devtools)
# creating documentation (i.e. the Rd files in man/)
devtools::document()
# checking documentation
devtools::check_man()
# run tests
devtools::test()
# checking documentation
devtools::build_vignettes()
# building tarball (e.g. oski_0.1.tar.gz)
devtools::build()
# checking install
devtools::install()
```

- For running the Shiny App, first make sure that you have built the package using devtools. After that one can launch the ShinApp by clicking on RunApp in the RStudio (code is in app.R)

### The presentation for the project is available at:
[http://rpubs.com/ankushdd/mfa](http://rpubs.com/ankushdd/mfa)

### The Shiny App is published at 
https://atang.shinyapps.io/multiple_factor_analysis/

### The vignette is available at [vignette](https://github.com/actang/Multiple-Factor-Analysis/blob/master/MFA/vignettes/mfa-vignette.Rmd?)


