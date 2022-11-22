# HW3
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/EdwardS2027/HW3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EdwardS2027/HW3/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

## Overview
'HW3' package is created for the purpose of copying the lm() and anova() functions, which can give us the fitted linear regression model and the results of hypothesis testing:
* 'lm2()' fits the linear regression model based on given formula and data, and returns the values similar to ones given from the 'summary()'
* 'anova2()' performs analysis of varaince on the fitted linear regression model for the purpose of hypothesis testing.


## Installation
You should have the 'devtools' package installed, before installing the 'HW3' package. 

``` r
install.packages('devtools')
```
You are now able to install the 'HW3' package by running this command
``` r
devtools::install_github("EdwardS2027/HW3")
```

To access this package, run
``` r
library("HW3")
```

## Usage
```{r, message = FALSE}
coeff = lm2(melb_data$Price~melb_data$YearBuilt+melb_data$Car+melb_data$Landsize,melb_data,na.action="omit")$coefficients
coeff
#>                         Estimate   Std. Error    t value      Pr(>|t|)
#>(Intercept)          1.321950e+07 3.513085e+05  37.629333 6.385300e-286
#>melb_data$YearBuilt -6.363473e+03 1.792960e+02 -35.491433 9.927155e-257
#>melb_data$Car        2.083608e+05 7.139330e+03  29.184915 3.312875e-178
#>melb_data$Landsize   3.487448e+01 6.725109e+00   5.185712  2.203520e-07

f = anova2(melb_data$Price~melb_data$YearBuilt+melb_data$Car+melb_data$Landsize,melb_data,na.action="omit")[,4]
f
#>melb_data$YearBuilt       melb_data$Car  melb_data$Landsize           Residuals 
#>         1051.06980           892.44127            26.89161                  NA 
```

## Contact
If you find any issue or a bug, please email 'eshao@umich.edu' with a minimal reproducible example of this particular issue or bug.
Footer
© 2022 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About
