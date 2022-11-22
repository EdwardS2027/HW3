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
options(max.print=20)
library("HW3")
lm2(melb_data$Price~melb_data$Rooms+I(melb_data$Method)+melb_data$YearBuilt+melb_data$Car+melb_data$Landsize,melb_data,na.action="omit")
#>$coefficients
#>                           Estimate   Std. Error     t value      Pr(>|t|)
#>(Intercept)           10634919.4547 3.153142e+05  33.7280028 1.380171e-233
#> [ reached getOption("max.print") -- omitted 8 rows ]
#>
#>$residuals
#>[1]  25393.29 241544.85 345065.46 603797.19 441990.60
#> [ reached getOption("max.print") -- omitted 8169 entries ]
#>
#>$sigma
#>[1] 527274.2
#>
#>$r.squared
#>[1] 0.374664
#>
#>$adj.r.squared
#>[1] 0.3740513

anova2(melb_data$Price~melb_data$Rooms+I(melb_data$Method)+melb_data$YearBuilt+melb_data$Car+melb_data$Landsize,melb_data,na.action="omit")
#>                        Df       Sum Sq      Mean Sq     F value        Pr(>F)
#>melb_data$Rooms          1 1.011350e+15 1.011350e+15 3637.711826  0.000000e+00
#> [ reached getOption("max.print") -- omitted 8 rows ]
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
