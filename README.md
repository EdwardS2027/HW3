# HW3
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/EdwardS2027/HW3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EdwardS2027/HW3/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

## Overview
'HW3' package is created for the purpose of copying the lm() and anova() functions, which can give us the fitted linear regression model and the results of hypothesis testing:
* 'lm2()' fits the linear regression model based on given formula and data, and returns the linear regression model and the results of hypothesis testing.
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
data <- read.csv("data/melb_data.csv")
attach(data)
library("HW3")
lm2(Price~Rooms+I(Method)+YearBuilt+Car+Landsize,data,Price>10000,na.action="omit",intercept=TRUE)
anova2(Price~Rooms+I(Method)+YearBuilt+Car+Landsize,data,Price>10000,na.action="omit",intercept=TRUE)
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
