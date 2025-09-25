
# lab4.LiUgroupwork

<!-- badges: start -->
[![R-CMD-check](https://github.com/ChenzhiNi315/lab4.LiUgroupwork/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ChenzhiNi315/lab4.LiUgroupwork/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of lab4.LiUgroupwork is to provide an R implementation of linear regression using QR decomposition for enhanced numerical stability. It includes detailed vignettes with tutorials and comparative examples. This package was developed as an assignment for the course 732A94 Advanced R Programming at Linköping University.

## Installation

You can install the development version of lab4.LiUgroupwork from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ChenzhiNi315/lab4.LiUgroupwork")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lab4.LiUgroupwork)

# Fit model on iris dataset
fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

# Methods
print(fit)
coef(fit)
head(pred(fit))
head(resid(fit))
summary(fit)
plot(fit)
