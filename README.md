
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

library(lab4.LiUgroupwork)

# Load sample data
data(mtcars)

# Fit linear regression model
model <- linreg(mpg ~ wt + hp, data = mtcars)

# View basic model information
print(model)
# Call:
# linreg(formula = mpg ~ wt + hp, data = mtcars)
# 
# Coefficients:
# (Intercept)          wt          hp  
#    37.22727    -3.87783    -0.03177

# Get detailed summary
summary(model)
# Call:
# linreg(formula = mpg ~ wt + hp, data = mtcars)
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.22727    1.59879  23.285  < 2e-16 ***
# wt          -3.87783    0.63273  -6.129 1.12e-06 ***
# hp          -0.03177    0.00903  -3.519  0.00145 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.593 on 29 degrees of freedom

# Extract coefficients
coef(model)
# (Intercept)          wt          hp 
# 37.22727012 -3.87783074 -0.03177295 

# Get predictions
pred_values <- pred(model)
head(pred_values)
# [1] 22.59375 22.59375 25.71978 21.25124 18.96613 18.14525

# Get residuals
residuals <- resid(model)
head(residuals)
# [1] -0.5937491 -0.5937491 -2.7197835 -0.2512355  2.0338663  1.8547542

# Create diagnostic plots
plot(model)
## basic example code
```

