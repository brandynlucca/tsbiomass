# List the Super Learner learners available in tsbiomass

Prints the learner method names available to the package's Super
Learners.

## Usage

``` r
list_learners()
```

## Value

A character vector of the available learner names.

## Examples

``` r
list_learners()
#>  [1] "bart"        "cubist"      "gam"         "glm"         "glm_elastic"
#>  [6] "glm_lasso"   "glm_ridge"   "gpr"         "knn"         "lmm"        
#> [11] "mars"        "mean"        "qreg"        "qrf"         "rebart"     
#> [16] "rf"          "rpart"       "svr"         "vfbart"      "wsbart"     
#> [21] "xbart"       "xgboost"    
```
