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
#>  [1] "bart"                 "cubist"               "gam"                 
#>  [4] "glm"                  "glm_elastic"          "glm_lasso"           
#>  [7] "glm_ridge"            "gpr"                  "knn"                 
#> [10] "lmm"                  "mars"                 "mean"                
#> [13] "qreg"                 "qreg_q75"             "qreg_q90"            
#> [16] "qrf"                  "rebart"               "rf"                  
#> [19] "rf_deep"              "rf_shallow"           "rpart"               
#> [22] "rpart_deep"           "rpart_shallow"        "svr"                 
#> [25] "vfbart"               "wsbart"               "xbart"               
#> [28] "xgboost"              "xgboost_conservative" "xgboost_flexible"    
```
