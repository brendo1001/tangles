# comments 06/05/19: Tangles 0.8.1 ##

## Test environments
* local windows laptop, R 3.5.0
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.6.0


## R CMD check results
* 0 errors 
* 0 warnings
* 0 notes


## Downstream dependencies
There are currently no downstream dependencies for this package


# comments 10/05/19: Tangles 0.8.1 ##

## Test environments
* local windows laptop, R 3.5.0
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.6.0
* Unbuntu 18.04 LTS laptop, R 3.5.2


## R CMD check results
* 0 errors 
* 0 warnings
* 0 notes


## Downstream dependencies
There are currently no downstream dependencies for this package

## CRAN RESPONSE
Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). That is not allow by CRAN policies. 

Please only write/save files if the user has specified a directory in the function themselves!
In your examples/vignettes/tests you can write to tempdir().

The codelines in most of your examples are commented out.
Please NEVER do that! Ideally find toy examples that can be regularly executed and checked. Lengthy examples (> 5 sec), can be wrapped in \donttest{}.

Please fix and resubmit.




# comments 30/09/19: Tangles 0.8.1 ##

## Test environments
* local windows laptop, R 3.5.1
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.6.0
* Unbuntu 18.04 LTS laptop, R 3.5.2


## R CMD check results
* 0 errors 
* 0 warnings
* 0 notes


## Downstream dependencies
There are currently no downstream dependencies for this package