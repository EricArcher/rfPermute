# rfPermute 2.0

## Changes

* Fixed bug with calculation of p-values not respecting importance measure scaling (division by standard deviations). New format of output has a $null.dist and separate $pval elements, with results for unscaled and scaled importance mesures. See ?rfPermute for more information.
* `rp.importance` and `plot.rfPermute` now take a `scale` argument to specify whether or not importance values should be scaled by standard deviations.
* If `nrep = 0` for `rfPermute`, a `randomForest` object is returned.


# rfPermute 1.9.3

## Additions

## Bug Fixes

* Fixed import declarations to avoid grid name clashes
* Fixed logic error in clean.rf.data where fixed predictors were not removed
* Fixed error in use of main argument in plot.rp.importance


# rfPermute 1.9.2

## Additions

* Added this NEWS.md
* Added README.md
* Added num.cores argument to rfPermute to take advantage of multi-threading 

## Changes

* Added internal keyword to calc.imp.pval to keep it from indexing
* Updated imports to match new CRAN policies

## Bug Fixes
