![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rfPermute)
[![Travis-CI Build Status](https://travis-ci.org/EricArcher/rfPermute.svg?branch=master)](https://travis-ci.org/EricArcher/rfPermute)
[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/github/EricArcher/rfPermute?branch=master&svg=true)](https://ci.appveyor.com/api/projects/status/github/EricArcher/rfPermute?branch=master&svg=true)  
![](http://cranlogs.r-pkg.org/badges/last-day/rfPermute?color=red)
![](http://cranlogs.r-pkg.org/badges/last-week/rfPermute?color=red)
![](http://cranlogs.r-pkg.org/badges/rfPermute?color=red)
![](http://cranlogs.r-pkg.org/badges/grand-total/rfPermute?color=red)  
[![DOI](https://zenodo.org/badge/23926/EricArcher/rfPermute.svg)](https://zenodo.org/badge/latestdoi/23926/EricArcher/rfPermute)
# rfPermute

## Description

`rfPermute` estimates the significance of importance metrics for a Random Forest model by permuting the response variable. It will produce null distributions of importance metrics for each predictor variable and p-value of observed. The package also includes several summary and 
visualization functions for `randomForest` and `rfPermute` results.

## Installation

To install the stable version from CRAN:

```r
install.packages('rfPermute')
```

To install the latest version from GitHub:

```r
# make sure you have Rtools installed
if (!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('EricArcher/rfPermute')
```

## Contact

* submit suggestions and bug-reports: <https://github.com/ericarcher/rfPermute/issues>
* send a pull request: <https://github.com/ericarcher/rfPermute/>
* e-mail: <eric.archer@noaa.gov>

## Current Functions

`classConfInt` Classification Confidence Intervals  
`clean.rf.data` Clean Random Forest Input Data  
`confusionMatrix` Confusion Matrix  
`exptd.err.rate` Expected Error Rate  
`impHeatmap` Importance Heatmap  
`pctCorrect` Percent Correctly Classified  
`plot.rfPermute` Plot Random Forest Importance Null Distributions  
`plot.rp.importance` Plot Random Forest Importance Distributions  
`plotVotes` Vote Distribution  
`proximity.plot` Plot Random Forest Proximity Scores  
`rfPermute` Estimate Permutation p-values for Random Forest Importance Metrics  
`rp.combine` Combine rfPermute Objects  
`rp.importance` Extract rfPermute Importance Scores and p-values  

## version 2.1.2

* Added `type` argument to `plotVotes` to choose between area and bar charts.

## version 2.1.1

* Added `n` argument to `impHeatmap`.
* Added functions: `classConfInt`, `confusionMatrix`, `plotVotes`, `pctCorrect`.

## version 2.0.1

* Fixed bug in `plot.rfPermute` that was reporting the p-value incorrectly at the top of the figure.
* Fixed multi-threading in `rfPermute` so it works on Windows too.
* Added `impHeatmap` function.
* Switched `proximity.plot` to use `ggplot2` graphics.

## version 2.0

* Fixed bug with calculation of p-values not respecting importance measure scaling (division by standard deviations). New format of output of `rfPemute` has separate `$null.dist` and `$pval` elements, each with results for unscaled and scaled importance mesures. See `?rfPermute` for more information.
* `rp.importance` and `plot.rfPermute` now take a `scale` argument to specify whether or not importance values should be scaled by standard deviations.
* If `nrep = 0` for `rfPermute`, a `randomForest` object is returned.

## version 1.9.3

* Fixed import declarations to avoid `grid` name clashes.
* Fixed logic error in `clean.rf.data` where fixed predictors were not removed.
* Fixed error in use of `main` argument in `plot.rp.importance`.

## version 1.9.2

* Added this NEWS.md
* Added README.md
* Added `num.cores` argument to `rfPermute` to take advantage of multi-threading 

## version 1.9.1

* Added internal keyword to `calc.imp.pval` to keep it from indexing
* Updated imports to match new CRAN policies