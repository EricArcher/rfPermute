#' \code{rfPermute} package
#' 
#' Random Forest Predictor Importance Significance and Model Diagnostics.
#' 
#' @aliases rfPermute-package
#' @docType package
#' @name rfPermute_package 
#' 
#' @importFrom randomForest randomForest
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom methods new
#' @keywords package
#' 
#' @export
rfPermuteTutorial <- function() {
  utils::browseURL(system.file("rfPermute_Tutorial.html", package = "rfPermute"))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to rfPermute v", utils::packageVersion("rfPermute"), "\n",
    "See rfPermuteTutorial() for a guide to the package."
  )
}
