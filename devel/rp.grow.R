#' @title Grow rfPermute Objects
#' @description Add additional trees to an \code{rfPermute} object.
#' 
#' @param x an object of class \code{rfPermute}
#' @param how.many number of trees to add.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @seealso \code{\link[randomForest]{grow}}
#' 
#' @examples
#' 
#' data(iris)
#' iris.rp <- randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE)
#' iris.rp <- grow(iris.rf, 50)
#' print(iris.rp)
#' 
#' layout(matrix(1:6, nrow = 2))
#' plotNull(rp.all) 
#' layout(matrix(1))
#' 
#' @importFrom abind abind
#' @importFrom randomForest grow
#' @export
#' 
rp.grow <- function(x, how.many, ...) {
  if(!inherit(x, "rfPermute")) stop("'x' is not an rfPermute object.")
  
  rf <- grow(x)
  
}