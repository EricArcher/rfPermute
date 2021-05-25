#' @title Combine rfPermute objects
#' @description Combines two or more ensembles of \code{rfPermute} objects into 
#'   one, combining \code{randomForest} results, null distributions, 
#'   and re-calculating p-values.
#' 
#' @param \dots two or more objects of class \code{rfPermute}, to be combined 
#'   into one.
#' 
#' @note This function has the same name as \code{randomForest::combine} 
#'   so it might be masked by that function if \code{randomForest} is loaded 
#'   after \code{rfPermute}. If all objects in \code{...} are pure 
#'   \code{randomForest} models, this function will just run 
#'   \code{randomForest::combine} and return the results.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @seealso \code{\link[randomForest]{combine}}
#' 
#' @examples
#' data(iris)
#' rp1 <- rfPermute(
#'   Species ~ ., iris, ntree = 50, norm.votes = FALSE, nrep = 100, num.cores = 1
#' )
#' rp2 <- rfPermute(
#'   Species ~ ., iris, ntree = 50, norm.votes = FALSE, nrep = 100, num.cores = 1
#' )
#' rp3 <- rfPermute(
#'   Species ~ ., iris, ntree = 50, norm.votes = FALSE, nrep = 100, num.cores = 1
#' )
#' rp.all <- combine(rp1, rp2, rp3)
#' rp.all
#' 
#' plotNull(rp.all) 
#' 
#' @export
#' 
combine <- function(...) {
  rp.list <- list(...)
  
  are.rf <- sapply(rp.list, function(x) inherits(x, "randomForest"))
  if(all(are.rf)) return(do.call(randomForest::combine, rp.list))
  
  are.rp <- sapply(rp.list, function(x) inherits(x, "rfPermute"))
  if(any(!are.rp)) stop("some objects in '...' are not rfPermute models.")
  
  rf <- do.call(randomForest::combine, lapply(rp.list, function(x) x$rf))
  null.dist <- sapply(c("unscaled", "scaled"), function(sc) {
    do.call(
      abind::abind, 
      c(lapply(rp.list, function(x) x$null.dist[[sc]]), along = 3)
    )
  }, simplify = FALSE)
  
  result <- list(
    rf = rf, 
    null.dist = null.dist,
    pval = .calcImpPval(rf, null.dist), 
    nrep = sum(sapply(rp.list, function(x) x$nrep)), 
    start = NULL,
    end = NULL
  )
  class(result) <- "rfPermute"
  result
}