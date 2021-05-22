#' @title Classification Confidence Intervals
#' @description Calculate confidence intervals for Random Forest classifications
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
#' @param conf.level confidence level for the \code{\link{binom.test}} confidence interval
#' @param threshold threshold to test observed classification probability against.
#' 
#' @return A matrix with the following columns for each class and overall:
#' \describe{
#'   \item{pct.correct}{percent correctly classified}
#'   \item{LCI_##, UCI_##}{the lower and upper central confidence intervals given \code{conf.level}}
#'   \item{Pr.gt_##}{the probability that the true classification probability is >= \code{threshold}}
#' }
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov} 
#' 
#' @examples
#' library(randomForest)
#' data(symb.metab)
#'
#' rf <- randomForest(type ~ ., symb.metab)
#' classConfInt(rf)
#' 
#' @export
#' 
classConfInt <- function(rf, conf.level = 0.95, threshold = NULL) {
  conf <- .confMat(rf)
  result <- t(sapply(1:nrow(conf), function(i) {
    correct <- conf[i, i]
    n <- sum(conf[i, ])
    ci <- stats::binom.test(correct, n, correct / n, conf.level = conf.level)$conf.int
    names(ci) <- paste(c("LCI", "UCI"), conf.level, sep = "_")
    prob.gt <- if(!is.null(threshold)) {
      stats::pbinom(correct, n, threshold)
    } else NULL
    if(!is.null(prob.gt)) names(prob.gt) <- paste("Pr.gt_", threshold, sep = "")
    c(pct.correct = correct / n, ci, prob.gt)
  }))
  rownames(result) <- rownames(conf)
  n <- sum(conf)
  correct <- sum(diag(conf))
  ci <- stats::binom.test(correct, n, correct / n, conf.level = conf.level)$conf.int
  prob.gt <- if(!is.null(threshold)) {
    stats::pbinom(correct, n, threshold)
  } else NULL
  rbind(result, Overall = c(correct / n, ci, prob.gt))
}
