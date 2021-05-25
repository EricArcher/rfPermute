#' @title Plot inbag or OOB distribution
#' @description Plot distributions of the fraction of trees that samples were
#'   inbag or out-of-bag (OOB) in the Random Forest model.
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
#' @param type plot the frequency samples were inbag (\code{"inbag"}) or
#'   out-of-bag (\code{"oob"}).
#' @param sampsize optional vector of sample sizes used in \code{rf} model.
#' @param bins number of bins in histogram.
#' @param plot display the plot?
#'   
#' @return the \code{ggplot2} object is invisibly returned. If \code{sampsize} 
#'   is not \code{NULL} and \code{rf} contains a classification model, the plot 
#'   will display red vertical lines at the expected values for the classes 
#'   based on their frequency and sample sizes.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' library(randomForest)
#' data(mtcars)
#' 
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' plotBag(rf)
#' 
#' @export
#'
plotBag <- function(rf, type = c("inbag", "oob"), sampsize = NULL, 
                    bins = NULL, plot = TRUE) {
  type <- match.arg(type)
  pct <- rf$oob.times / rf$ntree
  label <- "inbag"
  if(type == "oob") {
    pct <- 1 - pct
    label <- "OOB"
  }
  if(is.null(bins)) bins <- max(3, floor(length(rf$oob.times) / 10))
  p <- ggplot2::ggplot(data.frame(pct = pct), ggplot2::aes_string("pct")) +
    ggplot2::geom_histogram(bins = bins) +
    ggplot2::labs(
      x = paste("Percent of trees where sample was", label),
      y = "Frequency"
    )
  if(!is.null(sampsize) & rf$type == "classification") {
    exp.pct <- as.vector(sampsize / table(rf$y)) / rf$ntree
    if(type == "oob") exp.pct <- 1 - exp.pct
    p <- p +
      ggplot2::geom_vline(xintercept = exp.pct, color = "red")
  }
  
  if(plot) print(p)
  invisible(p)
}