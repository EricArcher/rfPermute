#' @title OOB Trace
#' @description Plot trace of cumulative OOB error rate by number of trees
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
#' @param plot display the plot?
#'   
#' @return the \code{ggplot2} object is invisibly returned.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(mtcars)
#' 
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' plotRFtrace(rf)
#' 
#' @importFrom plyr .
#' @importFrom magrittr %>% 
#' @importFrom rlang .data
#' @export
#'
plotRFtrace <- function(rf, plot = TRUE) {
  df <- rf$err.rate %>% 
    as.data.frame()
  
  p <- df %>% 
    dplyr::mutate(trees = 1:nrow(.)) %>% 
    tidyr::gather("class", "error", -.data$trees) %>% 
    dplyr::mutate(class = factor(.data$class, levels = colnames(df))) %>% 
    ggplot2::ggplot(ggplot2::aes_string("trees", "error", color = "class")) +
    ggplot2::geom_line() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  if(plot) print(p)
  invisible(p)
}