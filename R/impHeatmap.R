#' @title Importance Heatmap
#' @description Plot heatmap of importance scores or ranks from a 
#'   classification model
#' 
#' @param rf an object inheriting from \code{link{randomForest}}.
#' @param ranks plot ranks instead of actual importance scores?
#' @param plot print the plot?
#' @param xlab,ylab labels for the x and y axes.
#' 
#' @details \code{rf} must be a classification model run with 
#'   \code{importance = TRUE}.
#'   
#' @return the \code{ggplot} object is invisibly returned.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(mtcars)
#' rf <- randomForest(factor(am) ~ ., mtcars, importance = TRUE)
#' impHeatmap(rf, xlab = "Transmission", ylab = "Predictor")
#' 
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_raster scale_fill_gradient2 aes_string xlab ylab theme element_blank guide_colorbar
#' @export
#' 
impHeatmap <- function(rf, ranks = TRUE, plot = TRUE, xlab = NULL, ylab = NULL) {
  if(rf$type != "classification") stop("'rf' must be a classification model")
  if(is.null(rf$importanceSD)) stop("'rf' must be run with 'importance = TRUE'")
  
  # format importance data.frame
  imp <- data.frame(rf$importance, check.names = FALSE)
  classes <- levels(rf$y)
  imp$predictor <- rownames(imp)
  if(ranks) for(x in classes) imp[[x]] <- rank(-imp[[x]])
  imp <- melt(imp[, c("predictor", classes)], id.vars = "predictor",
              variable.name = "class", value.name = "value")
  imp$class <- factor(imp$class, levels = levels(rf$y))
  imp.val <- rf$importance[, "MeanDecreaseAccuracy"]
  imp$predictor <- factor(imp$predictor, levels = names(sort(imp.val)))

  g <- ggplot(imp, aes_string("class", "predictor")) +
    geom_raster(aes_string(fill = "value"))
  g <- g + if(ranks) {
    scale_fill_gradient2(
      "Rank", low = "#a50026", mid = "#ffffbf", high = "#313695",
       midpoint = mean(range(imp$value)), guide = guide_colorbar(reverse = TRUE)
    )
  } else {
    scale_fill_gradient2(
      "MeanDecreaseAccuracy", low = "#313695", mid = "#ffffbf", high = "#a50026",
      midpoint = mean(range(imp$value))
    )
  }
  g <- g + if(is.null(xlab)) theme(axis.title.x = element_blank()) else xlab(xlab)
  g <- g + if(is.null(ylab)) theme(axis.title.y = element_blank()) else ylab(ylab)
  
  if(plot) print(g)
  invisible(g)
}