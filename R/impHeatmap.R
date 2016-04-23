#' @title Importance Heatmap
#' @description Plot heatmap of importance scores or ranks
#' 
#' @param rf an object inheriting from \code{link{randomForest}}.
#' @param ranks plot ranks instead of actual importance scores?
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_raster scale_fill_gradient2 aes_string labs theme element_text
#' @export
#' 
impHeatmap <- function(rf, ranks = TRUE) {
  imp <- data.frame(rf$importance, check.names = FALSE)
  classes <- levels(rf$y)
  pred.lvls <- rownames(imp)[order(imp[, "MeanDecreaseAccuracy"])]
  imp$predictor <- rownames(imp)
  if(ranks) for(x in classes) imp[[x]] <- rank(-imp[[x]])
  imp <- melt(imp[, c("predictor", classes)], id.vars = "predictor",
              variable.name = "class", value.name = "value")
  imp$class <- factor(imp$class, levels = levels(rf$y))
  imp$predictor <- factor(imp$predictor, levels = rev(pred.lvls))
  
  ggplot(imp, aes_string("class", "predictor")) +
    geom_raster(aes_string(fill = "value")) +
    scale_fill_gradient2(low = "green", mid = "yellow", high = "red",
                         midpoint = mean(range(imp$value))) +
    labs(x = "", y = "") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}