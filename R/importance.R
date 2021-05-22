#' @title Extract rfPermute Importance Scores and p-values.
#' @description Extract a matrix of the observed importance scores
#'   and p-values from the object produced by a call to \code{rfPermute}
#' 
#' @param x An object produced by a call to \code{rfPermute}.
#' @param scale For permutation based measures, should the measures be divided 
#'   their "standard errors"?
#' @param sort.by character vector giving the importance metric(s) or p-values 
#'   to sort by. If \code{NULL}, defaults to \code{"MeanDecreaseAccuracy"} for 
#'   classification models and \code{"\%IncMSE"} for regression models.
#' @param decreasing logical. Should the sort order be increasing or decreasing?
#' @param alpha Critical alpha to identify "significant" predictors.
#' @param sig.only Plot only the significant (<= \code{alpha}) predictors?
#' @param type character vector listing which importance measures to plot.
#'   Can be class names or names of overall importance measures 
#'   (e.g., "MeanDecreaseAccuracy").
#' @param n Plot \code{n} most important predictors.
#' @param main Main title for plot.
#' @param ... optional arguments which will be ignored.
#' 
#' @details p-values can be given to the \code{sort.by} argument by adding 
#'   '.pval' to the column name of the desired column from the \code{importance} 
#'   element of the \code{rfPermute} object.
#'   
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(airquality)
#' 
#' # A regression model using the ozone example
#' ozone.rfP <- rfPermute(
#'   Ozone ~ ., data = airquality, ntree = 100, 
#'   na.action = na.omit, nrep = 50, num.cores = 1
#' )
#' imp.unscaled <- importance(ozone.rfP, scale = TRUE)
#' imp.unscaled
#'   
#' imp.scaled <- importance(ozone.rfP, scale = TRUE)
#' imp.scaled
#' 
#' # plot scaled importance scores
#' plotImportance(ozone.rfP, scale = TRUE)
#' 
#' # plot unscaled and only significant scores
#' plotImportance(ozone.rfP, scale = FALSE, sig.only = TRUE)
#' 
#' @export
#' 
importance <- function(x, ...)  UseMethod("importance")

#' @rdname importance
#' @export
importance.default <- function(x, ...) {
  if(inherits(x, "randomForest")) {
    randomForest::importance(x, ...)
  } else {
    stop("No method implemented for this class of object")
  }
}

#' @rdname importance
#' @export
importance.rfPermute <- function(x, scale = TRUE, sort.by = NULL, 
                                 decreasing = TRUE, ...) {  
  #if(!inherits(x, "rfPermute")) stop("'x' is not of class 'rfPermute'")
  if((!is.character(sort.by) & !is.vector(sort.by)) & !is.null(sort.by)) {
    stop("'sort.by' is not a character vector")
  }
  
  imp <- if(scale) {
    .scaleImp(x$importance, x$importanceSD)
  } else {
    x$importance
  }

  pval <- x$pval[, , if(scale) "scaled" else "unscaled"]
  colnames(pval) <- paste(colnames(pval), ".pval", sep = "")
  pred <- rownames(imp)
  vals <- do.call(cbind, lapply(1:ncol(imp), function(i) {
    cbind(imp[pred, i, drop = FALSE], pval[pred, i, drop = FALSE])
  }))
  
  if(is.null(sort.by)) {
    sort.by <- ifelse(
      x$type == "regression", "%IncMSE", "MeanDecreaseAccuracy"
    )
  }
  not.found <- sort.by[!(sort.by %in% colnames(vals))]
  if(length(not.found) > 0) {
    not.found <- paste(not.found, collapse = ", ")
    stop(paste("sort.by: ", not.found, " not found", sep = ""))
  }
  
  order.list <- lapply(sort.by, function(i) vals[, i, drop = FALSE])
  order.list <- c(order.list, decreasing = decreasing)
  vals[do.call(order, order.list), , drop = FALSE]
}


#' @rdname importance
#' 
#' @export
#' 
plotImportance <- function(x, scale = TRUE, alpha = 0.05, sig.only = FALSE, 
                           type = NULL, n = NULL, main = NULL) { 
  x <- importance(x, scale = scale)
  cols <- if(is.null(type)) {
    lapply(seq(1, ncol(x), 2), function(i) c(i, i + 1))
  } else {
    type <- unique(gsub(".pval", "", type))
    not.found <- setdiff(type, colnames(x))
    if(length(not.found) > 0) {
      not.found <- paste(not.found, collapse = ", ")
      stop(paste(
        "the following columns in 'type' can't be found in 'x':", 
        not.found
      ))
    }
    lapply(match(type, colnames(x)), function(i) c(i, i + 1))
  }
  
  imp.list <- lapply(cols, function(i) {
    imp.df <- as.data.frame(x[, i])
    colnames(imp.df) <- c("imp", "pval")
    imp.df$pred <- rownames(imp.df)
    imp.df$is.sig <- factor(imp.df$pval <= alpha)
    imp.df <- imp.df[order(imp.df$imp), ]
    rownames(imp.df) <- 1:nrow(imp.df)
    imp.df <- imp.df[order(imp.df$imp, decreasing = TRUE), ]
    if(sig.only) imp.df <- imp.df[as.logical(imp.df$is.sig), ]
    if(!is.null(n) & is.numeric(n)) imp.df <- imp.df[1:min(n, nrow(imp.df)), ]
    imp.df$pred <- stats::reorder(imp.df$pred, imp.df$imp)
    
    ggplot2::ggplot(
      imp.df, 
      ggplot2::aes_string(x = "pred", y = "imp", fill = "is.sig") 
    ) + 
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() + 
      ggplot2::ggtitle(colnames(x)[i]) + 
      ggplot2::scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
      ggplot2::theme(
        legend.position = "none",
        axis.title = ggplot2::element_blank()
      )
  })
  imp.list$top <- main
  imp.list$bottom <- "Importance"
  suppressWarnings(do.call(gridExtra::grid.arrange, imp.list))
}

