#' @title Confusion Matrix
#' @description Generate a confusion matrix for Random Forest analyses with 
#'   error rates translated into percent correctly classified, and columns for 
#'   confidence intervals and expected classification rates (priors) added.
#' 
#' @param x a \code{rfPermute} or \code{randomForest} model object.
#' @param conf.level confidence level for the \code{\link{binom.test}} confidence interval
#' @param threshold threshold to test observed classification probability against.
#' @param title a title for the plot.
#' @param plot display the plot?
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov} 
#' 
#' @seealso \code{\link{plotConfMat}}
#' 
#' @examples
#' library(randomForest)
#' data(mtcars)
#'
#' rf <- randomForest(factor(am) ~ ., mtcars, importance = TRUE)
#' confusionMatrix(rf)
#' 
#' @export
#' 
confusionMatrix <- function(x, conf.level = 0.95, threshold = NULL) {
  rf <- as.randomForest(x)
  if(rf$type != "classification") stop("'x' must be of a classification model")
  
  # Confusion matrix and counts
  cm <- .confMat(rf)
  class.n <- colSums(cm)
  all.n <- c(class.n, Overall = sum(class.n))
  total.n <- sum(class.n)
  correct.n <- diag(cm)
  correct.n <- c(correct.n, Overall = sum(correct.n))
  
  # Confidence intervals
  ci <- t(sapply(
    mapply(
      stats::binom.test, 
      x = correct.n, 
      n = all.n, 
      p = correct.n / all.n, 
      SIMPLIFY = FALSE
    ),
    function(x) x$conf.int
  ))
  colnames(ci) <- paste(c("LCI", "UCI"), conf.level, sep = "_")
  
  # Expected error rates (priors)
  exp.err <- 1 - class.n / total.n
  prior <- c(exp.err, Overall = sum(exp.err * class.n) / total.n)
  
  # Probability threshold
  prob.gt <- NULL
  if(!is.null(threshold)) {
    prob.gt <- lapply(threshold, function(p) {
      stats::pbinom(correct.n, total.n, p) * 100
    })
    prob.gt <- do.call(cbind, prob.gt)
    colnames(prob.gt) <- paste0("Pr.gt_", threshold)
  }
  
  cbind(
    rbind(cm, Overall = rep(NA, ncol(cm))), 
    pct.correct = (correct.n / all.n) * 100,
    ci * 100, 
    prior = (1 - prior) * 100, 
    class_p.value = 1 - stats::pbinom(correct.n, all.n, 1 - prior),
    prob.gt
  )
}


#' @rdname confusionMatrix
#' @export
#' 
plotConfMat <- function(x, title = NULL, plot = TRUE) {
  rf <- as.randomForest(x)
  if(rf$type == "regression") stop("'rf' must be of a classification model")
  
  conf <- .confMat(rf)
  pct.correct <- (100 * sum(diag(conf)) / sum(conf)) %>% 
    round(0) %>% 
    paste0("% correct")
  title <- if(is.null(title)) {
    pct.correct 
  } else {
    paste0(title, " (", pct.correct, ")")
  }
  freq <- rowSums(conf)
  rownames(conf) <- paste0(names(freq), " (", freq, ")")
  
  p <- conf %>% 
    prop.table(1) %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("observed") %>% 
    tidyr::gather("predicted", "prop", -.data$observed) %>% 
    dplyr::mutate(
      observed = factor(.data$observed),
      observed = stats::reorder(.data$observed, dplyr::desc(.data$observed)),
      predicted = factor(.data$predicted)
    ) %>% 
    ggplot2::ggplot(ggplot2::aes_string("predicted", "observed")) +
    ggplot2::geom_raster(ggplot2::aes_string(fill = "prop")) +
    ggplot2::scale_fill_viridis_c(
      option = "magma", 
      direction = -1, 
      limits = c(0, 1)
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = "Predicted", y = "True", title = title) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(title = "Proportion")) +
    ggplot2::theme(
      axis.text.x.top = ggplot2::element_text(angle = 45, hjust = 0),
      panel.background = ggplot2::element_blank()
    )
  
  if(plot) print(p)
  invisible(p)
}
