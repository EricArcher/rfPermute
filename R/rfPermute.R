#' @name rfPermute
#' @title Estimate Permutation p-values for Random Forest Importance Metrics
#' @description Estimate significance of importance metrics for a Random Forest 
#'   model by permuting the response variable. Produces null distribution of 
#'   importance metrics for each predictor variable and p-value of observed.
#'
#' @param x,y,formula,data,subset,na.action,\dots See \code{\link{randomForest}} 
#'   for definitions.
#' @param nrep Number of permutation replicates to run to construct 
#'   null distribution and calculate p-values (default = 100).
#' @param num.cores Number of CPUs to distribute permutation results over. 
#'   Defaults to \code{NULL} which uses one fewer than the number of cores 
#'   reported by \code{\link[parallel]{detectCores}}.
#'
#' @details All other parameters are as defined in \code{randomForest.formula}. 
#'   A Random Forest model is first created as normal to calculate the observed 
#'   values of variable importance. The response variable is then permuted 
#'   \code{nrep} times, with a new Random Forest model built for each 
#'   permutation step. 
#'
#' @return An \code{rfPermute} object which contains all of the components of a 
#'   \code{randomForest} object plus:
#'   \item{null.dist}{A list containing two three-dimensional arrays of null 
#'     distributions for \code{unscaled} and \code{scaled} importance measures.} 
#'   \item{pval}{A three dimensional array containing permutation p-values for 
#'     \code{unscaled} and \code{scaled} importance measures.}
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @keywords tree classif regression
#' 
#' @seealso 
#' \code{\link{plotNull}} for plotting null distributions from the \code{rfPermute} objects. \cr
#' \code{\link{importance}} for extracting importance measures. \cr
#' \code{\link{rp.combine}} for combining multiple \code{rfPermute} objects.\cr
#' \code{\link{proximityPlot}} for plotting case proximities.\cr
#' \code{\link{impHeatmap}} for plotting a heatmap of importance scores.\cr
#' \code{\link{randomForest}}
#'
#' @examples
#' # A regression model using the ozone example
#' data(airquality)
#' ozone.rfP <- rfPermute(
#'   Ozone ~ ., data = airquality, ntree = 100, 
#'   na.action = na.omit, nrep = 50, num.cores = 1
#' )
#'   
#' # Plot the null distributions and observed values.
#' plotNull(ozone.rfP) 
#'   
#' # Plot the unscaled importance distributions and highlight significant predictors
#' plotImportance(ozone.rfP, scale = FALSE)
#'   
#' # ... and the scaled measures
#' plotImportance(ozone.rfP, scale = TRUE)
#'
#' @importFrom magrittr %>% 
#' @importFrom rlang .data
#' @export
#' 
rfPermute <- function(x, ...) UseMethod("rfPermute")


#' @rdname rfPermute
#' 
#' @importFrom randomForest randomForest
#' @export
#' 
rfPermute.default <- function(x, y, ..., nrep = 100, num.cores = 1) {  
  orig.call <- match.call()
  orig.call$nrep <- NULL
  orig.call$num.cores <- NULL
  orig.call[[1]] <- as.name("randomForest")
  rf.call <- orig.call
  rf.call$x <- x
  rf.call$y <- y
  imp.element <- pmatch(names(rf.call), "importance")
  if(!all(is.na(imp.element))) {
    imp.id <- which(!is.na(imp.element))
    names(rf.call)[imp.id] <- "importance"
  }    
  rf.call$importance <- TRUE
  rf.call[-1] <- lapply(as.list(rf.call[-1]), eval, envir = parent.frame())
  rf <- eval(rf.call)
  rf$call <- orig.call
  
  # permutes 'y' in rf.call 'nrep' times and runs randomForest  
  if(nrep > 0) {
    # Setup number of cores
    if(is.null(num.cores)) num.cores <- parallel::detectCores() - 1
    if(is.na(num.cores)) num.cores <- 1
    num.cores <- max(1, num.cores)
    num.cores <- min(parallel::detectCores() - 1, num.cores)
    
    # Create list of permuted y values
    ran.y <- lapply(1:nrep, function(i) sample(rf.call$y))
    
    call.x <- x
    # Get importance scores for permutations
    #  a list of 3-dimensional arrays of importance scores
    cl <- swfscMisc::setupClusters(num.cores)
    null.dist <- tryCatch({
      if(is.null(cl)) { # Don't parallelize if num.cores == 1      
        lapply(ran.y, .permFunc, call.x = call.x, perm.rf.call = rf.call)
      } else { # Run random forest using parLapply
        parallel::clusterEvalQ(cl, require(randomForest))
        parallel::clusterExport(
          cl = cl, 
          varlist = c("call.x", "rf.call"), 
          envir = environment()
        )
        parallel::parLapplyLB(
          cl, ran.y, .permFunc, call.x = call.x, perm.rf.call = rf.call
        )
      }
    }, finally = if(!is.null(cl)) parallel::stopCluster(cl) else NULL)
    
    # create and load null distribution arrays for each scaled and unscaled importances
    rf$null.dist <- list(unscaled = .makeImpArray(rf$importance, nrep, NULL))
    rf$null.dist$scaled <- rf$null.dist$unscaled
    for(i in 1:nrep) {
      rf$null.dist$unscaled[, , i] <- null.dist[[i]][, , "unscaled"]
      rf$null.dist$scaled[, , i] <- null.dist[[i]][, , "scaled"]
    }
    
    # calculate p-value of observed importance metrics
    rf$pval <- .calcImpPval(rf) 
    
    class(rf) <- c("rfPermute", "randomForest")
  }
  
  return(rf)  
}


#' @rdname rfPermute
#' 
#' @export
#' 
rfPermute.formula <- function(formula, data = NULL, ..., subset, 
                              na.action = stats::na.fail, nrep = 100) {
  if (!inherits(formula, "formula")) stop("method is only for formula objects")
  m <- match.call(expand.dots = FALSE)
  if (any(c("xtest", "ytest") %in% names(m))) {
    stop("xtest/ytest not supported through the formula interface")
  }
  
  # extract formula terms
  names(m)[2] <- "formula"
  if (is.matrix(eval(m$data, parent.frame()))) m$data <- as.data.frame(data)
  m$... <- NULL
  m$nrep <- NULL
  m$na.action <- na.action
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame())
  y <- stats::model.response(m)
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
  m <- stats::model.frame(
    stats::terms(
      stats::reformulate(attributes(Terms)$term.labels)
    ), 
    data.frame(m)
  )
  for (i in seq(along = ncol(m))) {
    if (is.ordered(m[[i]])) m[[i]] <- as.numeric(m[[i]])
  }
  
  # run rfPermute
  rf.call <- match.call()
  rf.call[[1]] <- as.name("rfPermute")
  names(rf.call)[2:3] <- c("x", "y")
  rf.call$x <- m
  rf.call$y <- y
  rf.call$subset <- rf.call$na.action <- NULL
  rf.call[-1] <- lapply(rf.call[-1], eval, envir = parent.frame())
  rf <- eval(rf.call)
  
  # reconstitute original randomForest call
  rf.call <- match.call()
  rf.call[[1]] <- as.name("randomForest")
  rf$call <- rf.call
  rf$call$nrep <- NULL
  rf$terms <- Terms
  if (!is.null(attr(m, "na.action"))) rf$na.action <- attr(m, "na.action")
  
  class(rf) <- if(nrep > 0) {
    c("rfPermute", "randomForest.formula", "randomForest")
  } else {
    c("randomForest.formula", "randomForest")
  }
  
  return(rf)
}
