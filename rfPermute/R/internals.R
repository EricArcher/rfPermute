.scaleImp <- function(imp, impSD) {
  imp[, -ncol(imp)] <- imp[, -ncol(imp), drop = FALSE] / 
    ifelse(impSD < .Machine$double.eps, 1, impSD)
  return(imp)
}

.makeImpArray <- function(x, z.dim, z.dimnames) {
  array(
    x, dim = c(nrow(x), ncol(x), z.dim), 
    dimnames = list(rownames(x), colnames(x), z.dimnames)
  )
}

#' @importFrom swfscMisc pVal
.calcImpPval <- function(rp) {
  calcPval <- function(obs, null) {
    t(sapply(1:nrow(null), function(i) {
      sapply(1:ncol(null), function(j) pVal(obs[i, j], null[i, j, ]))
    }))
  }
  imp <- rp$importance
  impSD <- rp$importanceSD
  null.dist <- rp$null.dist
  rm(rp)
  arr <- .makeImpArray(calcPval(imp, null.dist$unscaled), 2, NULL)
  arr[, , 2] <- calcPval(.scaleImp(imp, impSD), null.dist$scaled)
  dimnames(arr) <- list(rownames(imp), colnames(imp), c("unscaled", "scaled"))
  return(arr)
}