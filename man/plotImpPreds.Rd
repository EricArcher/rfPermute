% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotImpPreds.R
\name{plotImpPreds}
\alias{plotImpPreds}
\title{Plot Important Predictor Distribution}
\usage{
plotImpPreds(
  x,
  df,
  class.col,
  imp.type = NULL,
  max.vars = 16,
  scale = TRUE,
  size = 1,
  point.alpha = 0.2,
  violin.alpha = 0.5,
  plot = TRUE
)
}
\arguments{
\item{x}{a \code{rfPermute} or \code{randomForest} model object.}

\item{df}{data.frame with predictors in \code{rf} model.}

\item{class.col}{response column name in \code{df}.}

\item{imp.type}{character string representing importance type to use for 
sorting predictors.}

\item{max.vars}{number of variables to plot (from most important to least).}

\item{scale}{For permutation based importance measures, should they be divided 
their "standard errors"?}

\item{size, point.alpha, violin.alpha}{controls size of points and alpha 
values (transparency) for points and violin plots.}

\item{plot}{display the plot?}
}
\value{
the \code{ggplot2} object is invisibly returned.
}
\description{
For classification models, plot distribution of predictor 
  variables on classes sorted by order of importance in model.
}
\note{
If the model in \code{x} is from \code{randomForest} and was run 
  with \code{importance = TRUE}, then 'MeanDecreaseAccuracy' is used as 
  the default importance measure for sorting. Otherwise, 'MeanDecreaseGini' 
  is used.
}
\examples{
library(randomForest)
data(mtcars)

df <- mtcars
df$am <- factor(df$am)

rf <- randomForest(am ~ ., df, importance = TRUE)
plotImpPreds(rf, df, "am")

}
\author{
Eric Archer \email{eric.archer@noaa.gov}
}
