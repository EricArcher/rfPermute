% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classPriors.R
\name{classPriors}
\alias{classPriors}
\title{Class Priors}
\usage{
classPriors(x, sampsize)
}
\arguments{
\item{x}{a \code{rfPermute} or \code{randomForest} model object.}

\item{sampsize}{the vector of sample sizes used to construct the model. If 
provided, must have length equal to number of classes. If set to
\code{NULL}, priors will be computed assuming empirical sample sizes.}
}
\description{
Compute the class classification priors and class-specific 
  model binomial p-values using these priors as null hypotheses.
}
\examples{
library(randomForest)
data(mtcars)

# random sampling with replacement
rf <- randomForest(factor(am) ~ ., mtcars)
confusionMatrix(rf)
classPriors(rf, NULL)

# balanced design
sampsize <- balancedSampsize(mtcars$am)
rf <- randomForest(factor(am) ~ ., mtcars, replace = FALSE, sampsize = sampsize)
confusionMatrix(rf)
classPriors(rf, sampsize)

}
\seealso{
\code{\link{balancedSampsize}}, \code{\link{confusionMatrix}}
}
\author{
Eric Archer \email{eric.archer@noaa.gov}
}
