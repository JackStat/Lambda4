\name{lambdas}
\alias{lambdas}
\title{Guttman's 6 Lambda Coefficients}
\usage{
  lambdas(x, missing = "complete", standardize = FALSE)
}
\arguments{
  \item{x}{Can be either a data matrix or a covariance
  matrix}

  \item{missing}{How to handle missing values.}

  \item{standardize}{When TRUE Results are standardized by
  using the correlation matrix instead of the covariance
  matrix for computation.}
}
\description{
  Guttman's 6 Lambda Coefficients
}
\examples{
lambdas(Rosenberg)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  Guttman L (1945). "A Basis for Analyzing Test-Retest
  Reliability." Psychometrika, 10, 255-282.
}

