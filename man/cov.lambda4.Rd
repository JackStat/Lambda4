\name{cov.lambda4}
\alias{cov.lambda4}
\title{Compute Covariance Maximized Lambda4}
\usage{
  cov.lambda4(x, method = "Hunt", missing = "complete",
    show.lambda4s = FALSE, show.splits = FALSE,
    standardize = FALSE)
}
\arguments{
  \item{x}{Can be either a data matrix or a covariance
  matrix.}

  \item{missing}{How to handle missing values.}

  \item{show.lambda4s}{If TRUE then the estimates for each
  split are included in the output.}

  \item{show.splits}{If TRUE then a binary matrix is
  exported that describes the ways the items were split.}

  \item{standardize}{When TRUE results are standardized by
  using the correlation matrix instead of the covariance
  matrix for computation.}

  \item{method}{Can select either Hunt or Osburn.}
}
\description{
  Compute Covariance Maximized Lambda4
}
\examples{
cov.lambda4(Rosenberg, method="Hunt")
cov.lambda4(Rosenberg, method="Osburn")
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}

