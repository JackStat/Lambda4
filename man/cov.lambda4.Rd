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
  This code estimates maximized lambda4, a split-half
  reliability estimate.  The function splits the halves by
  specifying a two column list of paired inter-item
  covariances in descending order. It then calculates
  Guttman's lambda4 on every possible split-half while
  preserving the inter-item pairings. The function then
  returns a list of the Lambda4s and then takes the
  minimum, maximum, median, and mean of the list.  This
  calculation is most appropiately applied to tests with
  multiple factors.
}
\examples{
cov.lambda4(Rosenberg, method="Hunt")
cov.lambda4(Rosenberg, method="Osburn")
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}

