\name{user.lambda4}
\alias{user.lambda4}
\title{Compute User Specified Lambda 4 (Split-Half)}
\usage{
  user.lambda4(x, split.method = "even.odd",
    item.stats = FALSE, missing = "complete")
}
\arguments{
  \item{x}{Can be either a data frame or a covariance
  matrix.}

  \item{split.method}{Specify method for splitting items.}

  \item{item.stats}{If TRUE then item statistics are
  provided in the output.}

  \item{missing}{How to handle missing values.}
}
\description{
  Compute User Specified Lambda 4 (Split-Half)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  Guttman L (1945). "A Basis for Analyzing Test-Retest
  Reliability." Psychometrika, 10, 255-282.
}

