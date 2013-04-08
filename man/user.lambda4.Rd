\name{user.lambda4}
\alias{user.lambda4}
\title{Compute User Specified Lambda 4 (Split-Half)}
\usage{
  user.lambda4(x, split.method = "even.odd",
    bootstrap = FALSE, B = 1000, show.boots = FALSE,
    item.stats = FALSE, missing = "complete")
}
\arguments{
  \item{x}{Can be either a data frame or a covariance
  matrix.}

  \item{split.method}{Specify method for splitting items.}

  \item{bootstrap}{Can bootstrap for standard errors.}

  \item{B}{Specify how many bootstraps.}

  \item{show.boots}{Specify if you want to the bootstraps
  included in the output.}

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

