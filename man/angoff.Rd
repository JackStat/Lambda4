\name{angoff}
\alias{angoff}
\title{Compute Angoff Coefficient}
\usage{
  angoff(x, split.method = "even.odd",
    missing = "complete", standardize = FALSE)
}
\arguments{
  \item{x}{Can be either a data matrix or a covariance
  matrix}

  \item{split.method}{Specify method for splitting items?}

  \item{missing}{How to handle missing values.}

  \item{standardize}{When TRUE Results are standardized by
  using the correlation matrix instead of the covariance
  matrix for computation.}
}
\description{
  Compute Angoff Coefficient
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}

