\name{kristof}
\alias{kristof}
\title{Compute Kristof Coefficient}
\usage{
  kristof(x, split.method = "triplet",
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
  Compute Kristof Coefficient
}
\examples{
kristof(Rosenberg, split.method="triplet")
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}

