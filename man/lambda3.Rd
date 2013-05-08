\name{lambda3}
\alias{lambda3}
\title{Compute Guttman's Lambda 3 Coefficient (Coefficent Alpha)}
\usage{
  lambda3(x, item.stats.max = 12, missing = "complete")
}
\arguments{
  \item{x}{Can be either a data matrix or a covariance
  matrix}

  \item{item.stats.max}{items statistics shown if the
  number of items are less than this value.}

  \item{missing}{how to handle missing values.}
}
\description{
  Often recognized as Cronbach's alpha, Guttman's Lambda 3
  can be used to estimate reliability when the data can be
  split in parallel forms.
}
\examples{
lambda3(Rosenberg)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  Cronbach L (1951). "Coefficient Alpha and the Internal
  Structure of Tests." Psychometrika, 16, 297-334. Guttman
  L (1945). "A Basis for Analyzing Test-Retest
  Reliability." Psychometrika, 10, 255-282.
}

