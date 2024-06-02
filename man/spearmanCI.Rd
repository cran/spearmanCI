\encoding{UTF-8}
\name{spearmanCI}
\alias{spearmanCI}
\alias{spearmanCI.default}
\title{Jackknife Euclidean / Empirical Likelihood Inference for Spearman's Correlation}
\description{
  Computes jackknife Euclidean / empirical likelihood confidence
  intervals for Spearman's correlation.
}
\arguments{
  \item{x}{vector with data.}
  \item{y}{vector with data.}
  \item{level}{the confidence level required.}
  \item{method}{this must be one of the strings \code{"Euclidean"} or
    \code{"empirical"}; see references below for details.}
  \item{plot}{logical; if \code{TRUE}, it plots log-likelihood ratio function.}
}
\usage{
spearmanCI(x, y, level = 0.95, method = "Euclidean", plot = FALSE)
}
\references{
  de Carvalho, M. and Marques, F. J. (2012). Jackknife Euclidean
  likelihood-based inference for Spearman's rho. \emph{North American
    Actuarial Journal}, \bold{16}, 487--492.

  Wang, R., and Peng, L. (2011). Jackknife empirical likelihood
  intervals for Spearman’s rho. \emph{North American Actuarial Journal},
  \bold{15}, 475--486.
}
\examples{
## Real data example
data(fire)
attach(fire)
spearmanCI(building, contents)

## The intervals in de Carvalho and Marques (2012, Section 3.2)
## differ slightly as they are based on the estimate 
## spearman <- function(x, y) {
##  n <- length(x)
##  F <- ecdf(x); G <- ecdf(y)
##  return(12 / n * sum((F(x) - 1 / 2) * (G(y) - 1 / 2)))  
## }

## Simulated data example
library(MASS)
pearson <- .7
Sigma <- matrix(c(1, pearson, pearson, 1), 2, 2)
xy <- mvrnorm(n = 1000, rep(0, 2), Sigma)
spearmanCI(xy[, 1], xy[, 2])
abline(v = 6 / pi * asin(pearson / 2), col = "grey", lty = 3)
}

\keyword{spearman}
\author{Miguel de Carvalho}
