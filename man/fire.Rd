\encoding{UTF-8}
\name{fire}
\alias{fire}
\title{Danish Fire Insurance Claims Database}
\description{
  Danish Fire Insurance Claims Database includes 2167 industrial fire losses
  gathered from the Copenhagen Reinsurance Company over the period 1980–1990.
}
\usage{data(fire)}
\format{A dataframe with 2167 observations on five variables. The object
  is of class \code{data.frame}.}
\examples{
data(fire)
attach(fire)
plot(building, contents, pch = 20, xlim = c(0,95), ylim = c(0,133),
     xlab = "Loss of Building", ylab = "Loss of Contents",
     main = "Danish Fire Insurance Claims") 
}
\keyword{datasets}
