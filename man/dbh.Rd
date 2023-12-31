% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrometry.R
\name{dbh}
\alias{dbh}
\title{Diameter or DBH}
\usage{
dbh(circum)
}
\arguments{
\item{circum}{numeric vector of circumference.}
}
\value{
A vector of diameter or \code{DBH}.
}
\description{
Computes diameter based on circumference. If circumference (perimeter) at breast height is given, then Diameter at Breast Height (\code{DBH}) is obtained. Used in dendrometry for trees' \code{DBH} calculation.
}
\examples{
x = seq(1, 5, .4)
dbh(x)
}
\seealso{
\code{\link{height}} for tree  height, \code{\link{circum}} for diameter.
}
