% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrometry.R
\name{slope2angle}
\alias{slope2angle}
\title{Slope to angle}
\usage{
slope2angle(slope, angleUnit = c("deg", "rad"))
}
\arguments{
\item{slope}{numeric vector of slope to be converted to angle.}

\item{angleUnit}{the desired unit for the returned angle value. Either \code{"deg"} or \code{"rad"}. Default is \code{"deg"}.}
}
\value{
A vector of angle values in specified unit.
}
\description{
Converts slope values to angle values.
}
\examples{
slope2angle(100)
slope2angle(17.6327)
slope2angle(angle2slope(30))
}
\seealso{
\code{\link{angle2slope}}, the reciprocal of \code{slope2angle}
}
