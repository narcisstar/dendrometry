% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrometry.R
\name{distanceH}
\alias{distanceH}
\title{Horizontal distance}
\usage{
distanceH(distance, angle, type = c("angle", "slope"),
          angleUnit = c("deg", "rad"))
}
\arguments{
\item{distance}{numeric vector of the distance measured on sloping area.}

\item{angle}{numeric vector of angle values.}

\item{type}{type of \code{angle}. Either \code{"angle"} or
\code{"slope"}. Default is \code{"slope"}.}

\item{angleUnit}{unit of \code{angle} measures when
\code{type = "angle"}. Either \code{"deg"} for degree or \code{"rad"} for
radian. Default is \code{"deg"}.}
}
\value{
A vector of horizontal distance.
}
\description{
Horizontal distance calculation for sloping area.
}
\examples{
distanceH(20, 30)
distanceH(20, angle = 30, type = "slope")
distanceH(20, angle = 25, type = "angle")
}
