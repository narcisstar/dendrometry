% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrometry.R
\name{spNmReduce}
\alias{spNmReduce}
\title{To abbreviate  species name from 'Gender epithet' to 'G. epithet'}
\usage{
spNmReduce(name, sep = " ")
}
\arguments{
\item{name}{a factor coerceable vector of species name in forms 'Gender epithet'.}

\item{sep}{character string which separates Gender and epithet. Default is space " ".}
}
\value{
A factor vector of species reduced names in forms 'G. epithet'.
}
\description{
To abbreviate  species name from 'Gender epithet' to 'G. epithet'.
Useful in plots with species names.
}
\details{
Returned reduced names are made unique.
}
\seealso{
\code{make.cepnames} in \code{vegan} package.
}
