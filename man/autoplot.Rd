% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{autoplot}
\alias{autoplot}
\alias{autoplot.sfnetwork}
\title{Plot sfnetwork geometries with ggplot2}
\usage{
autoplot.sfnetwork(object, ...)
}
\arguments{
\item{object}{An object of class \code{\link{sfnetwork}}.}

\item{...}{Ignored.}
}
\value{
An object of class \code{\link[ggplot2]{ggplot}}.
}
\description{
Plot the geometries of an object of class \code{\link{sfnetwork}}
automatically as a \code{\link[ggplot2]{ggplot}} object. Use this method
without the .sfnetwork suffix and after loading the \code{ggplot2} package.
}
\details{
See \code{\link[ggplot2]{autoplot}}.
}
