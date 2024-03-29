% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{diamondSquareCpp}
\alias{diamondSquareCpp}
\title{Generate a height map (c++)}
\usage{
diamondSquareCpp(mat, stepSize, roughness, startDev)
}
\arguments{
\item{mat}{\link[=matrix]{matrix(numeric)}\cr the template matrix into which
height values are written.}

\item{stepSize}{\link[=numeric]{numeric(.)}\cr ...}

\item{roughness}{\link[=numeric]{numeric(1)}\cr the Hurst exponent (fBm) or its
equivalent, the roughness factor (DSa). Bounded between 0 and 1.}

\item{startDev}{\link[=numeric]{numeric(1)}\cr ...}
}
\value{
A numeric matrix of the same dimensions as \code{mat} into which the
values of a height map have been written.
}
\description{
C++ function that generates a height map based on the diamond-square
algorithm
}
\seealso{
Other matrix modify functions: 
\code{\link{matDistanceCpp}()},
\code{\link{matReduceCpp}()}
}
\concept{matrix modify functions}
