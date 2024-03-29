% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{subNumNumCpp}
\alias{subNumNumCpp}
\title{Replace numeric values with other numeric values (c++)}
\usage{
subNumNumCpp(mat, replace, with)
}
\arguments{
\item{mat}{\link[=matrix]{matrix(numeric)}\cr the object in which to replaces
numeric values.}

\item{replace}{\link[=numeric]{numeric(.)}\cr values to replace.}

\item{with}{\link[=character]{character(.)}\cr values that replace the old values
(must have same length as \code{replace}).}
}
\value{
a numeric matrix where values in \code{replace} have been replaced
with values in \code{with}.
}
\description{
C++ function that replaces numeric values with other numeric values in a
matrix.
}
\seealso{
Other substitute functions: 
\code{\link{subChrIntCpp}()},
\code{\link{subNumChrCpp}()}
}
\concept{substitute functions}
