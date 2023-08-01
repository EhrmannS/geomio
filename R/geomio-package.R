#' geomio: geometry input/output
#'
#' The geomio package ...
#'
#' @author \strong{Maintainer, Author}: Steffen Ehrmann
#'   \email{steffen.ehrmann@posteo.de}
#'
#' @seealso \itemize{ \item Github project:
#'   \href{https://github.com/EhrmannS/geomio}{https://github.com/EhrmannS/geomio}
#'    \item Report bugs:
#'   \href{https://github.com/EhrmannS/geomio/issues}{https://github.com/EhrmannS/geomio/issues}
#'    }
#' @docType package
#' @name geomio
#' @useDynLib geomio, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

globalVariables(c("x", "y", "fid"))