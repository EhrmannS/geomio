#' geomio: geometry input/output
#'
#' The geomio package ...
#' geomio fills this gap by providing tools
#' \itemize{
#'   \item that produce an identical output for the same metadata of different
#'         classes (via so-called getters) and
#'   \item that use an identical input to write to various classes that
#'         originally require different input (via so-called setters).
#' }
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