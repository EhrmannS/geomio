#' Set the names of a spatial object.
#'
#' @param x the object for which to set a new name.
#' @param to [character(1)][character]\cr new name.
#' @return The object \code{x} with an update name.
#' @family setters
#' @importFrom checkmate assertCharacter
#' @importFrom tibble tibble
#' @name setNames
#' @rdname setNames
NULL

#' @rdname setNames
#' @name setNames
#' @export
setGeneric("setNames", function(x, to) standardGeneric("setNames"))


#' @rdname setNames
#' @export
setMethod(f = "setNames",
          signature = "geom",
          definition = function(x, to = NULL){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            assertCharacter(x = to, any.missing = FALSE, len = length(x@data))
            names(x@data) <- to

            return(x)
          }
)
