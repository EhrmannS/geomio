#' Get the number of columns of a spatial object.
#'
#' @param x the object from which to get the number of columns.
#' @return An integer of the number of columns.
#' @family getters
#' @name getCols
#' @rdname getCols
NULL

#' @rdname getCols
#' @name getCols
#' @export
setGeneric("getCols", function(x) standardGeneric("getCols"))


#' @rdname getCols
#' @export
setMethod(f = "getCols",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(x@type == "grid"){
              out <- x@geometry$x[2]
            } else {
              out <- NULL
            }

            return(out)
          }
)

#' @rdname getCols
#' @export
setMethod(f = "getCols",
          signature = "Raster",
          definition = function(x){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            raster::ncol(x)
          }
)

#' @rdname getCols
#' @export
setMethod(f = "getCols",
          signature = "SpatRaster",
          definition = function(x){

            if (!requireNamespace("terra", quietly = TRUE)) {
              stop(
                "Package \"terra\" must be installed to use this function.",
                call. = FALSE
              )
            }

            terra::ncol(x)
          }
)

#' @rdname getCols
#' @export
setMethod(f = "getCols",
          signature = "matrix",
          definition = function(x){

            ncol(x)
          }
)
