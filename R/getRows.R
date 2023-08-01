#' Get the number of rows of a spatial object.
#'
#' @param x the object from which to get the number of rows.
#' @return An integer of the number of rows.
#' @family getters
#' @name getRows
#' @rdname getRows
NULL

#' @rdname getRows
#' @name getRows
#' @export
setGeneric("getRows", function(x) standardGeneric("getRows"))


#' @rdname getRows
#' @export
setMethod(f = "getRows",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(x@type == "grid"){
              out <- x@geometry$y[2]
            } else {
              out <- NULL
            }

            return(out)
          }
)

#' @rdname getRows
#' @export
setMethod(f = "getRows",
          signature = "Raster",
          definition = function(x){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            raster::nrow(x)
          }
)

#' @rdname getRows
#' @export
setMethod(f = "getRows",
          signature = "SpatRaster",
          definition = function(x){

            if (!requireNamespace("terra", quietly = TRUE)) {
              stop(
                "Package \"terra\" must be installed to use this function.",
                call. = FALSE
              )
            }

            terra::nrow(x)
          }
)

#' @rdname getRows
#' @export
setMethod(f = "getRows",
          signature = "matrix",
          definition = function(x){

            out <- nrow(x)

            return(out)
          }
)
