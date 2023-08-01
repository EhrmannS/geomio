#' Get the spatial resolution of a spatial object.
#'
#' @param x the object from which to derive the resolution.
#' @return A vector of two values of the spatial resolution of \code{x} in x and
#'   y dimension.
#' @family getters
#' @name getRes
#' @rdname getRes
NULL

#' @rdname getRes
#' @name getRes
#' @export
setGeneric("getRes", function(x) standardGeneric("getRes"))


#' @rdname getRes
#' @export
setMethod(f = "getRes",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            temp <- x
            if(temp@type == "grid"){
              out <- c(temp@geometry$x[3], temp@geometry$y[3])
            } else {
              out <- NULL
            }

            return(out)
          }
)

#' @rdname getRes
#' @export
setMethod(f = "getRes",
          signature = "Raster",
          definition = function(x){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }
            temp <- raster::res(x)
            out <- c(temp[1], temp[2])

            return(out)
          }
)

#' @rdname getRes
#' @export
setMethod(f = "getRes",
          signature = "SpatRaster",
          definition = function(x){

            if (!requireNamespace("terra", quietly = TRUE)) {
              stop(
                "Package \"terra\" must be installed to use this function.",
                call. = FALSE
              )
            }

            temp <- terra::res(x)
            c(temp[1], temp[2])
          }
)

#' @rdname getRes
#' @export
setMethod(f = "getRes",
          signature = "matrix",
          definition = function(x){

            out <- c(1, 1)

            return(out)
          }
)
