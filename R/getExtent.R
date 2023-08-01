#' Get the extent (bounding box) of a spatial object.
#'
#' @param x the object from which to derive the extent.
#' @return A tibble of the lower left and upper right corner coordinates of the
#'   extent of \code{x}. This table two columns (x and y) and two rows (minimum
#'   and maximum).
#' @family getters
#' @importFrom tibble tibble
#' @name getExtent
#' @rdname getExtent
NULL

#' @rdname getExtent
#' @name getExtent
#' @export
setGeneric("getExtent", function(x) standardGeneric("getExtent"))


#' @rdname getExtent
#' @export
setMethod(f = "getExtent",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(x@type == "grid"){
              temp <- x
              out <- tibble(x = c(temp@geometry$x[1], temp@geometry$x[1] + temp@geometry$x[2]*temp@geometry$x[3]),
                            y = c(temp@geometry$y[1], temp@geometry$y[1] + temp@geometry$y[2]*temp@geometry$y[3]))
            } else {
              thePoints <- getPoints(x = x)
              out <- tibble(x = c(min(thePoints$x), max(thePoints$x)),
                            y = c(min(thePoints$y), max(thePoints$y)))
            }

            return(out)
          }
)

#' @rdname getExtent
#' @export
setMethod(f = "getExtent",
          signature = signature("Spatial"),
          definition = function(x){

            if (!requireNamespace("sp", quietly = TRUE)) {
              stop(
                "Package \"sp\" must be installed to use this function.",
                call. = FALSE
              )
            }

            ext <- raster::extent(x)
            tibble(x = c(ext@xmin, ext@xmax),
                   y = c(ext@ymin, ext@ymax))
          }
)

#' @rdname getExtent
#' @export
setMethod(f = "getExtent",
          signature = "sf",
          definition = function(x){

            if (!requireNamespace("sf", quietly = TRUE)) {
              stop(
                "Package \"sf\" must be installed to use this function.",
                call. = FALSE
              )
            }

            ext <- sf::st_bbox(x)
            tibble(x = c(ext[[1]], ext[[3]]),
                   y = c(ext[[2]], ext[[4]]))
          }
)

#' @rdname getExtent
#' @export
setMethod(f = "getExtent",
          signature = "Raster",
          definition = function(x){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            ext <- raster::extent(x)
            tibble(x = c(ext@xmin, ext@xmax),
                   y = c(ext@ymin, ext@ymax))
          }
)

#' @rdname getExtent
#' @export
setMethod(f = "getExtent",
          signature = "SpatRaster",
          definition = function(x){

            if (!requireNamespace("terra", quietly = TRUE)) {
              stop(
                "Package \"terra\" must be installed to use this function.",
                call. = FALSE
              )
            }

            ext <- terra::ext(x)
            tibble(x = c(ext[1], ext[2]),
                   y = c(ext[3], ext[4]))

          }
)

#' @rdname getExtent
#' @export
setMethod(f = "getExtent",
          signature = "matrix",
          definition = function(x){

            tibble(x = c(0, ncol(x)),
                   y = c(0, nrow(x)))

          }
)
