#' Get the reference window of a spatial object.
#'
#' The extent of an area that encompasses an object, which is at least the
#' objects extent, or larger.
#' @param x the object from which to derive the reference window.
#' @details Calling \code{getWindow} on spatial classes that do not have a
#'   window attribute returns the same value as \code{getExtent}.
#' @return A tibble of the corner coordinates of the reference window of
#'   \code{x}. This table has two columns (x and y) and two rows (minimum and
#'   maximum).
#' @family getters
#' @importFrom tibble tibble as_tibble
#' @name getWindow
#' @rdname getWindow
NULL

#' @rdname getWindow
#' @name getWindow
#' @export
setGeneric("getWindow", function(x) standardGeneric("getWindow"))


#' @rdname getWindow
#' @export
setMethod(f = "getWindow",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            as_tibble(x@window)
          }
)

#' @rdname getWindow
#' @export
setMethod(f = "getWindow",
          signature = signature("Spatial"),
          definition = function(x){

            ext <- raster::extent(x)
            tibble(x = c(ext@xmin, ext@xmax),
                   y = c(ext@ymin, ext@ymax))

          }
)

#' @rdname getWindow
#' @export
setMethod(f = "getWindow",
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

#' @rdname getWindow
#' @export
setMethod(f = "getWindow",
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

#' @rdname getWindow
#' @export
setMethod(f = "getWindow",
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

#' @rdname getWindow
#' @export
setMethod(f = "getWindow",
          signature = "matrix",
          definition = function(x){

            tibble(x = c(0, ncol(x)),
                   y = c(0, nrow(x)))

          }
)