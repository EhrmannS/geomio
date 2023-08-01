#' Get the coordinate reference system of a spatial object.
#'
#' @param x the object from which to extract the coordinate reference system.
#' @return The coordinate reference system of \code{x} given as proj4string.
#' @family getters
#' @name getCRS
#' @rdname getCRS
NULL

#' @rdname getCRS
#' @name getCRS
#' @export
setGeneric("getCRS", function(x) standardGeneric("getCRS"))


#' @rdname getCRS
#' @export
setMethod(f = "getCRS",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            x@crs
          }
)

#' @rdname getCRS
#' @export
setMethod(f = "getCRS",
          signature =  signature("Spatial"),
          definition = function(x){

            if (!requireNamespace("sp", quietly = TRUE)) {
              stop(
                "Package \"sp\" must be installed to use this function.",
                call. = FALSE
              )
            }

            as.character(x@proj4string)
          }
)

#' @rdname getCRS
#' @export
setMethod(f = "getCRS",
          signature = "sf",
          definition = function(x){

            if (!requireNamespace("sf", quietly = TRUE)) {
              stop(
                "Package \"sf\" must be installed to use this function.",
                call. = FALSE
              )
            }

            sf::st_crs(x)$proj4string
          }
)

#' @rdname getCRS
#' @export
setMethod(f = "getCRS",
          signature = 'Raster',
          definition = function(x){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            as.character(x@crs)
          }
)

#' @rdname getCRS
#' @export
setMethod(f = "getCRS",
          signature = 'SpatRaster',
          definition = function(x){

            if (!requireNamespace("terra", quietly = TRUE)) {
              stop(
                "Package \"terra\" must be installed to use this function.",
                call. = FALSE
              )
            }

            terra::crs(x, proj = TRUE)
          }
)