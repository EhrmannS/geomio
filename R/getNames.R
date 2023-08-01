#' Get the name(s) of a spatial object.
#'
#' @param x the object from which to get the name.
#' @return A vector of the names of \code{x}.
#' @family getters
#' @name getNames
#' @rdname getNames
NULL

#' @rdname getNames
#' @name getNames
#' @export
setGeneric("getNames", function(x) standardGeneric("getNames"))


#' @rdname getNames
#' @export
setMethod(f = "getNames",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            names(x@data)
          }
)

#' @rdname getNames
#' @export
setMethod(f = "getNames",
          signature = "sf",
          definition = function(x){
            allNames <- names(x)
            noGeom <- names(sf::st_drop_geometry(x))
            out <- allNames[!allNames %in% noGeom]

            return(out)
          }
)

#' @rdname getNames
#' @export
setMethod(f = "getNames",
          signature = "Spatial",
          definition = function(x){

            if (!requireNamespace("sp", quietly = TRUE)) {
              stop(
                "Package \"sp\" must be installed to use this function.",
                call. = FALSE
              )
            }

            names(x)
          }
)

#' @rdname getNames
#' @export
setMethod(f = "getNames",
          signature = "Raster",
          definition = function(x){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            names(x)
          }
)

#' @rdname getNames
#' @export
setMethod(f = "getNames",
          signature = "SpatRaster",
          definition = function(x){

            if (!requireNamespace("terra", quietly = TRUE)) {
              stop(
                "Package \"terra\" must be installed to use this function.",
                call. = FALSE
              )
            }

            names(x)
          }
)
