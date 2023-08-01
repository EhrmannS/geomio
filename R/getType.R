#' Get the type of a spatial object.
#'
#' @param x the object for which to determine the type.
#' @return A vector of two values of the geometry type (point/line/polygon/grid)
#'   and the specific main type/class of \code{x}.
#' @family getters
#' @name getType
#' @rdname getType
NULL

#' @rdname getType
#' @name getType
#' @export
setGeneric("getType", function(x) standardGeneric("getType"))


#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            c(x@type, x@type)
          }
)

#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = signature("Spatial"),
          definition = function(x){

            if (!requireNamespace("sp", quietly = TRUE)) {
              stop(
                "Package \"sp\" must be installed to use this function.",
                call. = FALSE
              )
            }

            theType <- class(x)[1]
            if(theType %in% c("SpatialPoints", "SpatialPointsDataFrame", "SpatialMultiPoints", "SpatialMultiPointsDataFrame", "SpatialPixels", "SpatialPixelsDataFrame")){
              geomType <- "point"
            } else if(theType %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialGrid", "SpatialGridDataFrame")){
              geomType <- "polygon"
            } else if(theType %in% c("SpatialLines", "SpatialLinesDataFrame")){
              geomType <- "line"
            }

            c(geomType, theType)
          }
)

#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = "sf",
          definition = function(x){

            if (!requireNamespace("sf", quietly = TRUE)) {
              stop(
                "Package \"sf\" must be installed to use this function.",
                call. = FALSE
              )
            }

            theType <- unique(as.character(sf::st_geometry_type(x)))
            if(theType %in% c("POINT", "MULTIPOINT")){
              geomType <- "point"
            } else if(theType %in% c("POLYGON", "MULTIPOLYGON")){
              geomType <- "polygon"
            } else if(theType %in% c("LINESTRING", "MULTILINESTRING")){
              geomType <- "line"
            }

            c(geomType, theType)
          }
)

#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = "Raster",
          definition = function(x){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            c("grid", class(x)[1])
          }
)

#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = "SpatRaster",
          definition = function(x){

            if (!requireNamespace("terra", quietly = TRUE)) {
              stop(
                "Package \"terra\" must be installed to use this function.",
                call. = FALSE
              )
            }

            c("grid", class(x)[1])
          }
)

#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = "matrix",
          definition = function(x){

            c("grid", class(x)[1])

          }
)