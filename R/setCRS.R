#' Set (or transform) the coordinate reference system of a spatial object.
#'
#' @details In case an object does not yet have a coordinate reference system
#'   assigned, this function simply assigns it. In case the object has already a
#'   valid crs, a transformation to the new crs will be carried out. The
#'   transformation is computed for all classes with the standard defined in the
#'   \code{rgdal} package.
#' @param x the object for which to set the coordinate reference system.
#' @param crs [character(1)][character]\cr the coordinate reference system to
#'   set for this object.
#' @return The object \code{x} with an assigned or transformed coordinate
#'   reference system.
#' @family setters
#' @name setCRS
#' @rdname setCRS
NULL

#' @rdname setCRS
#' @name setCRS
#' @docType methods
#' @export
setGeneric("setCRS", function(x, crs) standardGeneric("setCRS"))


#' @rdname setCRS
#' @export
setMethod(f = "setCRS",
          signature = "geom",
          definition = function(x, crs = NULL){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(is.na(x@crs)){
              x@crs <- crs
            } else{
              theCoords <- x@geometry[which(names(x@geometry) %in% c("x", "y"))]
              if(!all(c("+proj=longlat", "+ellps=WGS84") %in% strsplit(x@crs, " ")[[1]])){
                # geographic <- project(as.matrix(theCoords), proj = as.character(x@crs), inv = TRUE)
              } else{
                geographic <- as.matrix(theCoords)
              }

              if(crs != "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"){
                # projected <- project(geographic, proj = as.character(crs))
              } else{
                projected <- geographic
              }
              x@geometry <- tibble(projected, x@geometry[which(!names(x@geometry) %in% c("x", "y"))])
              x@crs <- crs
              x <- setWindow(x = x, to = getExtent(x))
            }
            x@provenance <- c(getProvenance(x = x), list(paste0("the crs was set to '", crs, "'.")))

            return(x)
          }
)

#' @rdname setCRS
#' @export
setMethod(f = "setCRS",
          signature = signature("Spatial"),
          definition = function(x, crs = NULL){

            if (!requireNamespace("sp", quietly = TRUE)) {
              stop(
                "Package \"sp\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(is.na(x@proj4string)){
              x@proj4string <- raster::crs(crs)
            } else{
              x <- sp::spTransform(x, CRSobj = crs(crs))
            }
            return(x)
          }
)

#' @rdname setCRS
#' @export
setMethod(f = "setCRS",
          signature = "sf",
          definition = function(x, crs = NULL){

            if (!requireNamespace("sf", quietly = TRUE)) {
              stop(
                "Package \"sf\" must be installed to use this function.",
                call. = FALSE
              )
            }
            if(is.na(sf::st_crs(x = x)$proj4string)){
              x <- sf::st_set_crs(x = x, value = crs)
            } else{
              x <- sf::st_transform(x, crs = crs)
            }
            return(x)
          }
)

#' @rdname setCRS
#' @export
setMethod(f = "setCRS",
          signature = "Raster",
          definition = function(x, crs = NULL){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(is.na(x@crs)){
              x@crs <- raster::crs(crs)
            } else{
              x <- raster::projectRaster(from = x, crs = raster::crs(crs))
            }
            x@history <- c(getProvenance(x = x), list(paste0("the crs was set to '", crs, "'.")))

            return(x)
          }
)