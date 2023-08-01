#' Transform a spatial object to class \code{SpatRaster}
#'
#' @param x the object to transform to class \code{SpatRaster}.
#' @param ... other arguments.
#' @return an object of class \code{SpatRaster}
#' @family spatial classes
#' @name as_terra
#' @rdname as_terra
NULL

#' @rdname as_terra
#' @name as_terra
#' @export
setGeneric("as_terra", function(x, ...) standardGeneric("as_terra"))


#' @rdname as_terra
#' @export
setMethod(f = "as_terra",
          signature = "geom",
          definition = function(x = NULL){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            featureType <- getType(x)

            if(!all(featureType %in% c("grid"))){
              stop("Only objects of type 'grid' can be transformed to a Raster*")
            } else {

              theFeatures <- getFeatures(x = x)
              theGroups <- getGroups(x = x)
              theNames <- getNames(x)
              theCRS <- getCRS(x = x)

              out <- terra::rast(ncols = x@point$x[2], nrows = x@point$y[2],
                                 xmin = x@point$x[1], xmax = x@point$x[1] + x@point$x[2]*x@point$x[3],
                                 ymin = x@point$y[1], ymax = x@point$y[1] + x@point$y[2]*x@point$y[3])
              out[] <- theFeatures[["gid"]]
              names(out) <- theNames

              if(any(names(theGroups) != "gid")){
                out <- terra::`levels<-`(x = out, value = as.data.frame(theGroups))
                # terra::levels(out) <- as.data.frame(theGroups)
              }

            }

            return(out)
          }
)
