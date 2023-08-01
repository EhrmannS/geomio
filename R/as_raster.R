#' Transform a spatial object to class \code{Raster*}
#'
#' @param x the object to transform to class \code{Raster*}.
#' @param ... other arguments.
#' @return an object of class \code{Raster*}
#' @family spatial classes
#' @name as_raster
#' @rdname as_raster
NULL

#' @rdname as_raster
#' @name as_raster
#' @export
setGeneric("as_raster", function(x, ...) standardGeneric("as_raster"))


#' @rdname as_raster
#' @export
setMethod(f = "as_raster",
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

              if(is.data.frame(theGroups)){
                theGroups <- list(theGroups)
              }
              theRasters <- list()
              for(i in seq_along(theNames)){

                mat <- matrix(data = unlist(theFeatures["gid"], use.names = F),
                              nrow = x@point$y[2],
                              ncol = x@point$x[2], byrow = TRUE)
                out <- raster::raster(x = mat, crs = theCRS,
                                      xmn = x@point$x[1], xmx = x@point$x[1] + x@point$x[2]*x@point$x[3],
                                      ymn = x@point$y[1], ymx = x@point$y[1] + x@point$y[2]*x@point$y[3])

                if(any(names(theGroups[[i]]) != "gid")){
                  out <- raster::ratify(out)
                  out@data@attributes <- list(as.data.frame(theGroups[[i]]))
                }
                out <- setProvenance(x = out, to = paste0("raster '", theNames[i], "' was transformed from an object of class geom."))

                theRasters <- c(theRasters, stats::setNames(object = list(out), nm = theNames[i]))
              }

              if(length(theRasters) > 1){
                out <- raster::stack(theRasters)
              } else {
                out <- theRasters[[1]]
              }


            }

            return(out)
          }
)

