#' Get a specific layer of a spatial object.
#'
#' @param x the object from which to get the layer.
#' @return A list of the layers of \code{x}. Each list-item hast the result of
#'   getNames(x) as name.
#' @family getters
#' @importFrom methods new
#' @name getLayers
#' @rdname getLayers
NULL

#' @rdname getLayers
#' @name getLayers
#' @export
setGeneric("getLayers", function(x) standardGeneric("getLayers"))


#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            out <- NULL
            for(i in seq_along(x@data)){

              theData <- x@data[i]

              temp <- new(Class = "geom",
                          type = x@type,
                          geometry = x@geometry,
                          data = theData,
                          window = x@window,
                          crs = x@crs,
                          provenance = x@provenance)
              out <- c(out, list(temp))

            }


            # if(theType == "grid"){
            #   theFeatures <- getFeatures(x = x)
            #   theGroups <- getGroups(x = x)
            #   theNames <- getNames(x)
            #
            #   if(is.data.frame(theGroups)){
            #     theGroups <- list(theGroups)
            #   }
            #
            #   for(i in seq_along(theNames)){
            #
            #     tempFeatures <- tibble(values = as.vector(theFeatures[["gid"]]))
            #     tempGroups <- theGroups[[i]]
            #     tempName <- theNames[i]
            #
            #     temp <- new(Class = "geom",
            #                 type = x@type,
            #                 geometry = x@geometry,
            #                 data = ,
            #                 window = x@window,
            #                 crs = x@crs,
            #                 provenance = x@provenance)
            #     out <- c(out, list(temp))
            #   }
            #
            # } else {
            #
            # }

            return(out)
          }
)

#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "Spatial",
          definition = function(x){

            if (!requireNamespace("sp", quietly = TRUE)) {
              stop(
                "Package \"sp\" must be installed to use this function.",
                call. = FALSE
              )
            }

            list(x)
          }
)

#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "sf",
          definition = function(x){

            if (!requireNamespace("sf", quietly = TRUE)) {
              stop(
                "Package \"sf\" must be installed to use this function.",
                call. = FALSE
              )
            }

            # allNames <- names(x)
            # noGeom <- names(sf::st_drop_geometry(x))
            # geomName <- allNames[!allNames %in% noGeom]

            # out <- setNames(list(x), geomName)
            # out <-
            list(x)

            # return(out)
          }
)

#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "Raster",
          definition = function(x){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            # extract objects and assign history if that was set
            out <- lapply(1:dim(x)[3], function(y){
              t <- x[[y]]
              if(inherits(x = x, what = "RasterBrick") & length(t@data@attributes) != 0){
                t@data@attributes <- t@data@attributes[[1]]
              }
              return(t)
            })

            return(out)
          }
)

#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "SpatRaster",
          definition = function(x){

            if (!requireNamespace("terra", quietly = TRUE)) {
              stop(
                "Package \"terra\" must be installed to use this function.",
                call. = FALSE
              )
            }

            out <- lapply(1:dim(x)[3], function(y){
              t <- x[[y]]
              return(t)
            })

            return(out)
          }
)

#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "matrix",
          definition = function(x){

            list(x)

          }
)
