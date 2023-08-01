#' Get the history of a spatial object.
#'
#' @param x the object from which to derive the history.
#' @return A list of the events that lead to \code{x}.
#' @family getters
#' @name getProvenance
#' @rdname getProvenance
NULL

#' @rdname getProvenance
#' @name getProvenance
#' @export
setGeneric("getProvenance", function(x) standardGeneric("getProvenance"))


#' @rdname getProvenance
#' @export
setMethod(f = "getProvenance",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            hist <- x@provenance
            if(length(hist) == 0){
              hist <- list("the object was loaded from memory")
            }

            return(hist)
          }
)

#' @rdname getProvenance
#' @export
setMethod(f = "getProvenance",
          signature = "Raster",
          definition = function(x){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(inherits(x, "RasterStack")){
              hist <- list()
              for(i in 1:dim(x)[3]){
                hist <- c(hist, x@history)
              }
            } else {
              hist <- x@history
            }
            if(length(hist) == 0){
              hist <- list("the object was loaded from memory")
            }

            return(hist)
          }
)
