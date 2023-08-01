#' Set additional entries to the history of an object
#'
#' @param x the object for which to set the coordinate reference system.
#' @param to [character(1)][character]\cr a character string describing
#'   one step in the provenance for this object; repeat to set several steps.
#' @details Both, objects of class \code{geom} and \code{Raster*} have the slot
#'   \code{@history}, which contains the provenance of that object. With
#'   \code{setProvenance}, that provenance can be updated, based on the
#'   modification the object has been exposed to. This happens automatically for
#'   all geometry operations that come with \code{geometr}.
#' @return The object \code{x} where the history slot has been updated.
#' @family setters
#' @name setProvenance
#' @rdname setProvenance
NULL

#' @rdname setProvenance
#' @name setProvenance
#' @docType methods
#' @export
setGeneric("setProvenance", function(x, to) standardGeneric("setProvenance"))


#' @rdname setProvenance
#' @export
setMethod(f = "setProvenance",
          signature = "geom",
          definition = function(x, to = NULL){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(length(x@provenance) == 0){
              temp <- list(paste0("the object was loaded from memory"))
            } else {
              temp <- x@provenance
            }

            x@provenance <- c(temp, list(to))
            return(x)
          }
)

#' @rdname setProvenance
#' @export
setMethod(f = "setProvenance",
          signature = "RasterLayer",
          definition = function(x, to = NULL){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(length(x@history) == 0){
              temp <- list(paste0("the object was loaded from memory"))
            } else {
              temp <- x@history
            }

            x@history <- c(temp, list(to))
            return(x)
          }
)
