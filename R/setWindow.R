#' Set the reference window of a spatial object.
#'
#' @param x the object for which to set a new reference window.
#' @param to [data.frame(1)][data.frame]\cr data.frame with columns \code{x} and
#'   \code{y} that contain the minimum and maximum values in the respective
#'   dimension to which the reference window shall be set, see Details.
#' @return The object \code{x} with an update reference window.
#' @family setters
#' @importFrom checkmate assertDataFrame assertNames
#' @importFrom tibble tibble
#' @name setWindow
#' @rdname setWindow
NULL

#' @rdname setWindow
#' @name setWindow
#' @export
setGeneric("setWindow", function(x, to) standardGeneric("setWindow"))


#' @rdname setWindow
#' @export
setMethod(f = "setWindow",
          signature = "geom",
          definition = function(x, to = NULL){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if("Extent" %in% class(to)){

              xVals <- c(to@xmin, to@xmax)
              yVals <- c(to@ymin, to@ymax)

            } else if(is.data.frame(to)){

              assertDataFrame(x = to, nrows = 2, min.cols = 2)
              assertNames(names(to), must.include = c("x", "y"))
              xVals <- c(min(to$x), max(to$x))

            } else if("bbox" %in% class(to)){

              names(to) <- tolower(names(to))
              assertNames(names(to), must.include = c("xmin", "xmax", "ymin", "ymax"))
              xVals <- c(to["xmin"], to["xmax"])
              yVals <- c(to["ymin"], to["ymax"])

            } else{

              stop("no suitable window provided.")

            }
            x@window <- tibble(x = c(min(xVals), max(xVals)),
                               y = c(min(yVals), max(yVals)))

            x@provenance <- c(getProvenance(x = x), list(paste0("the window was set to x[", paste(xVals, collapse = " "), "], y[", paste(yVals, collapse = " "), "]")))

            return(x)
          }
)
