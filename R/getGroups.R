#' Get the table of group attributes
#'
#' @param x the object from which to derive the attribute table.
#' @details This table contains at least the column 'gid'.
#'   When this function is called on "ANY" object, it is first tested whether
#'   that object has features (\code{\link{getFeatures}}), from which the groups
#'   can be reconstructed. If this is not the case, \code{NULL} is returned.
#' @return A tibble (or a list of tibbles per layer) of the group attributes of
#'   \code{x}.
#' @family getters
#' @importFrom tibble tibble as_tibble
#' @name getGroups
#' @rdname getGroups
NULL

#' @rdname getGroups
#' @name getGroups
#' @export
setGeneric("getGroups", function(x) standardGeneric("getGroups"))


#' @rdname getGroups
#' @export
setMethod(f = "getGroups",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(x@type == "grid"){


              out <- list()
              for(i in seq_along(x@data)){

                theGroups <- x@data[[i]]$groups
                theName <- names(x@data[[i]]$groups)[1]

                out <- c(out, stats::setNames(object = list(theGroups), nm = theName))
              }

            } else {
              out <- x@data[[1]]$group
            }

            return(out)
          }
)

#' @rdname getGroups
#' @export
setMethod(f = "getGroups",
          signature = "Raster",
          definition = function(x){

            if(inherits(x = x, what = "RasterBrick")){

              if (!requireNamespace("raster", quietly = TRUE)) {
                stop(
                  "Package \"raster\" must be installed to use this function.",
                  call. = FALSE
                )
              }

              tab <- x@data@attributes

              out <- lapply(seq_along(tab), function(y){
                t <- tab[[y]]
                if(length(t) == 0){
                  tibble(gid = integer())
                } else {
                  names <- colnames(t[[1]])
                  names[which(names == "id")] <- "gid"
                  temp <- as_tibble(t[[1]])
                  names(temp) <- names
                  temp
                }
              })
              names(out) <- names(x)

            } else {

              out <- NULL
              for(i in 1:dim(x)[3]){
                temp <- x[[i]]@data@attributes
                if(length(temp) != 0){
                  names <- names(temp[[1]])
                  names[which(names == "id")] <- "gid"
                  tab <- as_tibble(temp[[1]])
                  names(tab) <- names
                } else {
                  tab <- tibble(gid = integer())
                  names(tab) <- names(x)
                }
                if(dim(x)[3] == 1){
                  out <- tab
                } else {
                  out <- c(out, stats::setNames(object = list(tab), nm = names(x)[i]))
                }
              }

            }
            return(out)
          }
)

#' @rdname getGroups
#' @export
setMethod(f = "getGroups",
          signature = "SpatRaster",
          definition = function(x){

            if (!requireNamespace("terra", quietly = TRUE)) {
              stop(
                "Package \"terra\" must be installed to use this function.",
                call. = FALSE
              )
            }

            temp <- terra::cats(x)

            out <- NULL
            for(i in seq_along(temp)){

              tab <- temp[[i]]
              tab <- as_tibble(tab)
              tab <- tab[!is.na(tab[[names(x)[1]]]),]
              names <- names(tab)
              names[which(names == "ID")] <- "gid"
              names(tab) <- names

              if(dim(x)[3] == 1){
                out <- tab
              } else {
                out <- c(out, stats::setNames(object = list(tab), nm = names(x)[i]))
              }

            }
            return(out)

          }
)