#' Set a table of group attributes
#'
#' @param x the object to which to assign a new attribute table.
#' @param table [data.frame(1)][data.frame]\cr the new attribute table.
#' @return The object \code{x} with an updated group attribute table.
#' @family setters
#' @importFrom checkmate assertDataFrame
#' @importFrom tibble as_tibble
#' @name setGroups
#' @rdname setGroups
NULL

#' @rdname setGroups
#' @name setGroups
#' @export
setGeneric("setGroups", function(x, table) standardGeneric("setGroups"))


#' @rdname setGroups
#' @export
setMethod(f = "setGroups",
          signature = "geom",
          definition = function(x, table = NULL){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(!any(names(table) %in% c("gid"))){
              stop("'table' must contain the column 'gid'.")
            }

            if(x@type == "grid"){

            } else {
              theGroups <- getGroups(x = x)
              theGroups <- theGroups[c("gid")]

              outGroups <- merge(theGroups, table, all.y = TRUE)
              outGroups <- outGroups[c(c("gid"), names(outGroups)[!names(outGroups) %in% c("gid")])]

              if(any(colnames(outGroups) == "fid")){
                message("groups should not contain any column named 'fid', I am discarding the one you provided.")
                outGroups <- outGroups[,-which(colnames(outGroups) == "fid")]
              }

              x@data[[1]]$groups <- as_tibble(outGroups)
            }

            cln <- colnames(table)
            if(length(cln) > 1){
              hist <- paste0("the 'groups' attribute table was joined with the variables (", paste(cln, collapse = ", "), ")")
            } else {
              hist <- paste0("the 'groups' attribute table was joined with the variable ", cln)
            }

            x@provenance <- c(getProvenance(x = x), list(hist))

            return(x)
          }
)

#' @rdname setGroups
#' @export
setMethod(f = "setGroups",
          signature = "RasterLayer",
          definition = function(x, table = NULL){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            assertDataFrame(x = table)
            temp <- raster::ratify(x)
            nIDs <- length(temp@data@attributes[[1]][,1])
            stopifnot(dim(table)[1] == nIDs)
            temp@data@attributes <- list(table)

            return(temp)
          }
)
