#' Set a table of feature attributes
#'
#' @param x the object to which to assign a new attribute table.
#' @param table [data.frame(1)][data.frame]\cr the attribute table.
#' @return The object \code{x} with an updated feature attribute table.
#' @family setters
#' @importFrom checkmate assertDataFrame assertTRUE
#' @importFrom tibble as_tibble
#' @name setFeatures
#' @rdname setFeatures
NULL

#' @rdname setFeatures
#' @name setFeatures
#' @export
setGeneric("setFeatures", function(x, table) standardGeneric("setFeatures"))


#' @rdname setFeatures
#' @export
setMethod(f = "setFeatures",
          signature = "geom",
          definition = function(x, table = NULL){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(!any(colnames(table) %in% "fid")){
              stop("'table' must contain the column 'fid'.")
            }

            if(x@type == "grid"){

            } else {

              theFeatures <- getFeatures(x = x)
              theGroups <- getGroups(x = x)
              if(any(colnames(table) %in% "gid")){
                theFeatures <- theFeatures[,-which(colnames(theFeatures) == "gid")]

                outGroups <- theGroups[theGroups$gid %in% table$gid,]

                x@data[[1]]$groups <- outGroups
              }
              outFeatures <- merge(x = theFeatures, y = table, all.x = TRUE)
              outFeatures <- outFeatures[c(c("fid", "gid"), names(outFeatures)[!names(outFeatures) %in% c("fid", "gid")])]

              x@data[[1]]$features <- as_tibble(outFeatures)

            }

            cln <- colnames(table)
            if(length(cln) > 1){
              hist <- paste0("the 'features' attribute table was joined with the variables (", paste(cln, collapse = ", "), ")")
            } else {
              hist <- paste0("the 'features' attribute table was joined with the variable ", cln)
            }

            x@provenance <- c(getProvenance(x = x), list(hist))

            return(x)
          }
)

#' @rdname setFeatures
#' @export
setMethod(f = "setFeatures",
          signature = "Spatial",
          definition = function(x, table = NULL){

            if (!requireNamespace("sp", quietly = TRUE)) {
              stop(
                "Package \"sp\" must be installed to use this function.",
                call. = FALSE
              )
            }

            assertDataFrame(x = table)

            if(grepl("DataFrame", class(x))){
              if(any(colnames(table) %in% colnames(x@data))){
                x@data <- merge(x@data, table, all.x = TRUE)
              } else{
                x@data <- as_tibble(cbind(x@data, table))
              }
              out <- x
            } else{
              if(inherits(x, "SpatialPixels")){
                out <- sp::SpatialPixelsDataFrame(points = x, data = table)
              } else if(inherits(x, "SpatialPoints")){
                out <- sp::SpatialPointsDataFrame(coords = x, data = table)
              } else if(inherits(x, "SpatialMultiPoints")){
                out <- sp::SpatialMultiPointsDataFrame(coords = x, data = table)
              } else if(inherits(x, "SpatialLines")){
                out <- sp::SpatialLinesDataFrame(sl = x, data = table, match.ID = FALSE)
              } else if(inherits(x, "SpatialPolygons")){
                out <- sp::SpatialPolygonsDataFrame(Sr = x, data = table)
              }
            }

            return(out)
          }
)

#' @rdname setFeatures
#' @export
setMethod(f = "setFeatures",
          signature = signature("sf"),
          definition = function(x, table = NULL){

            if (!requireNamespace("sf", quietly = TRUE)) {
              stop(
                "Package \"sf\" must be installed to use this function.",
                call. = FALSE
              )
            }

            assertDataFrame(table)
            assertTRUE(nrow(x) == nrow(table))

            if(any(colnames(table) %in% colnames(x))){
              out <- merge(x = x, y = table, all.x = TRUE)
            } else{
              out <- as_tibble(cbind(x, table))
            }
            return(out)
          }
)
