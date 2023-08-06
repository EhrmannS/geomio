#' Transform a spatial object to class \code{geom}
#'
#' @param x the object to transform to class \code{geom}.
#' @param ... other arguments.
#' @param group [logical(1)][logical]\cr should the values of a Raster* or the
#'   attributes of MULTI* features be grouped, i.e., should the unique values be
#'   assigned into the groups table (\code{TRUE})? The default behaviour for
#'   Raster* would be not to assign values into the group attribute table if no
#'   RAT is available and for MULTI* features it would be to keep the attributes
#'   as duplicated per-feature attributes (\code{FALSE})?
#' @param as_hex [logical(1)][logical]\cr should the bands 'red', 'green' and
#'   'blue' of a gridded object be transformed to hexadecimal values
#'   (\code{TRUE}), or should they be retained as columns in a stacked grid geom
#'   (\code{FALSE}, default)?.
#' @param ... additional arguments.
#' @details When transforming a simple feature to a geom, all MULTI* features
#'   are organised on a per feature basis, where the attribute table of features
#'   in the geom contains those variables that are valid for each feature, while
#'   the attribute table of groups contains those variables, that are unique
#'   only at the level of groups of features (i.e., at the level of MULTI*
#'   simple features). Those variables that are valid at the level of groups
#'   would be duplicated in the attribute table of features. When a MULTI*
#'   feature is transformed to a geom, the default behaviour is to copy the
#'   simple feature as closely as possible. However, to reduce the object size
#'   (and improve its' organisation), it is possible to assign the attributes of
#'   groups into the attribute table of groups of the geom by setting
#'   \code{group = TRUE}.
#'
#'   When transforming a Raster* (or possibly other gridded classes) with
#'   several layers to a geom, the layers are by default organised into a list
#'   with a layer per list item. However, when several layers contain
#'   fundamentally the same data (i.e., values that are associated to the same
#'   groups), layers could be stacked \code{stack = TRUE}, because they share
#'   the same group attribute table.
#' @return an object of class \code{geom}
#' @family spatial classes
#' @importFrom checkmate assertLogical assertNames
#' @importFrom tibble tibble
#' @importFrom grDevices rgb
#' @importFrom utils object.size
#' @name as_geometr
#' @rdname as_geometr
NULL

#' @rdname as_geometr
#' @name as_geometr
#' @export
setGeneric("as_geometr", function(x, ...) standardGeneric("as_geometr"))


#' @rdname as_geometr
#' @export
setMethod(f = "as_geometr",
          signature = "Spatial",
          definition = function(x = NULL, group = FALSE, ...){

            if (!requireNamespace("sp", quietly = TRUE)) {
              stop(
                "Package \"sp\" must be installed to use this function.",
                call. = FALSE
              )
            }

            theFeatures <- getFeatures(x = x)

            sourceClass <- class(x)[1]
            if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame", "SpatialPixels", "SpatialPixelsDataFrame")){
              type <- "point"
            } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){
              type <- "point"
            } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){
              type <- "line"
            } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialGrid", "SpatialGridDataFrame")){
              type <- "polygon"
            }

            if(group){

              temp <- theFeatures[-which(colnames(theFeatures) %in% c("fid", "gid"))]
              temp <- temp[!duplicated(temp),]
              theGroups <- as_tibble(cbind(gid = 1:dim(temp)[1], temp))
              theFeatures <- theFeatures[c("fid", "gid")]

            } else {
              theGroups <- tibble(gid = unique(theFeatures$gid))
            }

            tempData <- list(features = theFeatures, groups = theGroups)
            theData <- stats::setNames(list(tempData), paste0(getNames(x = x), collapse = "_"))

            provenance <- paste0("geom was transformed from an object of class '", sourceClass, "'.")

            out <- new(Class = "geom",
                       type = type,
                       geometry = getPoints(x = x),
                       data = theData,
                       window = getWindow(x = x),
                       crs = getCRS(x = x),
                       provenance = list(provenance))

            return(out)
          }
)

#' @rdname as_geometr
#' @export
setMethod(f = "as_geometr",
          signature = "sf",
          definition = function(x = NULL, group = FALSE, ...){

            if (!requireNamespace("sf", quietly = TRUE)) {
              stop(
                "Package \"sf\" must be installed to use this function.",
                call. = FALSE
              )
            }

            theFeatures <- getFeatures(x = x)

            sourceClass <- sf::st_geometry_type(x)
            if(length(unique(sourceClass)) == 1){
              sourceClass <- unique(sourceClass)
            } else{
              # what happens if a sf-object has different feature-types?
            }
            if(sourceClass %in% c("POINT", "MULTIPOINT")){
              type <- "point"
            } else if(sourceClass %in% c("LINESTRING", "MULTILINESTRING")){
              type <- "line"
            } else if(sourceClass %in% c("POLYGON", "MULTIPOLYGON")){
              type <- "polygon"
            }
            if(sourceClass %in% c("MULTIPOINT", "MULTILINESTRING", "MULTIPOLYGON") & group){

              temp <- theFeatures[-which(colnames(theFeatures) %in% c("fid", "gid"))]
              temp <- temp[!duplicated(temp),]
              theGroups <- as_tibble(cbind(gid = 1:dim(temp)[1], temp))
              theFeatures <- theFeatures[c("fid", "gid")]

            } else {
              theGroups <- tibble(gid = unique(theFeatures$gid))
            }
            provenance <- paste0("geom was transformed from an sf-object of geometry type '", sourceClass, "'.")

            tempData <- list(features = theFeatures, groups = theGroups)
            theData <- stats::setNames(list(tempData), getNames(x = x))

            out <- new(Class = "geom",
                       type = type,
                       geometry = getPoints(x = x),
                       data = theData,
                       window = getWindow(x = x),
                       crs = getCRS(x = x),
                       provenance = list(provenance))

            return(out)
          }
)

#' @rdname as_geometr
#' @export
setMethod(f = "as_geometr",
          signature = "Raster",
          definition = function(x = NULL, group = FALSE, as_hex = FALSE, ...){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            theExtent <- getExtent(x = x)
            theCoords <- tibble(y = c(min(theExtent$y), raster::nrow(x), raster::yres(x)),
                                x = c(min(theExtent$x), raster::ncol(x), raster::xres(x)))
            theCoords <- theCoords[c("x", "y")]

            theType <- getType(x = x)

            assertLogical(x = as_hex, len = 1)
            if(as_hex){
              assertNames(x = names(x), must.include = c("red", "green", "blue"))
              red <- getFeatures(x = x[["red"]])[[2]]
              red[is.na(red)] <- 255L
              green <- getFeatures(x = x[["green"]])[[2]]
              green[is.na(green)] <- 255L
              blue <- getFeatures(x = x[["blue"]])[[2]]
              blue[is.na(blue)] <- 255L
              alpha <- rep(255, length(blue))
              alpha[is.na(red)] <- 0L
              alpha[is.na(green)] <- 0L
              alpha[is.na(blue)] <- 0L

              x <- x[[1]] # subset to have dim(x) == 1
              names(x) <- "hex_col"
            }

            theData <- theFeatures <- theGroups <- hist <- NULL
            for(i in 1:dim(x)[3]){

              theInput <- x[[i]]
              theName <- names(x)[i]
              hist <- c(hist, paste0("layer '", theName, "' was transformed from an object of class ", theType[2], "."))

              if(as_hex){
                rawVal <- rgb(red = red, green = green, blue = blue, alpha = alpha, maxColorValue = 255)
              } else {
                rawVal <- getFeatures(x = theInput)[[2]]
              }
              tempGroups <- getGroups(theInput)
              if(group & dim(tempGroups)[1] == 0) {
                tempGroups <- tibble(gid = sortUniqueCpp(raster::getValues(theInput)))
              }

              rleVal <- rle(rawVal)
              if(object.size(rleVal) > object.size(rawVal)){
                tempFeatures <- tibble(rawVal)
                names(tempFeatures) <- "val"
              } else {
                tempFeatures <- tibble(val = rleVal$values,
                                       len = rleVal$lengths)
                hist <- c(hist, paste0("layer '", theName, "' is run-length encoded."))
              }

              tempData <- list(features = tempFeatures, groups = tempGroups)
              theData <- c(theData, stats::setNames(list(tempData), theName))

            }

            out <- new(Class = "geom",
                       type = "grid",
                       geometry = theCoords,
                       data = theData,
                       window = getWindow(x = x),
                       crs = getCRS(x = x),
                       provenance = c(getProvenance(x), hist))

            return(out)
          }
)
