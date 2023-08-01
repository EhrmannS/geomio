#' Transform a spatial object to class \code{sf}
#'
#' @param x the object to transform to class \code{sf}.
#' @param ... other arguments.
#' @return If \code{x} is a \code{geom} and has attributes other than
#'   \code{fid} and \code{gid}, a "Simple feature collection", otherwise a
#'   "Geometry set". Several features of the \code{geom} are returned as MULTI*
#'   feature, when they have \code{gid} and optionally other attributes in
#'   common, otherwise they are returned as a single simple feature.
#' @family spatial classes
#' @importFrom checkmate assertClass
#' @importFrom tibble tibble as_tibble
#' @name as_sf
#' @rdname as_sf
NULL

#' @rdname as_sf
#' @name as_sf
#' @export
setGeneric("as_sf", function(x, ...) standardGeneric("as_sf"))


#' @rdname as_sf
#' @export
setMethod(f = "as_sf",
          signature = "geom",
          definition = function(x = NULL){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            theCoords <- getPoints(x = x)
            theData <- getFeatures(x = x)
            theGroups <- getGroups(x = x)
            theCRS <- getCRS(x = x)
            featureType <- getType(x)[2]

            makeDF <- FALSE

            if(featureType == "point"){

              gids <- unique(theData$gid)
              tempOut <- list()
              for(i in seq_along(gids)){
                tempFids <- theData$fid[theData$gid == gids[i]]
                tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids,]

                if(length(tempVerts$x) > 1 | any(table(theData$gid) > 1)){
                  # make MULTIPOINT
                  # ensure that there are no duplicate coordinates for it to be a simple feature
                  tempVerts <- tempVerts[!duplicated(tempVerts[c("x", "y")]),]
                  tempOut <- c(tempOut, list(sf::st_multipoint(as.matrix(tempVerts))))
                } else{
                  # make POINT
                  tempOut <- c(tempOut, list(sf::st_point(as.matrix(tempVerts))))
                }
              }
              out <- sf::st_sfc(tempOut)

              attr <- tibble(fid = unique(theCoords$fid))
              if(!all(names(theCoords) %in% c("x", "y", "fid"))){
                if(length(out) < dim(theCoords)[1]){
                  warning("MULTIPOINTS don't support individual attributes per point, ignoring '", names(theCoords)[!names(theCoords) %in% c("x", "y", "fid")] , "'.")
                } else {
                  makeDF <- TRUE
                  attr <- theCoords[,!names(theCoords) %in% c("x", "y")]
                }
              }

              if(!all(names(theData) %in% c("fid", "gid"))){
                makeDF <- TRUE
              }
              attr <- merge(x = attr, y = theData, by = "fid", all.x = TRUE, suffixes = c(".point", ".feature"))

              if(!all(names(theGroups) %in% c("gid"))){
                makeDF <- TRUE
              }
              attr <- as_tibble(merge(x = attr, y = theGroups, by = "gid", all.x = TRUE, suffixes = c(".feature", ".group")))

              if(length(out) < dim(theCoords)[1]){
                uniqueNames <- names(attr)[!names(attr) %in% names(theGroups)]
                warning("MULTIPOINTS don't support individual attributes per point, ignoring '", paste(uniqueNames, collapse = ", ") , "'.")
                attr <- attr[-which(colnames(attr) %in% uniqueNames)]
                attr <- unique(attr)
              }

              if(makeDF){
                attr <- attr[,!names(attr) %in% c("fid", "gid")]
                out <- sf::st_sf(geom = out, attr)
              } else {
                out <- sf::st_sf(geom = out)
              }

            } else if(featureType %in% c("line")){

              gids <- unique(theData$gid)
              tempOut <- list()
              for(i in seq_along(gids)){
                tempFids <- theData$fid[theData$gid == gids[i]]

                if(length(tempFids) > 1){
                  # make MULTILINESTRING
                  subStrings <- list()
                  for(j in seq_along(tempFids)){
                    tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids[j],]
                    subStrings <- c(subStrings, list(as.matrix(tempVerts)))
                  }
                  tempOut <- c(tempOut, list(sf::st_multilinestring(subStrings)))

                } else{
                  tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids,]
                  # make LINESTRING
                  tempOut <- c(tempOut, list(sf::st_linestring(as.matrix(tempVerts))))

                }
              }
              out <- sf::st_sfc(tempOut)

              attr <- tibble(gid = unique(theData$gid))
              if(!all(names(theData) %in% c("fid", "gid"))){
                uniqueData <- theData[,!names(theData) %in% c("fid")]
                uniqueData <- uniqueData[!duplicated(uniqueData),]
                if(length(out) != dim(uniqueData)[1]){
                  warning("MULTILINESTRING doesn't support individual attributes per line, ignoring '", names(theData)[!names(theData) %in% c("fid", "gid")] , "'.")
                } else {
                  makeDF <- TRUE
                  attr <- uniqueData
                }
              }

              if(!all(names(theGroups) %in% c("gid"))){
                makeDF <- TRUE
              }
              attr <- as_tibble(merge(x = attr, y = theGroups, by = "gid", all.x = TRUE, suffixes = c(".feature", ".group")))

              if(makeDF){
                attr <- attr[,!names(attr) %in% c("fid", "gid")]
                out <- sf::st_sf(geom = out, attr)
              } else {
                out <- sf::st_sf(geom = out)
              }

            } else if(featureType %in% c("polygon")){

              gids <- unique(theData$gid)
              tempOut <- list()
              for(i in seq_along(gids)){

                tempFids <- theData$fid[theData$gid == gids[i]]

                if(length(tempFids) > 1){
                  # make MULTIPOLYGON
                  subPolys <- list()
                  for(j in seq_along(tempFids)){
                    tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids[j],]
                    dups <- as.numeric(duplicated(tempVerts))
                    dups <- c(0, dups[-length(dups)])
                    rings <- 1 + cumsum(dups)
                    tempVerts <- split(x = tempVerts, f = rings)
                    tempVerts <- lapply(seq_along(tempVerts), function(x){
                      as.matrix(tempVerts[[x]])
                    })
                    subPolys <- c(subPolys, list(tempVerts))
                  }
                  tempOut <- c(tempOut, list(sf::st_multipolygon(subPolys)))

                } else{
                  # make POLYGON
                  tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids,]
                  dups <- as.numeric(duplicated(tempVerts))
                  dups <- c(0, dups[-length(dups)])
                  rings <- 1 + cumsum(dups)
                  tempVerts <- split(x = tempVerts, f = rings)
                  tempVerts <- lapply(seq_along(tempVerts), function(x){
                    as.matrix(tempVerts[[x]])
                  })
                  tempOut <- c(tempOut, list(sf::st_polygon(tempVerts)))
                }
              }
              out <- sf::st_sfc(tempOut)

              attr <- tibble(gid = unique(theData$gid))
              if(!all(names(theData) %in% c("fid", "gid"))){
                uniqueData <- theData[,!names(theData) %in% c("fid")]
                uniqueData <- uniqueData[!duplicated(uniqueData),]
                if(length(out) != dim(uniqueData)[1]){
                  warning("MULTIPOLYGON doesn't support individual attributes per polygon, ignoring '", paste0(names(theData)[!names(theData) %in% c("fid", "gid")], collapse = ", ") , "'.")
                } else {
                  makeDF <- TRUE
                  attr <- uniqueData
                }
              }

              if(!all(names(theGroups) %in% c("gid"))){
                makeDF <- TRUE
              }
              attr <- as_tibble(merge(x = attr, y = theGroups, by = "gid", all.x = TRUE, suffixes = c(".feature", ".group")))

              if(makeDF){
                attr <- attr[,!names(attr) %in% c("fid", "gid")]
                out <- sf::st_sf(geom = out, attr)
              } else {
                out <- sf::st_sf(geom = out)
              }
            }
            out <- sf::st_set_crs(x = out, value = theCRS)

            return(out)
          }
)
