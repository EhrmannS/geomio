#' Transform a spatial object to class \code{Spatial}
#'
#' @param x the object to transform to class \code{Spatial}.
#' @param ... other arguments.
#' @return an object of class \code{Spatial}
#' @family spatial classes
#' @importFrom tibble as_tibble
#' @importFrom checkmate assertClass
#' @name as_sp
#' @rdname as_sp
NULL

#' @rdname as_sp
#' @name as_sp
#' @export
setGeneric("as_sp", function(x, ...) standardGeneric("as_sp"))


#' @rdname as_sp
#' @export
setMethod(f = "as_sp",
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
              attr <- tibble(fid = theCoords$fid)

              temp <- theCoords[c("x", "y")]
              out <- sp::SpatialPoints(temp)

              if(!all(names(theCoords) %in% c("x", "y", "fid"))){
                makeDF <- TRUE
                attr <- theCoords[,!names(theCoords) %in% c("x", "y")]
              }
              if(!all(names(theData) %in% c("fid", "gid"))){
                makeDF <- TRUE
                temp <- theData[,!names(theData) %in% c("gid")]
                attr <- merge(x = attr, y = temp, by = "fid", all.x = TRUE)
              }
              if(!all(names(theGroups) %in% c("gid"))){
                makeDF <- TRUE
                temp <- merge(x = theData[c("fid", "gid")], y = theGroups, by = "gid", all.x = TRUE)
                attr <- merge(x = attr, y = temp, by = "fid", all.x = TRUE)
              }

              if(makeDF){
                attr <- attr[,!names(attr) %in% c("fid", "gid")]
                out <- sp::SpatialPointsDataFrame(out, data = attr, match.ID = FALSE)
              }

            } else if(featureType == "line"){
              attr <- tibble(fid = theData$fid)

              fids <- unique(theData$fid)
              tempOut <- list()
              outLines <- list()

              for(i in seq_along(fids)){
                tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% i,]
                outLines <- c(outLines, sp::Lines(list(sp::Line(tempVerts)), fids[i]))
              }
              out <- sp::SpatialLines(outLines)

              if(!all(names(theData) %in% c("fid", "gid"))){
                makeDF <- TRUE
                temp <- theData[,!names(theData) %in% c("gid")]
                attr <- merge(x = attr, y = temp, by = "fid", all.x = TRUE)
              }
              if(!all(names(theGroups) %in% c("gid"))){
                makeDF <- TRUE
                temp <- merge(x = theData[c("fid", "gid")], y = theGroups, by = "gid", all.x = TRUE)
                attr <- merge(x = attr, y = temp, by = "fid", all.x = TRUE)
              }

              if(makeDF){
                attr <- attr[,!names(attr) %in% c("fid", "gid")]
                out <- sp::SpatialLinesDataFrame(out, data = attr, match.ID = FALSE)
              }

            } else if(featureType == "polygon"){
              attr <- tibble(fid = theData$fid)

              gids <- unique(theData$gid)
              outPolygons <- list()
              for(i in seq_along(gids)){

                tempFids <- theData$fid[theData$gid == gids[i]]
                tempPolys <- list()
                for(j in seq_along(tempFids)){

                  tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids[j],]
                  dups <- as.numeric(duplicated(tempVerts))
                  dups <- c(0, dups[-length(dups)])
                  rings <- 1 + cumsum(dups)
                  temp <- split(x = tempVerts, f = rings)
                  tempVerts <- lapply(seq_along(temp), function(x){
                    newVerts <- temp[[x]]
                    if(min(newVerts$x) > min(tempVerts$x) & max(newVerts$x) < max(tempVerts$x) &
                       min(newVerts$y) > min(tempVerts$y) & max(newVerts$y < max(tempVerts$y))){
                      sp::Polygon(as.matrix(newVerts), hole = TRUE)
                    } else{
                      sp::Polygon(as.matrix(newVerts))
                    }
                  })
                  tempPolys <- c(tempPolys, tempVerts)

                }
                outPolygons <- c(outPolygons, sp::Polygons(tempPolys, gids[i]))

              }
              # make a SpatialPolygon out of that
              out <- sp::SpatialPolygons(outPolygons)

              if(!all(names(theData) %in% c("fid", "gid"))){
                makeDF <- TRUE
                temp <- theData[,!names(theData) %in% c("gid"), drop = FALSE]
                attr <- merge(x = attr, y = temp, by = "fid", all.x = TRUE)
              }
              if(!all(names(theGroups) %in% c("gid"))){
                makeDF <- TRUE
                temp <- merge(x = theData[c("fid", "gid")], y = theGroups, by = "gid", all.x = TRUE)
                attr <- merge(x = attr, y = temp, by = "fid", all.x = TRUE)
              }

              if(makeDF){
                attr <- attr[,!names(attr) %in% c("fid", "gid"), drop = FALSE]
                out <- sp::SpatialPolygonsDataFrame(out, data = attr, match.ID = FALSE)
              }
            } else if(featureType == "grid"){

            }

            if(!is.na(theCRS)){
              sp::proj4string(out) <- sp::CRS(theCRS)
            }

            return(out)
          }
)
