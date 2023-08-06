#' Get the table of feature attributes
#'
#' Get tabular information of the attributes of features.
#' @param x the object from which to derive the attribute table.
#' @details This table contains at least the column 'fid'. In case \code{x} has
#'   any typ other than 'grid', it contains also the column 'gid' and in case it
#'   has type 'grid', it also contains the column 'values'.
#' @return A tibble (or a list of tibbles per layer) of the feature attributes
#'   of \code{x}.
#' @family getters
#' @importFrom tibble tibble as_tibble
#' @importFrom methods as
#' @name getFeatures
#' @rdname getFeatures
NULL

#' @rdname getFeatures
#' @name getFeatures
#' @export
setGeneric("getFeatures", function(x) standardGeneric("getFeatures"))


#' @rdname getFeatures
#' @export
setMethod(f = "getFeatures",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            if(x@type == "grid"){

              fids <- x@geometry$x[2] * x@geometry$y[2]

              out <- tibble(fid = 1:fids)

              for(i in seq_along(x@data)){
                theFeatures <- x@data[[i]]$features
                theName <- names(x@data[[i]]$groups)[1]

                if(all(c("val", "len") %in% names(theFeatures))){

                  temp <- list(lengths = theFeatures$len,
                               values = theFeatures$val)
                  attr(temp, "class") <- "rle"
                  temp <- inverse.rle(temp)

                } else {
                  temp <- theFeatures
                }

                out[theName] <- temp

              }

            } else {
              out <- x@data[[1]]$features
            }

            return(out)
          }
)

#' @rdname getFeatures
#' @export
setMethod(f = "getFeatures",
          signature = signature("Spatial"),
          definition = function(x){

            if (!requireNamespace("sp", quietly = TRUE)) {
              stop(
                "Package \"sp\" must be installed to use this function.",
                call. = FALSE
              )
            }

            theData <- NULL
            sourceClass <- class(x)[1]
            if(sourceClass %in% c("SpatialGrid")){
              sourceClass <- "SpatialPolygons"
            } else if(sourceClass %in% "SpatialGridDataFrame"){
              sourceClass <- "SpatialPolygonsDataFrame"
            } else if(sourceClass %in% "SpatialPixels"){
              sourceClass <- "SpatialPoints"
            } else if(sourceClass %in% "SpatialPixelsDataFrame"){
              sourceClass <- "SpatialPointsDataFrame"
            }
            x <- as(x, sourceClass)
            prev <- 0
            if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame")){
              type <- "point"

              if(sourceClass %in% "SpatialPointsDataFrame"){
                theData <- tibble(fid = seq_along(x@coords[,1]),
                                  gid = seq_along(x@coords[,1]))
                theData <- as_tibble(cbind(theData, x@data))
              } else{
                theData <- tibble(fid = seq_along(x@coords[,1]),
                                  gid = seq_along(x@coords[,1]))
              }

            } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){
              type <- "point"

              for(i in seq_along(x@coords)){
                tempCoords <- x@coords[[i]]

                if(sourceClass %in% "SpatialMultiPointsDataFrame"){
                  tempData <- tibble(fid = seq_along(tempCoords[,1])+prev,
                                     gid = i,
                                     x@data[i,])
                  j <- length(tempCoords[,1])

                  theData <- rbind(theData, tempData)
                  otherNames <- colnames(x@data)
                } else{
                  tempData <- tibble(fid = seq_along(tempCoords[,1])+prev,
                                     gid = i)
                  j <- length(tempCoords[,1])
                  theData <- rbind(theData, tempData)
                  otherNames <- NULL
                }
              }
              colnames(theData) <- c("fid", "gid", otherNames)

            } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){
              type <- "line"

              for(i in seq_along(x@lines)){
                theLines <- x@lines[[i]]

                for(j in seq_along(theLines@Lines)){
                  if(sourceClass %in% "SpatialLinesDataFrame"){
                    tempData <- tibble(fid = prev + j, gid = prev + j, x@data[i,])
                    theData <- rbind(theData, tempData)
                    otherNames <- colnames(x@data)
                  } else{
                    theData <- rbind(theData, tibble(fid = prev + j, gid = prev + j))
                    otherNames <- NULL
                  }
                }
                prev <- prev + length(theLines@Lines)

              }
              colnames(theData) <- c("fid", "gid", otherNames)

            } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){
              type <- "polygon"

              for(i in seq_along(x@polygons)){
                thePolys <- x@polygons[[i]]

                if(sourceClass %in% "SpatialPolygonsDataFrame"){
                  tempData <- x@data[i,]
                  otherNames <- colnames(x@data)
                } else{
                  tempData <- NULL
                  otherNames <- NULL
                }

                # for(j in seq_along(thePolys@Polygons)){
                #   polyCoords <- thePolys@Polygons[[j]]@coords
                #   polyCoords <- polyCoords[!duplicated(polyCoords),]
                # }
                tempData <- as_tibble(cbind(fid = i, gid = i, tempData))

                theData <- rbind(theData, tempData)

              }
              colnames(theData) <- c("fid", "gid", otherNames)

            }
            out <- theData

            return(out)
          }
)

#' @rdname getFeatures
#' @export
setMethod(f = "getFeatures",
          signature = "sf",
          definition = function(x){

            if (!requireNamespace("sf", quietly = TRUE)) {
              stop(
                "Package \"sf\" must be installed to use this function.",
                call. = FALSE
              )
            }

            sourceClass <- sf::st_geometry_type(x)
            theCoords <- sf::st_coordinates(x)
            if(length(unique(sourceClass)) == 1){
              sourceClass <- unique(sourceClass)
              if(sourceClass %in% c("POINT")){

                data <- x
                sf::st_geometry(data) <- NULL
                fids <- seq_along(theCoords[, 1])
                out <- tibble(fid = fids, gid = fids)
                if(dim(data)[2] != 0){
                  out <- as_tibble(cbind(out, data))
                }
                colnames(out) <- c("fid", "gid", names(data))

              } else if(sourceClass %in% c("MULTIPOINT")){

                data <- x
                sf::st_geometry(data) <- NULL
                fids <- seq_along(theCoords[, 1])
                gids <- theCoords[, 3]
                out <- tibble(fid = fids, gid = gids)
                if(dim(data)[2] != 0){
                  out <- as_tibble(cbind(out, data))
                }
                colnames(out) <- c("fid", "gid", names(data))

              } else if(sourceClass %in% c("LINESTRING")){

                data <- x
                sf::st_geometry(data) <- NULL
                fids <- unique(theCoords[, 3])
                out <- tibble(fid = fids, gid = fids)
                if(dim(data)[2] != 0){
                  out <- as_tibble(cbind(out, data))
                }
                colnames(out) <- c("fid", "gid", names(data))

              } else if(sourceClass %in% c("MULTILINESTRING")){

                data <- x
                sf::st_geometry(data) <- NULL
                dataNames <- names(data)

                fact <- 10**nchar(max(theCoords[,3]))
                toSeq <- theCoords[,4]*fact + theCoords[,3]
                toSeq <- rle(toSeq)
                fids <- seq_along(toSeq$values)
                gids <- tibble(L2 = unique(theCoords[, 4]),
                               count = as.numeric(by(data = theCoords[, 3], INDICES = theCoords[, 4], function(x) length(unique(x)))))

                out <- tibble(fid = fids,
                              gid = rep(seq_along(gids$L2), gids$count))
                if(dim(data)[2] != 0){
                  data <- tibble(rep(data[,1], gids$count))
                  out <- as_tibble(cbind(out, data))
                }
                colnames(out) <- c("fid", "gid", dataNames)

              } else if(sourceClass %in% c("POLYGON")){

                data <- x
                sf::st_geometry(data) <- NULL
                dataNames <- names(data)
                fids <- unique(theCoords[, 4])
                new <- tibble(fid = fids, gid = fids)
                out <- as_tibble(cbind(new, data))
                colnames(out) <- c("fid", "gid", dataNames)

              } else if(sourceClass %in% c("MULTIPOLYGON")){

                data <- x
                sf::st_geometry(data) <- NULL
                dataNames <- colnames(data)

                fact <- 10**nchar(max(theCoords[,4]))
                toSeq <- theCoords[,5]*fact + theCoords[,4]
                toSeq <- rle(toSeq)
                fids <- seq_along(toSeq$values)
                gids <- tibble(L3 = unique(theCoords[, 5]),
                               count = as.numeric(by(data = theCoords[, 4], INDICES = theCoords[, 5], function(x) length(unique(x)))))
                new <- tibble(fid = fids,
                              gid = rep(seq_along(gids$L3), gids$count))
                data <- as_tibble(data[rep(seq_len(nrow(data)), gids$count),])
                out <- as_tibble(cbind(new, data))
                colnames(out) <- c("fid", "gid", dataNames)

              }
            } else{
              # what happens if a sf-object has different feature-types?
              stop("simple features with multiple feature types are not yet supported.")
            }

            return(out)
          }
)

#' @rdname getFeatures
#' @export
setMethod(f = "getFeatures",
          signature = "Raster",
          definition = function(x){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            vals <- raster::getValues(x)
            if(inherits(x = x, what = "RasterBrick")){
              out <- tibble(fid = seq_along(vals[,1]), gid = 1)
              out <- as_tibble(cbind(out, vals))
            } else {
              out <- NULL
              for(i in 1:dim(x)[3]){
                if(is.matrix(vals)){
                  temp <- vals[,i]
                  theName <- "matrix"
                } else {
                  temp <- vals
                  theName <- names(x)
                }
                tab <- tibble(fid = seq_along(temp), temp)
                colnames(tab) <- c("fid", theName)
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

#' @rdname getFeatures
#' @export
setMethod(f = "getFeatures",
          signature = "SpatRaster",
          definition = function(x){

            if (!requireNamespace("terra", quietly = TRUE)) {
              stop(
                "Package \"terra\" must be installed to use this function.",
                call. = FALSE
              )
            }

            vals <- terra::values(x)
            out <- NULL
            for(i in 1:dim(vals)[2]){
              if(is.matrix(vals)){
                temp <- vals[,i]
              } else {
                temp <- vals
              }

              tab <- tibble(fid = seq_along(temp), values = temp)
              if(dim(x)[3] == 1){
                out <- tab
              } else {
                out <- c(out, setNames(list(tab), names(x)[i]))
              }

            }
            return(out)
          }
)

#' @rdname getFeatures
#' @export
setMethod(f = "getFeatures",
          signature = "matrix",
          definition = function(x){

            temp <- as.vector(t(x))
            out <- tibble(fid = seq_along(temp), gid = temp, values = temp)

            return(out)
          }
)