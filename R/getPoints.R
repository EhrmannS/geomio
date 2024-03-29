#' Get the table of point coordinates
#'
#' Get tabular information of the point coordinates.
#' @param x the object from which to derive the point coordinates.
#' @details This table contains three columns (x, y and fid) and as many rows as
#'   there are points. In case \code{x} is a polygon, the last point of each
#'   distinct feature is a duplicate of its first point. In case \code{x} has
#'   the type 'grid', all layers are summarised into one tibble, as several
#'   layers must have the same extent and resolution, so that each point occurrs
#'   in each layer, merely with a different, layer-specific value.
#' @return A tibble of the point coordinates of \code{x}.
#' @family getters
#' @importFrom tibble tibble as_tibble
#' @importFrom methods as
#' @name getPoints
#' @rdname getPoints
NULL

#' @rdname getPoints
#' @name getPoints
#' @export
setGeneric("getPoints", function(x) standardGeneric("getPoints"))


#' @rdname getPoints
#' @export
setMethod(f = "getPoints",
          signature = "geom",
          definition = function(x){

            if (!requireNamespace("geometr", quietly = TRUE)) {
              stop(
                "Package \"geometr\" must be installed to use this function.",
                call. = FALSE
              )
            }

            theType <- getType(x = x)[1]

            if(theType == "grid"){
              # rebuild points
              xGrid <- seq(from = x@geometry$x[1], length.out = x@geometry$x[2], by = x@geometry$x[3]) + 0.5
              yGrid <- seq(from = x@geometry$y[1], length.out = x@geometry$y[2], by = x@geometry$y[3]) + 0.5
              out <- tibble(fid = seq(1:(length(xGrid)*length(yGrid))),
                            x = rep(xGrid, times = length(yGrid)),
                            y = rep(yGrid, each = length(xGrid)))
            } else {
              thePoints <- x@geometry
              out <- tibble(fid = thePoints$fid,
                            x = thePoints$x,
                            y = thePoints$y)
            }

            return(out)
          }
)

#' @rdname getPoints
#' @export
setMethod(f = "getPoints",
          signature = "Spatial",
          definition = function(x){

            if (!requireNamespace("sp", quietly = TRUE)) {
              stop(
                "Package \"sp\" must be installed to use this function.",
                call. = FALSE
              )
            }

            theCoords <- NULL
            prev <- 0
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

            if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame")){

              theCoords <- as_tibble(cbind(x@coords,
                                           fid = seq_along(x@coords[,1])))
              colnames(theCoords) <- c("x", "y", "fid")

            } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){

              temp <- x
              nCoords <- 0
              for(i in seq_along(temp@coords)){
                tempCoords <- tibble(x = temp@coords[[i]][,1],
                                     y = temp@coords[[i]][,2])
                theCoords <- rbind(theCoords, tempCoords)
              }
              theCoords <- tibble(x = theCoords$x,
                                  y = theCoords$y,
                                  fid = seq_along(theCoords$x))

            } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){

              for(i in seq_along(x@lines)){
                theLines <- x@lines[[i]]
                for(j in seq_along(theLines@Lines)){
                  theLine <- theLines@Lines[[j]]

                  tempCoords <- tibble(x = theLine@coords[,1],
                                       y = theLine@coords[,2],
                                       fid = prev + j)
                  theCoords <- rbind(theCoords, tempCoords)
                }
                prev <- prev + length(theLines@Lines)
              }

            } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){

              for(i in seq_along(x@polygons)){
                thePolys <- x@polygons[[i]]
                for(j in seq_along(thePolys@Polygons)){
                  thePoly <- thePolys@Polygons[[j]]
                  polyCoords <- thePoly@coords

                  tempCoords <- tibble(x = polyCoords[,1],
                                       y = polyCoords[,2],
                                       fid = i)
                  theCoords <- rbind(theCoords, tempCoords)
                }
              }

            }

            return(theCoords)
          }
)

#' @rdname getPoints
#' @export
setMethod(f = "getPoints",
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

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = seq_along(theCoords[, 1]))

              } else if(sourceClass %in% c("MULTIPOINT")){

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = seq_along(theCoords[, 1]))

              } else if(sourceClass %in% c("LINESTRING")){

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = theCoords[,3])

              } else if(sourceClass %in% c("MULTILINESTRING")){

                fact <- 10**nchar(max(theCoords[,3]))
                toSeq <- theCoords[,4]*fact + theCoords[,3]
                toSeq <- rle(toSeq)
                fids <- rep(seq_along(toSeq$values), toSeq$lengths)

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = fids)

              } else if(sourceClass %in% c("POLYGON")){

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = theCoords[,4])

              } else if(sourceClass %in% c("MULTIPOLYGON")){

                fact <- 10**nchar(max(theCoords[,4]))
                toSeq <- theCoords[,5]*fact + theCoords[,4]
                toSeq <- rle(toSeq)
                fids <- rep(seq_along(toSeq$values), toSeq$lengths)

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = fids)

              }
            } else{
              # what happens if a sf-object has different feature-types?
              stop("simple features with multiple feature types are not yet supported.")
            }

            return(theCoords)
          }
)

#' @rdname getPoints
#' @export
setMethod(f = "getPoints",
          signature = "Raster",
          definition = function(x){

            if (!requireNamespace("raster", quietly = TRUE)) {
              stop(
                "Package \"raster\" must be installed to use this function.",
                call. = FALSE
              )
            }

            res <- raster::res(x)
            xGrid <- seq(from = x@extent@xmin, length.out = x@ncols, by = res[1]) + 0.5
            yGrid <- seq(from = x@extent@ymin, length.out = x@nrows, by = res[2]) + 0.5
            out <- tibble(x = rep(xGrid, times = length(yGrid)),
                          y = rep(yGrid, each = length(xGrid)),
                          fid = seq(1:(length(xGrid)*length(yGrid))))

            return(out)
          }
)

#' @rdname getPoints
#' @export
setMethod(f = "getPoints",
          signature = "SpatRaster",
          definition = function(x){

            if (!requireNamespace("terra", quietly = TRUE)) {
              stop(
                "Package \"terra\" must be installed to use this function.",
                call. = FALSE
              )
            }

            res <- terra::res(x)
            ext <- terra::ext(x)
            xGrid <- seq(from = ext[1], length.out = terra::ncol(x), by = res[1]) + 0.5
            yGrid <- seq(from = ext[3], length.out = terra::nrow(x), by = res[2]) + 0.5
            out <- tibble(x = rep(xGrid, times = length(yGrid)),
                          y = rep(yGrid, each = length(xGrid)),
                          fid = seq(1:(length(xGrid)*length(yGrid))))


            return(out)
          }
)

#' @rdname getPoints
#' @export
setMethod(f = "getPoints",
          signature = "matrix",
          definition = function(x){

            xGrid <- seq(from = 0, length.out = ncol(x), by = 1) + 0.5
            yGrid <- seq(from = 0, length.out = nrow(x), by = 1) + 0.5
            out <- tibble(x = rep(xGrid, times = length(yGrid)),
                          y = rep(yGrid, each = length(xGrid)),
                          fid = seq(1:(length(xGrid)*length(yGrid))))


            return(out)
          }
)