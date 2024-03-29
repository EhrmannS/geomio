library(testthat)
library(checkmate)
library(geometr)
library(sp)
library(sf)
library(raster)
context("as_geometr")


test_that("transform from sf to geom", {

  # test POINT
  # temp <- gtGeoms$point
  temp@feature$gid <- temp@feature$fid
  input <- gc_sf(temp)

  output <- as_geometr(input = input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 9, ncols = 3)

  # test MULTIPOINT
  # input <- gc_sf(gtGeoms$point)

  output <- as_geometr(input = input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 9, ncols = 3)
  expect_data_frame(output@feature, any.missing = FALSE, nrows = 9, ncols = 2)

  # output <- as_geometr(input = input, group = TRUE)
  # expect_class(output, classes = "geom")
  # expect_true(output@type == "point")
  # expect_data_frame(output@point, any.missing = FALSE, nrows = 8, ncols = 3)
  # expect_data_frame(output@group, any.missing = FALSE, nrows = 2, ncols = 2)

  # test LINESTRING
  # input <- gc_sf(gtGeoms$line)

  output <- as_geometr(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "line")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 9, ncols = 3)

  # test MULTILINESTRING
  # temp <- gtGeoms$line
  temp@feature$gid <- 1
  temp@group$gid <- 1
  input <- gc_sf(temp)

  output <- as_geometr(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "line")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 9, ncols = 3)

  # test POLYGON
  # input <- gc_sf(gtGeoms$polygon)

  output <- as_geometr(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 11, ncols = 3)

  # test MULTIPOLYGON
  # temp <- gtGeoms$polygon
  temp@feature$gid <- 1
  temp@group$gid <- 1
  input <- as_sf(temp)

  output <- as_geometr(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 11, ncols = 3)
})

test_that("transform from sp to geom", {
  # test 'SpatialPoints'
  # input <- gc_sp(input = gtGeoms$point)

  output <- as_geometr(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")

  # test 'SpatialPointsDataFrame'
  input <- SpatialPointsDataFrame(input, data.frame(data = 9:1), match.ID = TRUE)

  output <- as_geometr(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")

  # # test 'SpatialMultiPoints'
  # input <- gtSP$SpatialMultiPoints
  #
  # output <- as_geometr(input)
  # expect_class(output, "geom")
  # expect_true(output@type == "point")
  # expect_true(length(unique(output@point$fid)) == 8)

  # # test 'SpatialMultiPointsDataFrame'
  # input <- SpatialMultiPointsDataFrame(input, data = data.frame(data = 1:2))
  #
  # output <- as_geometr(input)
  # expect_class(output, "geom")
  # expect_true(output@type == "point")
  # expect_data_frame(output@feature, any.missing = FALSE, nrows = 8, ncols = 3)

  # test 'SpatialLines'
  # input <- gc_sp(input = gtGeoms$line)

  output <- as_geometr(input)
  expect_class(output, "geom")
  expect_true(output@type == "line")
  expect_true(length(unique(output@point$fid)) == 2)

  # test 'SpatialLinesDataFrame'
  input <- SpatialLinesDataFrame(input, data = data.frame(data = 1:2), match.ID = FALSE)

  output <- as_geometr(input)
  expect_class(output, "geom")
  expect_true(output@type == "line")
  expect_data_frame(output@feature, any.missing = FALSE, nrows = 2, ncols = 3)

  # test 'SpatialPolygons'
  # input <- gc_sp(input = gtGeoms$polygon)

  output <- as_geometr(input)
  expect_class(output, "geom")
  expect_true(output@type == "polygon")

  # test 'SpatialPolygonsDataFrame'
  input <- SpatialPolygonsDataFrame(input, data = data.frame(data = 1:2), match.ID = FALSE)

  output <- as_geometr(input)
  expect_class(output, "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@feature, any.missing = FALSE, nrows = 2, ncols = 3)

  # test 'SpatialGrid'
  x = GridTopology(c(0,0), c(1,1), c(5,5))
  input = SpatialGrid(grid = x)

  output <- as_geometr(input = input)
  expect_class(output, "geom")
  expect_true(output@type == "polygon")

  # test 'SpatialGridDataFrame'
  input <- SpatialGridDataFrame(grid = input, data = data.frame(data = letters[1:25]))

  output <- as_geometr(input)
  expect_class(output, "geom")
  expect_true(output@type == "polygon")

  # test 'SpatialPixels'
  data(meuse.grid)
  pts = meuse.grid[c("x", "y")]
  input = SpatialPixels(SpatialPoints(pts))

  output <- as_geometr(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")

  # test 'SpatialPixelsDataFrame'
  input <- SpatialPixelsDataFrame(points = input, data = meuse.grid)

  output <- as_geometr(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")
})

test_that("transform from Raster to geom", {
  # RasterStack
  # input <- raster::stack(list(as_raster(gtGeoms$grid$categorical),
  #                             as_raster(gtGeoms$grid$continuous)))
  names(input) <- c("categorical", "continuous")

  output <- as_geometr(input)
  expect_list(x = output, len = 2)
  expect_class(output$categorical, "geom")
  expect_true(output$categorical@type == "grid")
  expect_class(output$continuous, "geom")
  expect_true(output$continuous@type == "grid")
  expect_data_frame(x = output$continuous@group, nrows = 0)

  # RasterStack with grouping
  output <- as_geometr(input, group = TRUE)
  expect_list(x = output, len = 2)
  expect_class(output$categorical, "geom")
  expect_true(output$categorical@type == "grid")
  expect_class(output$continuous, "geom")
  expect_true(output$continuous@type == "grid")
  expect_data_frame(x = output$continuous@group, nrows = 91)

  # RasterStack with stacking
  output <- as_geometr(input, stack = TRUE)
  expect_class(output, "geom")
  expect_true(output@type == "grid")
  expect_data_frame(x = output@group, nrows = 9)

  # RasterStack with stacking and grouping
  output <- as_geometr(input, stack = TRUE, group = TRUE)
  expect_class(output, "geom")
  expect_true(output@type == "grid")
  expect_data_frame(x = output@group, nrows = 93)

  # RasterLayer
  # input <- gc_raster(gtGeoms$grid$continuous)

  output <- as_geometr(input)
  expect_class(output, "geom")
  expect_true(output@type == "grid")
  expect_data_frame(x = output@group, nrows = 0)

  # RasterLayer with grouping
  output <- as_geometr(input, group = TRUE)
  expect_class(output, "geom")
  expect_true(output@type == "grid")
  expect_data_frame(x = output@group, nrows = 91)
})
