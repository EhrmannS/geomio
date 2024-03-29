library(testthat)
library(checkmate)
library(geometr)
library(sp)
library(sf)
library(raster)
context("getFeatures")


test_that("getFeatures of a 'geom'", {
  # test non-grid geom
  # coords <- data.frame(x = c(40, 70, 70, 50),
  #                      y = c(40, 40, 60, 70),
  #                      fid = 1)
  # window <- data.frame(x = c(0, 80),
  #                      y = c(0, 80))
  # aGeom <- gs_polygon(anchor = coords, window = window)
  # output <- getFeatures(aGeom)

  expect_data_frame(output, any.missing = FALSE, nrows = 1, ncols = 2)
  expect_names(names(output), identical.to = c("fid", "gid"))

  # test grid geom
  # ... layer with attribute table
  output <- getFeatures(gtGeoms$grid$categorical)
  expect_data_frame(output, any.missing = FALSE, nrows = 3360)
  expect_names(x = names(output), permutation.of = c("fid", "gid"))

  # ... layer without attribute table
  output <- getFeatures(gtGeoms$grid$continuous)
  expect_data_frame(output, any.missing = FALSE, nrows = 3360)
  expect_names(x = names(output), permutation.of = c("fid", "gid"))
})

test_that("getFeatures of a Spatial* object", {
  # input <- gc_sp(input = gtGeoms$polygon)

  output <- getFeatures(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("fid", "gid"))
})

test_that("getFeatures of a sf object", {
  # input <- gc_sf(gtGeoms$polygon)

  output <- getFeatures(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("fid", "gid"))
})

test_that("getFeatures of any other object", {
  output <- getFeatures("bla")
  expect_null(object = output)
})
