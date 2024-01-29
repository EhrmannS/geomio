library(testthat)
library(checkmate)
library(geometr)
library(sp)
library(sf)
library(raster)
context("as_terra")


test_that("transform geom to SpatRaster", {
  # test a single RasterLayer
  # output <- as_terra(input = gtGeoms$grid$categorical)
  expect_class(x = output, classes = "SpatRaster")
  expect_list(x = levels(output), any.missing = FALSE)
  expect_data_frame(x = levels(output)[[1]], nrows = 9, ncols = 2)
})

test_that("errors when transforming a geom not of type 'grid'", {

  # expect_error(object = as_terra(gtGeoms$polygon))
})
