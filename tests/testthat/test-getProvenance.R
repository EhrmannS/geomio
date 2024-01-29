library(testthat)
library(checkmate)
library(geometr)
library(sp)
library(sf)
library(raster)
context("getProvenance")


test_that("getProvenance of a geom", {
  # coords <- data.frame(x = c(40, 70, 70, 50),
  #                      y = c(40, 40, 60, 70),
  #                      fid = 1)
  # window <- data.frame(x = c(0, 80),
  #                      y = c(0, 80))
  # aGeom <- gs_polygon(anchor = coords, window = window)
  output <- getProvenance(aGeom)

  expect_list(output, any.missing = FALSE, types = "character")
  expect_true(output[[1]] == "object was created as 'polygon' geom.")
})

test_that("getProvenance of a RasterLayer", {
  # input <- gc_raster(gtGeoms$grid$categorical)
  input@history <- list("bla")

  output <- getProvenance(input)
  expect_list(output, len = 1, types = "character")
})

test_that("getProvenance of a RasteBrick", {
  # seems like I don't have a brick within this package, so I create a random one
  # input <- raster::brick(list(gc_raster(gtGeoms$grid$categorical),
  #                             gc_raster(gtGeoms$grid$continuous)))
  input@history <- list("bla")

  output <- getProvenance(input)
  expect_list(output, len = 1, types = "character")
})

test_that("getProvenance of a RasteStack", {
  # input <- raster::stack(list(gc_raster(gtGeoms$grid$categorical),
  #                             gc_raster(gtGeoms$grid$continuous)))
  input@history <- list("bla")

  output <- getProvenance(input)
  expect_list(output, len = 2, types = "character")
})

test_that("getProvenance of any other object", {
  # output <- getProvenance("bla")
  expect_null(object = output)
})
