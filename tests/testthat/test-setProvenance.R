library(testthat)
library(checkmate)
library(geometr)
library(sp)
library(sf)
library(raster)
context("setProvance")


test_that("set history of a geom", {
  # geom with history
  theHistory <- "geom has been modified"

  output <- setProvance(x = gtGeoms$polygon, history = theHistory)
  expect_class(x = output, classes = "geom")
  expect_list(x = output@history, len = 2, types = "character")
  expect_true(object = output@history[[2]] == "geom has been modified")

  # ... without history
  temp <- gtGeoms$polygon
  temp@history <- list()

  output <- setProvance(x = gtGeoms$polygon, history = theHistory)
  expect_class(x = output, classes = "geom")
  expect_list(x = output@history, len = 2, types = "character")
  expect_true(object = output@history[[2]] == "geom has been modified")
})

test_that("set history of a Raster", {
  # Raster without history
  input <- raster(system.file("external/rlogo.grd", package="raster"))
  theHistory <- "raster has been modified"

  output <- setProvance(x = input, history = theHistory)
  expect_class(x = output, classes = "RasterLayer")
  expect_list(x = output@history, len = 2, types = "character")
  expect_true(object = output@history[[2]] == "raster has been modified")

  # ... with history
  input@history <- list("bla")

  output <- setProvance(x = input, history = theHistory)
  expect_class(x = output, classes = "RasterLayer")
  expect_list(x = output@history, len = 2, types = "character")
  expect_true(object = output@history[[2]] == "raster has been modified")
})
