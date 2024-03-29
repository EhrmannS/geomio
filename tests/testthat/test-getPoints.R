library(testthat)
library(checkmate)
library(geometr)
library(sp)
library(sf)
library(raster)
context("getPoints")


test_that("getPoints of a 'geom'", {
  # coords <- data.frame(x = c(40, 70, 70, 50),
  #                      y = c(40, 40, 60, 70),
  #                      fid = 1)
  # window <- data.frame(x = c(0, 80),
  #                      y = c(0, 80))
  # input <- gs_polygon(anchor = coords, window = window)

  output <- getPoints(x = input)
  expect_data_frame(output, any.missing = FALSE, nrows = 5, ncols = 3)
  expect_names(names(output), identical.to = c("x", "y", "fid"))
})

test_that("getPoints of a Spatial* object", {
  # input1 <- gc_sp(input = gtGeoms$point)
  # input2 <- gc_sp(input = gtGeoms$polygon)

  # point should have as many coordinates as points
  output <- getPoints(x = input1)
  expect_data_frame(output, any.missing = FALSE, nrows = 9, ncols = 3)
  expect_names(names(output), identical.to = c("x", "y", "fid"))

  # polygon should have one point duplicated, hence, 5 times as many points as features
  output <- getPoints(x = input2)
  expect_data_frame(output, any.missing = FALSE, nrows = 11, ncols = 3)
  expect_names(names(output), identical.to = c("x", "y", "fid"))
})

test_that("getPoints of an sf object", {
  # input <- gc_sf(gtGeoms$polygon)

  output <- getPoints(x = input)
  expect_data_frame(output, any.missing = FALSE, nrows = 11, ncols = 3)
  expect_names(names(output), identical.to = c("x", "y", "fid"))
})

test_that("Error if arguments have wrong value", {
  # input <- st_sf(st_sfc(st_geometrycollection(list(st_point(1:2))),
  #                       st_geometrycollection(list(st_linestring(matrix(1:4,2))))))

  expect_error(getPoints(x = input))
})

test_that("getPoints of any other object", {
  output <- getPoints("bla")
  expect_null(object = output)
})
