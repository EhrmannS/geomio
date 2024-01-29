library(testthat)
library(checkmate)
library(geometr)
library(sp)
library(sf)
library(raster)
context("getRows")


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
