library(testthat)
library(edfr)

test_check("edfr")

test_that("some stuff", {
  expect_equal(10,10)
  expect_equal(10,10.01)
})
