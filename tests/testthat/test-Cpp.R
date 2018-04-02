


library(lolog)
library(testthat)

context("C++ tests")

test_that("C++", {
  runLologCppTests()
  expect_true(TRUE)
})
