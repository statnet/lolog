

library(lolog)
library(testthat)

context("C++ tests")

test_that("C++",{
  .C("runLologTests")	
  expect_true(TRUE)
})
