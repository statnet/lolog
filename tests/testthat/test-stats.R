
library(lolog)
library(testthat)
library(ergm)
library(network)
context("stat tests")

test_that("Stats", {
  
  data(ukFaculty)
  data(florentine)
  es <- summary(ukFaculty ~ nodefactor("GroupC",4))
  lss <- calculateStatistics(ukFaculty ~ nodeFactor("GroupC"))
  expect_true(all(es == lss))
  
  
  es <- summary(ukFaculty ~ mutual)
  lss <- calculateStatistics(ukFaculty ~ mutual)
  
  })