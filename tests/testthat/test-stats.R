
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
  expect_true(all(es == lss))
  
  
  es <- summary(flomarriage ~ absdiff("wealth"))
  lss <- calculateStatistics(flomarriage ~ absDiff("wealth"))
  expect_true(all(es == lss))
  
  es <- summary(flomarriage ~ absdiff("wealth",3))
  lss <- calculateStatistics(flomarriage ~ absDiff("wealth",3))
  expect_true(all(es == lss))
  
  })