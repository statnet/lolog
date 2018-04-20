
library(lolog)
library(testthat)
library(ergm)
library(network)
context("stat tests")

test_that("Stats", {
  
  data(ukFaculty)
  data(florentine)
  data(lazega)
  
  Group <- ukFaculty %v% "Group"
  Group[is.na(Group)] <- 4
  ukFaculty %v% "Group" <- Group
  ukFaculty %v% "GroupC" <- as.character(Group)
  
  es <- summary(ukFaculty ~ nodefactor("GroupC",4))
  lss <- calculateStatistics(ukFaculty ~ nodeFactor("GroupC"))
  expect_true(all(es == lss))
  
  
  es <- summary(ukFaculty ~ mutual)
  lss <- calculateStatistics(ukFaculty ~ mutual)
  expect_true(all(es == lss))
  
  
  wabsdiff<-outer(flomarriage %v% "wealth", flomarriage %v% "wealth",function(x,y){abs(x-y)})
  es <- summary(flomarriage ~ edgecov(wabsdiff))
  lss <- calculateStatistics(flomarriage ~ edgeCov(wabsdiff))
  expect_true(all(es == lss))
  
  es <- summary(flomarriage ~ absdiff("wealth"))
  lss <- calculateStatistics(flomarriage ~ absDiff("wealth"))
  expect_true(all(es == lss))
  
  es <- summary(flomarriage ~ absdiff("wealth",3))
  lss <- calculateStatistics(flomarriage ~ absDiff("wealth",3))
  expect_true(all(es == lss))
  
  lss <- calculateStatistics(lazega ~ degree(0:10) + esp(0:10, 1))
  expect_equivalent(lss, c(2, 3, 2, 4, 2, 4, 4, 1, 1, 5, 1, 5, 16, 29, 17, 23, 
                           11, 10, 4, 0, 0, 0))
  
  lss <- calculateStatistics(ukFaculty ~ degree(direction="in", d=0:10) + degree(0:10, direction="out"))
  expect_equivalent(lss, c(0, 1, 3, 0, 5, 4, 10, 7, 7, 11, 5, 1, 3, 5, 7, 6, 
                           4, 9, 4, 4, 1, 5))
  
  })
  
