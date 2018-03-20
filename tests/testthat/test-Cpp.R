

library(lolog)
library(testthat)

context("C++ tests")

test_that("C++",{
	.C("runLologTests")	
	NULL
})
