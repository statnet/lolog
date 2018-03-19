# TODO: Add comment
# 
# Author: ianfellows
###############################################################################


library(lolog)

context("C++ tests")

test_that("C++",{
	.C("runLologTests")	
	NULL
})
