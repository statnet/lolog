# TODO: Add comment
# 
# Author: ianfellows
###############################################################################


library(lolog)
library(network)

context("C++ tests")

test_that("C++",{
	.C("runErnmTests")	
	NULL
})
