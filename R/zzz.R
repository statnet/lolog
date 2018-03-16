


loadModule("lolog",TRUE)
.onLoad <- function(libname, pkgname){
	.C("initStats")
}

.onUnload <- function(libpath) {}



#setwd("/Users/ianfellows/Documents/Eclipse_workspace/")
#library(roxygen2)
#roxygenize('lolog',roclets='rd')
