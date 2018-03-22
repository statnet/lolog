


loadModule("lolog",TRUE)
.onLoad <- function(libname, pkgname){
	.C("initStats")
}

.onUnload <- function(libpath) {}



#library(roxygen2)
#roxygenize('lolog',roclets='rd')
