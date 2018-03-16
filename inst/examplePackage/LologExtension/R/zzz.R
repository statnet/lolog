.onLoad <- function(libname, pkgname){
	# Run registerMinDegree in MinDegree.cpp to allow
	# MinDegree to be used in formula
	.C("registerMinDegree")
}

# #You can now use the new MinDegree term e.g.:
# library(ergm)
# data(flo)
# nflo<-as.BinaryNet(network(flo,directed=FALSE) )
# fit <- lolog(nflo ~ edges() + minDegree(3),verbose=0)
# summary(fit)
