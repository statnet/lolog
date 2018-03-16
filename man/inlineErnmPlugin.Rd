\name{inlineErnmPlugin}
\alias{inlineErnmPlugin}
\title{An lolog plug-in for easy C++ prototyping and access}
\usage{
  inlineErnmPlugin(...)
}
\arguments{
  \item{...}{plug-in arguments}
}
\description{
  An lolog plug-in for easy C++ prototyping and access

  The inline plug-in for lolog
}
\examples{
library(inline)
registerPlugin("lolog",inlineErnmPlugin)
src <- "
		Rcpp::IntegerMatrix tmp(0,2);
		lolog::BinaryNet<lolog::Directed> net(tmp,Rcpp::as<int>(n));
		return net;
		"
emptyNetwork <- cxxfunction(signature(n="integer"), src, plugin="lolog")
net <- emptyNetwork(10)
net[1:10,1:10]
}

