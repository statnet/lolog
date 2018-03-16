


#' An lolog plug-in for easy C++ prototyping and access
#' @param ... plug-in arguments
#' @examples
#' library(inline)
#' registerPlugin("lolog",inlineErnmPlugin)
#' src <- "
#'		Rcpp::IntegerMatrix tmp(0,2);
#'		lolog::BinaryNet<lolog::Directed> net(tmp,Rcpp::as<int>(n));
#'		return net;
#'		"
#' emptyNetwork <- cxxfunction(signature(n="integer"), src, plugin="lolog")
#' net <- emptyNetwork(10)
#' net[1:10,1:10]
inlineErnmPlugin <- Rcpp:::Rcpp.plugin.maker(
		include.before = "#include <lolog.h>", 
		libs           = "", 
		package        = "lolog"
)
