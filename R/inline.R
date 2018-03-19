


# #' An lolog plug-in for easy C++ prototyping and access
# #' @param ... plug-in arguments
# #' @examples
# #' library(inline)
# #' registerPlugin("lolog",inlineLologPlugin)
# #' src <- "
# #'		Rcpp::IntegerMatrix tmp(0,2);
# #'		lolog::BinaryNet<lolog::Directed> net(tmp,Rcpp::as<int>(n));
# #'		return net;
# #'		"
# #' emptyNetwork <- cxxfunction(signature(n="integer"), src, plugin="lolog")
# #' net <- emptyNetwork(10)
# #' net[1:10,1:10]
# inlineLologPlugin <- Rcpp::Rcpp.plugin.maker(
# 		include.after = "#include <lolog.h>", 
# 		libs           = "",
# 		LinkingTo = unique(c("lolog","BH", "Rcpp")), 
# 		Depends = unique(c("lolog","BH", "Rcpp")), 
# 		Imports = unique(c("lolog","BH", "Rcpp")),
# 		package        = "lolog"
# )


