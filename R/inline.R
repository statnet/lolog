


#' An lolog plug-in for easy C++ prototyping and access
#' @param ... plug-in arguments
#' @examples
#' src <- "
#' lolog::BinaryNet<lolog::Directed> makeEmptyNetwork(const int n){
#' Rcpp::IntegerMatrix tmp(0,2);
#' lolog::BinaryNet<lolog::Directed> net(tmp, n);
#' return net;
#' }
#' "
#' Rcpp::registerPlugin("lolog",inlineLologPlugin)
#' emptyNetwork <- cppFunction(src,plugin="lolog")
#' net <- emptyNetwork(10L)
#' net[1:10,1:10]
inlineLologPlugin <- Rcpp::Rcpp.plugin.maker(
  include.after = "#include <lolog.h>", 
  LinkingTo = unique(c("lolog","BH", "Rcpp")), 
  Depends = unique(c("lolog","BH", "Rcpp")), 
  Imports = unique(c("lolog","BH", "Rcpp")),
  package        = "lolog"
)
