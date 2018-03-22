
#' initialize model statistics
#' @name initLologStatistics
#' @aliases initLologStatistics
#' @usage initLologStatistics()
NULL


#' The inline plug-in for lolog
#' @name inlineLologPlugin
#' @aliases inlineLologPlugin
#' @usage inlineLologPlugin
NULL

#' Register Statistics
#' @name registerDirectedStatistic
#' @aliases registerDirectedStatistic registerUndirectedStatistic  
#' registerDirectedOffset 
#' registerUndirectedOffset
#' @usage registerDirectedStatistic
NULL

#' Models
#' @name LologModels
#' @docType class
#' @aliases DirectedModel UndirectedModel 
#' Rcpp_DirectedModel-class Rcpp_UndirectedModel-class
NULL

#' BinaryNet
#' @name BinaryNet
#' @docType class
#' @details 
#' Rcpp_DirectedNet and Rcpp_UndirectedNet are the native network classes for the
#' lolog package. They are designed for algortihmic performance, and are thin wrappers
#' for an underlying C++ object. These network objects can be passed back and forth between
#' R and C++ with little overhaed. Because they are pointers to C++ objects, serialization
#' via 'save' or 'dput' are not supported
#' @aliases DirectedNet UndirectedNet Rcpp_DirectedNet-class Rcpp_UndirectedNet-class
NULL

#' LatentOrderLikelihood
#' @name LatentOrderLikelihood
#' @docType class
#' @aliases DirectedLatentOrderLikelihood UndirectedLatentOrderLikelihood Rcpp_DirectedLatentOrderLikelihood-class Rcpp_UndirectedLatentOrderLikelihood-class
NULL


#' Friendship network of a UK university faculty
#' 
#' The personal friendship network of a faculty of a UK university, 
#' consisting of 81 vertices (individuals) and 817 directed and weighted connections. 
#' The school affiliation of each individual is stored as a vertex attribute. 
#' @usage
#' data(ukFaculty)
#' @docType data
#' @name ukFaculty
#' @aliases ukFaculty
#' @section Licenses and Citation: When publishing results obtained using this
#' data set, the original authors (Nepusz T., Petroczi A., Negyessy L., Bazso F. 2008)
#' should be cited, along with this \code{R} package.
#' @references
#' 
#' Nepusz T., Petroczi A., Negyessy L., Bazso F.: Fuzzy communities and the 
#' concept of bridgeness in complex networks. Physical Review E 77:016107, 2008.
#' @source The data set was origenally reported by Nepusz et. al. (2008) and was
#' subsequently processed and included by the igraphdata package. We have simply
#' converted their network from an igraph to a network object.
#' @keywords datasets
NULL



#' LOLOG Model Terms
#' @name lolog-terms
#' @docType methods
#' @details 
#' A place holder for model term documentation.
NULL

