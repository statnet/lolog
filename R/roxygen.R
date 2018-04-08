

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
#' lolog package. They are designed for algorithmic performance, and are thin wrappers
#' for an underlying C++ object. These network objects can be passed back and forth between
#' R and C++ with little overhead. Because they are pointers to C++ objects, serialization
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
#' @source The data set was originally reported by Nepusz et. al. (2008) and was
#' subsequently processed and included by the igraphdata package. We have simply
#' converted their network from an igraph to a network object.
#' @keywords datasets
NULL

#' Collaboration Relationships Among Partners at a New England Law Firm
#' 
#' This data set comes from a network study of corporate law partnership that was 
#' carried out in a Northeastern US corporate law firm, referred to as SG&R, 1988-1991 
#' in New England.
#' @usage
#' data(lazega)
#' @docType data
#' @name lazega
#' @aliases lazega
#' @section Licenses and Citation: CC BY 4.0. When publishing results obtained using this
#' data set, the original authors (Lazega, 2001)
#' should be cited, along with this \code{R} package.
#' @references
#' Lazega, Emmanuel (2001), The Collegial Phenomenon: The Social Mechanisms of Cooperation 
#' among Peers in a Corporate Law Partnership, Oxford: Oxford University Press
#' @source 
#' See \url{http://elazega.fr/?page_id=609} and \url{https://www.stats.ox.ac.uk/~snijders/siena/Lazega_lawyers_data.htm}
#' @keywords datasets
NULL

#' Internal Symbols
#' @name call-symbols
#' @description Internal symbols used to access compiles code.
#' @docType methods
#' @aliases _lolog_initStats _rcpp_module_boot_lolog initLologStatistics runLologCppTests
NULL

#' LOLOG Model Terms
#' @name lolog-terms
#' @docType methods
#' @section Statistic Descriptions:
#' \itemize{
#' \item{\code{edges}  (dyad-independent)  (order-independent)  (directed)  (undirected)}{ 
#' \emph{Edges:} This term adds one network statistic equal to the number of edges 
#' (i.e. nonzero values) in the network. }
#' 
#' \item{\code{ star(k, direction="in")  } (order-independent) (directed)  (undirected)}{ 
#' The \code{k} argument is a vector of distinct integers. 
#' This term adds one network statistic to the model for each element in \code{k}. 
#' The \eqn{i}th such statistic counts the number of distinct \code{k[i]}-stars in the network, 
#' where a \eqn{k}-star is defined to be a node \eqn{N} and a set of \eqn{k} different nodes 
#' \eqn{\{O_1, \dots, O_k\}} such that the ties \eqn{\{N, O_i\}} exist for \eqn{i=1, \dots, k}. 
#' For directed networks, direction indicates whether the count is of in-stars (direction="in") 
#' or out-stars (direction="out")}
#' 
#' \item{\code{triangles()} (order-independent) (directed)  (undirected)}{ 
#' This term adds one statistic to the model equal to the number of triangles
#' in the network. For an undirected network, a triangle is defined to be any
#' set \eqn{\{(i,j), (j,k), (k,i)\}} of three edges. For a directed network, a
#' triangle is defined as any set of three edges \eqn{(i{\rightarrow}j)}{(i,j)}
#' and \eqn{(j{\rightarrow}k)}{(j,k)} and either \eqn{(k{\rightarrow}i)}{(k,i)}
#' or \eqn{(k{\leftarrow}i)}{(i,k)}. }
#' 
#' 
#' \item{\code{clustering()}  (order-independent) (undirected)}{ 
#' The global clustering coefficient, defined as the number of triangles over the
#' number of possible triangles \url{https://en.wikipedia.org/wiki/Clustering_coefficient}, or
#' 3 * triangles / 2-stars.
#' }
#' 
#' \item{\code{transitivity()}  (order-independent) (undirected)}{ 
#' The Soffer-Vazquez   transitivity. This is clustering metric that adjusts for large degree
#' differences and is described by C in Equation 6 of 
#' \url{https://pdfs.semanticscholar.org/7af5/f8c871d99b868cd0ed70c5fd09f59b399769.pdf}. Note 
#' The approximation of the number of possible shared neighbors between node i and j of min(d_i,d_j) - 1
#' in this implementation.
#'  }
#'  
#' \item{\code{ mutual() } (order-independent) (directed)}{ A count of the number of pairs of actors 
#' \eqn{i} and \eqn{j} for which \eqn{(i{\rightarrow}j)}{(i,j)} and \eqn{(j{\rightarrow}i)}{(j,i)} 
#' both exist. 
#' }
#' \item{\code{ nodeMatch(name) } (dyad-independent)  (order-independent)  (directed)  (undirected)}{ 
#'  For categorical network nodal variable 'name,' the number of edges between nodes with the same 
#'  variable value.
#'  }
#' \item{\code{ nodeMix(name) } (dyad-independent)  (order-independent)  (directed)  (undirected)}{ 
#' For categorical network nodal variable 'name,' adds one statistic for each combination of levels of the
#' variable equal to the count of edges between those levels.
#'   }
#' \item{\code{ degree(d, direction="undirected", lessThanOrEqual=FALSE) } (order-independent)  (directed)  
#' (undirected)}{ 
#' The \code{d} argument is a vector of distinct integers. This term adds one
#' network statistic to the model for each element in \code{d}; the \eqn{i}th
#' such statistic equals the number of nodes in the network of degree
#' \code{d[i]}, i.e. with exactly \code{d[i]} edges. 
#' 
#' For directed networks if direction="undirected"
#' degree is counted as the sum of the in and out degrees of a node. If direction="in" then in-degrees are
#' used and direction="out" indicates out-degrees.
#' 
#' If lessThanOrEqual=TRUE, then the count is the number of nodes with degree less than or equal to d.
#'   }
#'   
#'   
#' \item{\code{ degreeCrossProd() (order-independent)  (undirected) }}{ 
#'     This term adds one network statistic equal to the mean of the cross-products
#'     of the degrees of all pairs of nodes in the network which are tied.
#'   }
#'   
#' \item{\code{ nodeCov(name) } (dyad-independent)  (order-independent)  (directed)  (undirected)}{ 
#' The \code{attrname} argument is a character string giving the name of a
#' numeric attribute in the network's vertex attribute list.
#' This term adds a single network statistic to the model equaling the sum of
#' \code{name(i)} and \code{name(j)} for all edges \eqn{(i,j)} in the
#' network. For categorical variables, levels are coded as 1,..,nlevels`.
#'   }
#'   
#' \item{\code{gwesp(alpha)}  (order-independent)  (directed)  (undirected)}{ 
#' This term is just like \code{gwdsp} except it adds a statistic equal to the
#' geometrically weighted \emph{edgewise} (not dyadwise) shared partner
#' distribution with decay parameter
#' \code{alpha} parameter, which should be non-negative.
#'   }
#'
#' \item{\code{ gwdegree(alpha, direction="undirected") }  (order-independent)  (directed)  (undirected)}{ 
#' This term adds one network statistic to the model equal to the weighted
#' degree distribution with decay controlled by the \code{decay} parameter.
#' The \code{alpha} parameter is the same as theta_s in equation (14) in Hunter (2007).
#' 
#' For directed networks if direction="undirected" degree is counted as the sum of the in and 
#' out degrees of a node. If direction="in" then in-degrees are used ans direction="out" 
#' indicates out-degrees.
#' 
#'  }
#'  
#' \item{\code{ gwdsp(alpha) } (order-independent)  (directed)  (undirected)}{ 
#' 
#'  This term adds one network statistic to the model equal to the geometrically
#'  weighted dyadwise shared partner distribution with decay parameter
#'  \code{decay} parameter, which should be non-negative. 
#' 
#'  }
#'  
#' \item{\code{ esp(d) } (order-independent)  (directed)  (undirected)}{ 
#' 
#' This term adds one network
#' statistic to the model for each element in \code{d} where the \eqn{i}th such
#' statistic equals the number of \emph{edges} (rather than dyads) in the
#' network with exactly \code{d[i]} shared partners. This term can be used with
#' directed and undirected networks. For directed networks the count is over
#' cycles of the form: node --> nbr1 --> nbr2 --> node.
#' 
#'  }
#'  
#' \item{\code{ geoDist(long, lat, distCuts=Inf) }(dyad-independent)  (order-independent)  (undirected)}{  
#' 
#' given nodal variables for longitude and latitude, calculates the sum of the
#' great circle distance between connected nodes. distCuts splits this into
#' separate statistics that count the sum of the minimum of the cut point and the
#' distance.
#' 
#' }
#' \item{\code{ dist(names } (dyad-independent)  (order-independent) (undirected)}{ 
#' 
#' Calculates a statistic equal to the sum of the euclidean distances between
#' connected nodes on the numeric nodal variables specified in names.
#' 
#' }
#' \item{\code{ preferentialAttachment(k=1) } (undirected)}{ 
#' 
#' An order dependent preferential attachment term. For each edge, adds
#' 
#' log( (k+degree) / (n * (meanDegree + k)))
#' 
#' where degree is the current degree of the acting node, n is the network size, and meanDegree is
#' the mean degree of the network. This depends upon the order in which edges are added.
#' 
#'  }
#' \item{\code{ sharedNbrs(k=1) } (undirected)}{ 
#' 
#' for each edge adds
#' 
#' log(k + shared / minDeg)
#' 
#' where shared is the current number of shared neighbors between the two nodes, and
#' minDeg is the minimum of the current degrees of the two nodes (i.e. the number of possible shared 
#' neighbors).
#' 
#'  }
#' \item{\code{ nodeLogMaxCov(name) } (order-independent)  (undirected)}{ 
#' 
#' For each edge (i,j) and nodal variable variable, add to the statistic
#' 
#' log(max(variable[i],variable[j]))
#' 
#' If the variable is a (partial) rank order of nodal inclusion into the network,
#' this statistic can be useful in modeling the mean degree over the course of the
#' growth process.
#' 
#'  }
#'  
#'  
#'  
#' \item{\code{ nodeFactor(name, direction="undirected") } (order-independent)  (undirected) (directed)}{ 
#' 
#' The \code{name} argument is a character vector giving
#' one or more names of categorical attributes in the network's vertex
#' attribute list. This term adds multiple network statistics to the
#' model, one for each of (a subset of) the unique values of the
#' \code{attrname} attribute (or each combination of the attributes
#' given). Each of these statistics gives the number of times a node
#' with that attribute or those attributes appears in an edge in the
#' network. In particular, for edges whose endpoints both have the same
#' attribute values, this value is counted twice.  For directed networks, if direction="in" then in-edges are
#' used and direction="out" indicates out-edges.
#' 
#'  }
#'  
#'  
#' \item{\code{ absDiff(name, power=1) } (order-independent)  (undirected) (directed)}{ 
#' 
#' The \code{name} argument is a character string giving the name
#' of one or mode quantitative attribute in the network's vertex attribute
#' list. This term adds one network statistic to the model equaling the
#' sum of \code{sum(abs(name[i]-name[j])^pow)} for all edges (i,j)
#' in the network. 
#' 
#'  }
#'  
#'  }
#'  
#' @section Constraint Descriptions:
#' \itemize{
#' \item{\code{boundedDegree(lower,upper)}  (order-independent)  (undirected)}{ 
#' Adds a constraint that the degrees for the network must be between lower and upper.
#'   }
#' 
#' }
#' 
NULL

