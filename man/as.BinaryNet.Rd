% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binary-net.R
\name{as.BinaryNet}
\alias{as.BinaryNet}
\title{Convert to either an UndirectedNet or DirectedNet object}
\usage{
as.BinaryNet(x, ...)
}
\arguments{
\item{x}{the object}

\item{...}{unused}
}
\value{
either an Rcpp_UndirectedNet or Rcpp_DirectedNet object
}
\description{
Convert to either an UndirectedNet or DirectedNet object
}
\details{
Converts network objects to BinaryNets. This function also converts
other graph formats, such as igraph and tidygraph, utilizing
intergraph::asNetwork.
}
\examples{
data(ukFaculty)
net <- as.BinaryNet(ukFaculty)
net
}
