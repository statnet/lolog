

#' convert and UndirectedNet to a network object
#' @param x the object
#' @param ... unused
as.network.Rcpp_UndirectedNet <- function(x,...){
	el <- x$edges()
	attr(el,"n") <- n <- x$size()
	
	if(nrow(el) > 0)
  	nw <- network(el,directed=FALSE)
	else
	  nw <- network.initialize(n, directed = FALSE)
	
	for(i in which(x$nMissing(1:n)>0)){
		nas <- which(is.na(x[i,1:n]))
		nw[i,nas] <- NA
	}
	
	vn <- x$variableNames(TRUE)
	if(length(vn)>0){
		for(i in 1:length(vn)){
			vals <- x[[vn[i]]]
			if(vn[i] == "vertex.names"){
				network.vertex.names(nw) <- as.character(vals)
			}else if(vn[i] == "na"){
				
			}else{
				nw %v% vn[i] <- if(is.factor(vals)) as.character(vals) else as.vector(vals)
			}
		}
	}
	nw
}

#' convert and DirectedNet to a network object
#' @param x the object
#' @param ... unused
as.network.Rcpp_DirectedNet <- function(x,...){
	el <- x$edges()
	attr(el,"n") <- n <- x$size()
	
	if(nrow(el) > 0)
	  nw <- network(el,directed=TRUE)
	else
	  nw <- network.initialize(n, directed = TRUE)
	
	for(i in which(x$nMissing(1:n)>0)){
		nas <- which(is.na(x[i,1:n]))
		nw[i,nas] <- NA
	}
	
	vn <- x$variableNames(TRUE)
	if(length(vn)>0){
		for(i in 1:length(vn)){
			vals <- x[[vn[i]]]
			if(vn[i] == "vertex.names"){
				network.vertex.names(nw) <- as.character(vals)
			}else if(vn[i] == "na"){
				
			}else{
				nw %v% vn[i] <- if(is.factor(vals)) as.character(vals) else as.vector(vals)
			}
		}
	}
	nw
}

#' plot an DirectedNet object
#' @param x the object
#' @param ... additional parameters for plot.network
#' @method plot Rcpp_DirectedNet
plot.Rcpp_DirectedNet <- function(x,...){
	x <- as.network(x)
	plot(x,...)
}

#' plot an UndirectedNet object
#' @param x the object
#' @param ... additional parameters for plot.network
#' @method plot Rcpp_UndirectedNet
plot.Rcpp_UndirectedNet <- function(x,...){
	x <- as.network(x)
	plot(x,...)
}

#' convert and network to either an UndirectedNet or DirectedNet object
#' @param x the object
#' @param ... unused
as.BinaryNet <- function(x,...){
	if(inherits(x,"Rcpp_UndirectedNet"))
		return(x)
	if(inherits(x,"Rcpp_DirectedNet"))
		return(x)	
	if(!inherits(x,"network"))
		stop("x must be a BinaryNet or network object")
	directed <- is.directed(x)
	el <- as.matrix(x,matrix.type="edgelist")
	n <- attr(el,"n")
	if(directed)
		net <- new(DirectedNet,el,n)
	else
		net <- new(UndirectedNet,el,n)
	vn <- list.vertex.attributes(x)
	for(v in vn)
		net[[v]] <- x %v% v
	
	net
}

#' indexing
#' @name [
#' @aliases [,Rcpp_DirectedNet-method
#' @param x object
#' @param i indices
#' @param j indices
#' @param ... unused
#' @param maskMissing should missing values be masked by NA
#' @param drop unused
#' @docType methods
#' @rdname extract-methods
setMethod("[", c("Rcpp_DirectedNet"),
		function(x, i, j, ..., maskMissing=TRUE, drop=TRUE)
		{
			x$`[`(i,j,maskMissing)
		})

#' indexing
#' @name [
#' @aliases [,Rcpp_UndirectedNet-method
#' @param x object
#' @param i indices
#' @param j indices
#' @param ... unused
#' @param maskMissing should missing values be masked by NA
#' @param drop unused
#' @docType methods
#' @rdname extract-methods
setMethod("[", c("Rcpp_UndirectedNet"),
		function(x, i, j, ..., maskMissing=TRUE, drop=TRUE)
		{
			x$`[`(i,j,maskMissing)
		})

#' indexing
#' @name [<-
#' @aliases [<-,Rcpp_DirectedNet-method
#' @param x object
#' @param i indices
#' @param j indices
#' @param ... unused
#' @param maskMissing should missing values be masked by NA
#' @param value values to assign
#' @docType methods
#' @rdname extract-methods
setMethod("[<-", c("Rcpp_DirectedNet"),
		function(x, i, j, ..., value)
		{
			if(is.vector(value)){
				if(length(value)==length(i) && length(j)==1)
					value <- as.matrix(as.logical(value))
				else if(length(value)==length(j) && length(i)==1)
					value <- t(as.matrix(as.logical(value)))
				else
					stop("invalid assignment")
			}
			x$`[<-`(i,j,value)
			x
		})

#' indexing
#' @name [<-
#' @aliases [<-,Rcpp_UndirectedNet-method
#' @param x object
#' @param i indices
#' @param j indices
#' @param ... unused
#' @param maskMissing should missing values be masked by NA
#' @param value values to assign
#' @docType methods
#' @rdname extract-methods
setMethod("[<-", c("Rcpp_UndirectedNet"),
		function(x, i, j, ..., value)
		{
			if(is.vector(value)){
				if(length(value)==length(i) && length(j)==1)
					value <- as.matrix(as.logical(value))
				else if(length(value)==length(j) && length(i)==1)
					value <- t(as.matrix(as.logical(value)))
				else
					stop("invalid assignment")
			}
			x$`[<-`(i,j,value)
			x
		})

