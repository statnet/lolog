



#' creates a model
#' @param formula the model formula
#' @param cloneNet create a deep copy of the network within the model object
#' @param theta the model parameters.
createCppModel <- function(formula,cloneNet=TRUE,theta=NULL){
  modelClass <- "Model"
	form <- formula
	env <- environment(form)
	net <- as.BinaryNet(eval(form[[2]],envir=env))
	if(cloneNet)
		net <- net$clone()
	terms <- .prepModelTerms(formula)
	model <- .makeCppModelFromTerms(terms, net, theta, modelClass)
	model
}


.prepModelTerms <- function(formula){
  if(is.null(formula))
    return(NULL)
  form <- formula
	env <- environment(formula)
	
	# Parse out vertex ordering (if it exists)
	vertexOrder <- integer();
	if(!is.symbol(formula[[3]]) && as.character(formula[[3]][[1]])=="|"){
	  tmp <- formula[[3]][[3]]
	  vertexOrder <- eval(tmp,envir=env)
	  if(any(is.na(vertexOrder)))
	    stop("vertex order can not have any NA values")
	  if(!is.numeric(vertexOrder))
	    stop("vertex order must be numeric")
	  form[[3]] <- formula[[3]][[2]]
	}
	
	# parse out model terms
	tmp <- form[[3]]
	lastTerm <- FALSE
	stats <- list()
	offsets <- list()
	while(!lastTerm){
		ls <- length(stats)
		lo <- length(offsets)
		term <- if(is.symbol(tmp)){
					lastTerm <- TRUE
					term <- tmp
				} else if(as.character(tmp[[1]])=="+"){
					tmp[[3]]
				}else{
					lastTerm <- TRUE
					tmp
				}
		
		name <- if(is.symbol(term)) as.character(term) else as.character(term[[1]])
		args <- NULL
		if(name=="offset" || name=="constraint"){
			term <- term[[2]]
			name <- if(is.symbol(term)) as.character(term) else as.character(term[[1]])
			if(length(term)>1){
				term[[1]] <- as.name("list")
				args <- eval(term,envir=env)
			}else{
				args <- list()
			}
			offsets[[lo+1]] <- args
			names(offsets)[lo+1] <- name
		}else{
			if(length(term)>1){
				term[[1]] <- as.name("list")
				args <- eval(term,envir=env)
			}else{
				args <- list()
			}
			stats[[ls+1]] <- args
			names(stats)[ls+1] <- name
		}
		if(!lastTerm)
			tmp <- tmp[[2]]
	}
	
	list(stats=stats,offsets=offsets,vertexOrder=vertexOrder)
}


.makeCppModelFromTerms <- function(terms, net, theta=NULL, modelClass="Model"){
	net <- as.BinaryNet(net)
	
	clss <- class(net)
	networkEngine <- substring(clss,6,nchar(clss)-3)
	ModelType <- eval(parse(text=paste("lolog::",networkEngine,modelClass,sep="")))
	
	model <- new(ModelType)
	model$setNetwork(net)
	
	stats <- rev(terms$stats)
	offsets <- rev(terms$offsets)
	
	if(length(stats)>0)
		for(i in 1:length(stats)){
			t <- try(model$addStatistic(names(stats)[i],stats[[i]]), silent=TRUE)
			if(inherits(t,"try-error")){
				to <- try(model$addOffset(names(offsets)[i],offsets[[i]]), silent=TRUE)
				if(inherits(to,"try-error"))
					stop(t)
			}
		}
	if(length(offsets)>0)
		for(i in 1:length(offsets))
			model$addOffset(names(offsets)[i],offsets[[i]])
	if(!is.null(theta))
		model$setThetas(theta)
	model$setVertexOrder(as.integer(rank(terms$vertexOrder, ties.method = "min")))
	model
	
}




#'calculate model statistics from a formula
#' @param formula An lolog formula
calculateStatistics <- function(formula){
	createCppModel(formula,cloneNet=FALSE)$statistics()
}

