
#' create a skeleton for a package extending lolog
#' @param path where to create the package
lologPackageSkeleton <- function( path = "."){
	pkgPath <- find.package("lolog")
	p <- file.path(pkgPath,"examplePackage","LologExtension")
	file.copy(p,path,recursive=TRUE)
}

# Used to indicate a required parameter
.required <- function(){
	r <- NA
	class(r) <- ".requiredParam"
	r
}

# Parses evaulated parameters as if in a function call.
# using names and positional matching
# @param lis a list of parameter values
# @param params a named list of default parameters
.matchParams <- function(lis,params){
  n <- length(lis)
  nm <- names(lis)
  np <- names(params)
  if(is.null(nm)){
    params[1:n] <- lis
    names(params) <- np
    return(params)
  }
  result <- params
  mch <- pmatch(nm,np)
  result[na.omit(mch)] <- lis[!is.na(mch)]
  j <- 1
  for(i in 1:n){
    if(!is.na(mch)[i]){
      next
    }
    while(j %in% na.omit(mch))
      j <- j + 1
    if(nm[i]=="")
      result[[j]] <- lis[[i]]
    else
      stop(paste("unknown named parameter ",nm[i]))
    j <- j + 1
  }
  for(i in 1:length(result)){
  	if(inherits(result[[i]],".requiredParam"))
  		stop(paste("parameter",names(result)[i],"required but not present"))
  }
  result
}
