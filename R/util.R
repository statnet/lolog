

#' Create a skeleton for a package extending lolog
#' @param path where to create the package
#' @seealso \code{\link{inlineLologPlugin}}
#' @details
#' lolog is a modular package, and can be extended at 
#' both the R and C++ level. This function will build a package
#' skeleton that can be used as a starting point for
#' development. To create the package in the current directory
#' run:
#' 
#' \code{lologPackageSkeleton()}
#' 
#' Build and install the package from the command line with
#' 
#' \code{R CMD build LologExtension}
#' 
#' \code{R CMD INSTALL LologExtension_1.0.tar.gz}
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' #install package
#' lologPackageSkeleton()
#' system("R CMD build LologExtension")
#' system("R CMD INSTALL LologExtension_1.0.tar.gz")
#' 
#' library(LologExtension) #Load package
#' 
#' # Run model with new minDegree statistic
#' library(network)
#' m <- matrix(0,20,20)
#' for(i in 1:19) for(j in (i+1):20) m[i,j] <- m[j,i] <- rbinom(1,1,.1)
#' g <- network(m, directed=FALSE)
#' fit <- lologVariational(g ~ edges() + minDegree(1L))
#' summary(fit)
#' 
#' }
lologPackageSkeleton <- function(path = ".") {
  pkgPath <- find.package("lolog")
  p <- file.path(pkgPath, "examplePackage", "LologExtension")
  file.copy(p, path, recursive = TRUE)
}

# Used to indicate a required parameter
.required <- function() {
  r <- NA
  class(r) <- ".requiredParam"
  r
}

# Parses evaulated parameters as if in a function call.
# using names and positional matching
# @param lis a list of parameter values
# @param params a named list of default parameters
.matchParams <- function(lis, params) {
  n <- length(lis)
  nm <- names(lis)
  np <- names(params)
  if (is.null(nm)) {
    params[1:n] <- lis
    names(params) <- np
    return(params)
  }
  result <- params
  mch <- pmatch(nm, np)
  result[na.omit(mch)] <- lis[!is.na(mch)]
  j <- 1
  for (i in 1:n) {
    if (!is.na(mch)[i]) {
      next
    }
    while (j %in% na.omit(mch))
      j <- j + 1
    if (nm[i] == "")
      result[[j]] <- lis[[i]]
    else
      stop(paste("unknown named parameter ", nm[i]))
    j <- j + 1
  }
  for (i in 1:length(result)) {
    if (inherits(result[[i]], ".requiredParam"))
      stop(paste("parameter", names(result)[i], "required but not present"))
  }
  result
}

' @describeIn lolog extracts estimated model coefficients.
#' 
#' @param object {an `lolog` object.}
#' @examples
#' \donttest{
#' # Extract parameter estimates as a numeric vector:
#' coef(fit)
#' }
#' @import stats
#' @importFrom stats coef
#' @export
coef.lolog <- function(object, ...){object$theta}
