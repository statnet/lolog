#' Conduct Monte Carlo diagnostics on a lolog model fit
#' 
#' This function prints diagnostic information and creates simple diagnostic
#' plots for MC sampled statistics produced from a lolog fit.
#' 
#' A pair of plots are produced for each statistical trace of the sampled
#' output statistic values on the left and density estimate for each variable
#' in the Monte Carlo samples on the right.  Diagnostics printed to the console include
#' correlations and convergence diagnostics.
#'
#' @param object A model fit object to be diagnosed.
#' @param center Logical: If TRUE, center the samples on the
#'   observed statistics.
#'   plotting page.  Ignored if \code{latticeExtra} package is not
#'   installed.
#' @return \code{\link{mc.diagnostics.lolog}} returns some degeneracy
#' information, if it is included in the original object.  The function is
#' mainly used for its side effect, which is to produce plots and summary
#' output based on those plots.
#' @seealso \code{\link{ergm}}, \code{coda} package,
#' \code{\link{mcmc.ergm}}
#' @import ergm
#' @export
plot.lologGmm <- function(object,
                          center=TRUE,
                          type="pairs",
                          vars.per.page=3,...) {

  if(!is(object,"lologGmm")){
    stop("A lologGmm object argument must be given.")
  }

  sm <- as.matrix(object$stats)

  if(is.null(sm)) stop("Monte Carlo sampling was not run or not stored.")

  if(center){
    sm <- sweep(sm, 2, object$targetStats, "-", check.margin=TRUE)
  }

  statnames <- c("Mean", "SD")
  varstats <- matrix(nrow = ncol(sm), ncol = length(statnames), 
                     dimnames = list(colnames(sm), statnames))
  smmean <- apply(sm, 2, mean)
  smvar <- apply(sm, 2, var)
  varstats[, 1] <- smmean
  varstats[, 2] <- sqrt(smvar)
  varstats <- drop(varstats)

  cat("\nEmpirical mean and standard deviation for each Sample Statistic:\n\n")
  print(varstats, digits = max(3, .Options$digits - 3), ...)
  
  cat("\nSample statistics cross-correlations:\n")
  print(cor(sm))

  if(type == "stats"){
    pairs(sm)
  }else{
   if(requireNamespace('latticeExtra', quietly=TRUE)){  
    dp <- update(lattice::densityplot(coda::mcmc(object$stats, 1, thin=1), panel=function(...){lattice::panel.densityplot(...);lattice::panel.abline(v=0)}),xlab=NULL,ylab=NULL)

    pages <- ceiling(ncol(sm)/vars.per.page)
    # if the number of vars is less than vars.per.page, make adjustment
    if(ncol(sm)<vars.per.page){
      vars.per.page<-ncol(sm)
    }
  
    print(update(dp,layout=c(1,vars.per.page),as.table=TRUE,main="Sample statistics"))
   }else{
    message("Package latticeExtra is not installed. Falling back on coda's default MCMC diagnostic plots.")
    plot(sm,...)
   }
  }
  
  invisible()
}
