

#' Creates a probability model for a latent ordered network model
#' @param formula A LOLOG formula
#' @param theta Parameter values.
#' @return 
#' An Rcpp object representing the likeilhood model
createLatentOrderLikelihood <- function(formula, theta=NULL){
  env <- environment(formula)
  net <- as.BinaryNet(eval(formula[[2]],envir=env))
  model <- createCppModel(formula)
  clss <- class(net)
  networkEngine <- substring(clss,6,nchar(clss)-3)
  LikType <- eval(parse(text=paste("lolog::",networkEngine,"LatentOrderLikelihood",sep="")))
  lik <- new(LikType, model)
  if(!is.null(theta)){
    lik$setThetas(theta)
  }
  lik
}


.createLatentOrderLikelihoodFromTerms <- function(terms, net, theta=NULL){
  net <- as.BinaryNet(net)
  model <- .makeCppModelFromTerms(terms, net, theta)
  clss <- class(net)
  networkEngine <- substring(clss,6,nchar(clss)-3)
  LikType <- eval(parse(text=paste("lolog::",networkEngine,"LatentOrderLikelihood",sep="")))
  lik <- new(LikType, model)
  if(!is.null(theta)){
    lik$setThetas(theta)
  }
  lik
}


#' Fits a latent ordered network model using maximum likelihood
#' @param formula A lolog formula
#' @param nReplicates An integer controling how many dyad ordering to perform.
#' @param downsampleRate Controls what proportion of dyads in each ordering should be dropped.
#' @param targetFrameSize Sets downsampleRate so that the model frame for the logistic regression will have on average this amount of observations.
#' @return An object of class 'lologVariationalFit'.
lologVariational <- function(formula, nReplicates=5L, downsampleRate=NULL, targetFrameSize=500000){
  
  lolik <- createLatentOrderLikelihood(formula)
  nReplicates <- as.integer(nReplicates)
  
  dyadIndependent <- lolik$getModel()$isIndependent(TRUE,TRUE)
  dyadIndependentOffsets <- lolik$getModel()$isIndependent(TRUE,FALSE)
  allDyadIndependent <- all(dyadIndependent) & all(dyadIndependentOffsets)
  if(allDyadIndependent & nReplicates != 1L){
    cat("\n Model is dyad independent. Replications are redundant. Setting nReplicates <- 1L.\n")
    nReplicates <- 1L
  }
  network <- lolik$getModel()$getNetwork()
  n <- network$size()
  ndyads <- n * (n-1)
  if(!network$isDirected())
    ndyads <- ndyads / 2
  if(is.null(downsampleRate)){
    downsampleRate <-min( 1, targetFrameSize / ndyads)
  }
  samples <- lolik$variationalModelFrame(nReplicates, downsampleRate)
  predictors <- lapply(samples,function(x) as.data.frame(x[[2]],
                                                         col.names=1:length(x[[2]])))
  predictors <- do.call(rbind, predictors)
  outcome <- do.call(c, lapply(samples, function (x) x[[1]]))
  
  logFit <- glm(outcome ~ as.matrix(predictors) - 1, family = binomial())
  theta <- logFit$coefficients
  names(theta) <- names(lolik$getModel()$statistics())
  result <- list(method="variational",
                 theta=theta,
                 vcov=vcov(logFit)*nReplicates / downsampleRate,
                 nReplicates=nReplicates,
                 downsampleRate=downsampleRate,
                 likelihoodModel=lolik,
                 outcome=outcome,
                 allDyadIndependent = allDyadIndependent,
                 predictors=predictors)
  class(result) <- c("lologVariationalFit","lolog","list")
  result
}


#' Print of a lologVariationalFit object
#' @param x the object
#' @param ... additional parameters (unused)
#' @method print lologVariationalFit
print.lologVariationalFit <- function(x, ...){
  if(x$allDyadIndependent) cat("MLE Coefficients:\n") else cat("Variational Inference Coefficients:\n")
  print(x$theta)
  if(x$downsampleRate != 1){
    cat("Downsampling rate:",x$downsampleRate,"\n")
  }
  if(!x$allDyadIndependent) cat("# of replicates:",x$nReplicates,"\n")
}


#' Print of a lolog object
#' @param x the object
#' @param ... additional parameters (unused)
#' @method print lolog
print.lolog <- function(x, ...){
  cat(x$method, "Coefficients:\n")
  print(x$theta)
}


#' Summary of a lolog object
#' @param object the object
#' @param ... additional parameters (unused)
#' @method summary lolog
summary.lolog <- function(object, ...){
  x <- object
  theta <- x$theta
  se <- sqrt(diag(x$vcov))
  pvalue <- 2 * pnorm(abs(theta / se),lower.tail = FALSE)
  stats <- x$likelihoodModel$getModel()$statistics()
  orderInd <- x$likelihoodModel$getModel()$isIndependent(FALSE,TRUE)
  stats[!orderInd] <- NA
  result <- data.frame(observed_statistics=stats, theta=theta, se=se, pvalue=round(pvalue,4))
  rownames(result) <- names(stats)
  result
}


#' Fits a LOLOG model via Generalized Method of Moments
#' @param formula A lolog formula for the sufficient statistics
#' @param auxFormula A lolog formula of statistics to use for moment matching
#' @param theta Initial parameters values
#' @param nsamp The number of sample neteworks to draw at each iteration
#' @param includeOrderIndependent If true, all order independent terms in formula are used for moment matching.
#' @param weights The type of weights to use in the GMM objective. Either 'full' for the inverse of the full covariance matrix or 'diagnoal' for the inverse of the diagonal of the covariance matrix.
#' @param tol The Hotteling's T^2 p-value tolerance for convergance for the transformed moment conditions.
#' @param nHalfSteps The maximum number of half steps to take when the objective is not improved in an interation.
#' @param maxIter The maximum number of iterations.
#' @param minIter The minimum number of iterations.
#' @param startingStepSize The starting dampening of the parameter update.
#' @param maxStepSize The largest allowed value for dampening.
#' @param cluster A parallel cluster to use for graph simulation.
#' @param verbose Level of verbosity 0-2
#' @return An object of class 'lolog'
lolog <- function(formula, auxFormula=NULL, theta=NULL, nsamp=1000, includeOrderIndependent=TRUE, 
                     weights="full", tol= .1, nHalfSteps=10, maxIter=100, minIter=2,
                     startingStepSize=.1, maxStepSize=.5, cluster=NULL,verbose=TRUE){
  
  #initialize theta via variational inference
  if(is.null(theta)){
    if(verbose) cat("Initializing using variational fit\n")
    varFit <- lologVariational(formula,downsampleRate = 1)
    if(varFit$allDyadIndependent){
      if(verbose) cat("Model is dyad independent. Returning maximum likelihood estimate.\n")
      return(varFit)
    }
    theta <- varFit$theta
    if(verbose) cat("theta:\n")
    if(verbose) print(theta)
  }
  
  lolik <- createLatentOrderLikelihood(formula, theta=theta)
  
  orderIndependent <- lolik$getModel()$isIndependent(FALSE,TRUE)
  dyadIndependent <- lolik$getModel()$isIndependent(TRUE,TRUE)
  dyadIndependentOffsets <- lolik$getModel()$isIndependent(TRUE,FALSE)
  if(all(dyadIndependent) & all(dyadIndependentOffsets)){
    if(verbose) cat("Model is dyad independent. Returning maximum likelihood estimate.\n")
    varFit <- lologVariational(formula,downsampleRate = 1)
    return(varFit)
  }
  
  terms <- .prepModelTerms(formula)
  auxTerms <- .prepModelTerms(auxFormula)
  samp <- NULL
  obsStats <- NULL
  if(!is.null(auxFormula)){
    auxModel <- createCppModel(auxFormula)
    auxModel$setNetwork(lolik$getModel()$getNetwork())
    auxModel$calculate()
    obsStats <- auxModel$statistics()
  }
  if(includeOrderIndependent){
    obsStats <- c(lolik$getModel()$statistics()[orderIndependent], obsStats)
  }
  stepSize <- startingStepSize
  lastTheta <- NULL
  lastObjective <- Inf
  hsCount <- 0
  iter <- 0
  if(!is.null(cluster)){
    clusterEvalQ(cluster, {
      library(lolog)
      library(network)
    })
    tmpNet <- lolik$getModel()$getNetwork()$clone()
    tmpNet$emptyGraph()
    network <- as.network(tmpNet)
    clusterExport(cluster, "terms", envir = environment())
    clusterExport(cluster, "auxTerms", envir = environment())
    clusterExport(cluster, "network", envir = environment())
    clusterExport(cluster, "orderIndependent", envir = environment())
    clusterExport(cluster, "includeOrderIndependent", envir = environment())
  }
  while(iter < maxIter){
    iter <- iter + 1
    
    #generate networks
    lolik$setThetas(theta)
    stats <- matrix(0,ncol=length(theta),nrow=nsamp)
    estats <- matrix(0,ncol=length(theta),nrow=nsamp)
    if(includeOrderIndependent)
      auxStats <- matrix(0,ncol=length(obsStats) - sum(orderIndependent),nrow=nsamp)
    else
      auxStats <- matrix(0,ncol=length(obsStats),nrow=nsamp)
    if(is.null(cluster)){
      for(i in 1:nsamp){
        if(verbose) cat(".")
        samp <- lolik$generateNetwork()
        if(!is.null(auxFormula)){
          auxModel$setNetwork(samp$network)
          auxModel$calculate()
          auxStats[i,] <- auxModel$statistics()
        }
        stats[i,] <- samp$stats + samp$emptyNetworkStats
        estats[i,] <- samp$expectedStats + samp$emptyNetworkStats
      }
      if(includeOrderIndependent)
        auxStats <- cbind(stats[,orderIndependent], auxStats)
      
      if(verbose) cat("\n")
    }else{
      workingNetwork <- as.network(lolik$getModel()$getNetwork())
      worker <- function(i, theta){
        cat(i," ")
        network <- as.BinaryNet(network)
        lolik <- lolog:::.createLatentOrderLikelihoodFromTerms(terms, network, theta)
        samp <- lolik$generateNetwork()
        if(!is.null(auxTerms)){
          auxModel <- lolog:::.makeCppModelFromTerms(auxTerms, network)
          auxModel$setNetwork(samp$network)
          auxModel$calculate()
          as <- auxModel$statistics()
        }else{
          as <- numeric()
        }
        list(stats=samp$stats + samp$emptyNetworkStats,
             estats = samp$expectedStats + samp$emptyNetworkStats,
             auxStats = as)
      }
      results <- parallel::parLapply(cluster, 1:nsamp, worker, theta=theta)
      stats <- t(sapply(results, function(x) x$stats))
      estats <- t(sapply(results, function(x) x$estats))
      if(!is.null(auxFormula))
        auxStats <- t(sapply(results, function(x) x$auxStats))
      else
        auxStats <- NULL
      if(includeOrderIndependent)
        auxStats <- cbind(stats[,orderIndependent], drop(auxStats))
    }
    
    #calculate gradient of moment conditions
    grad <- matrix(0,ncol=length(theta),nrow=length(obsStats))
    for(i in 1:length(obsStats)){
      for(j in 1:length(theta)){
        grad[i,j] <- -(cov(auxStats[,i], stats[,j]) - cov(auxStats[,i], estats[,j]))
      }
    }
    
    if(weights == "diagonal")
      W <- diag( 1 / (diag(var(auxStats))) )
    else
      W <- solve(var(auxStats))
    
    #cacluate moment conditions and stat/observed stat differences transformed by W.
    mh <- colMeans(auxStats)
    diffs <- -sweep(auxStats, 2, obsStats)
    transformedDiffs <- t(t(grad) %*% W %*% t(diffs))
    momentCondition <- colMeans(transformedDiffs)
    
    objective <- colMeans(diffs) %*% W %*% colMeans(diffs)
    if(verbose) cat("Objective:\n")
    if(verbose) print(objective)
    objCrit <- max(-1000000, objective - lastObjective) / (lastObjective + 1)
    
    if(verbose) cat("Moment Conditions:\n")
    if(verbose) print(momentCondition)
    
    
    # Calculate inverse
    invFailed <- inherits(try(gradInv <- solve(t(grad) %*% W %*% grad),silent = TRUE),"try-error")
    
    if(verbose > 1) pairs(stats)
    
    # If inverse failed, or the objective has increased singificantly, initiate half stepping
    if(hsCount < nHalfSteps && !is.null(lastTheta) && (invFailed || objCrit > .3)){
      if(verbose) cat("Half step back\n")
      theta <- (lastTheta + theta) / 2
      hsCount <- hsCount + 1
      stepSize <- stepSize / 2
      if(verbose) cat("Theta:\n")
      if(verbose) print(theta)
      next
    }else{
      stepSize <- min(maxStepSize, stepSize * 1.25)
      hsCount <- 0
    }
    if(verbose) print(stepSize)
    
    #Update theta
    lastTheta <- theta
    theta <- theta - stepSize * gradInv %*% momentCondition
    lastObjective <- objective
    
    if(verbose) print("auxStat Diffs:")
    if(verbose) print(colMeans(diffs) / sqrt(diag(var(diffs))))
    
    #Hotelling's T^2 test
    hotT <- momentCondition %*% solve(var(transformedDiffs)/nrow(transformedDiffs)) %*% momentCondition
    pvalue <- pchisq(hotT,df=length(theta), lower.tail = FALSE)
    if(verbose) cat("Hotelling's T2 p-value: ",pvalue,"\n")
    if(verbose) cat("Theta:\n")
    if(verbose) print(theta)
    if(pvalue > tol && iter >= minIter){
      break
    }else if(iter < maxIter){
      
    }
  }
  
  if(is.null(samp)){
    samp <- lolik$generateNetwork()
  }
  
  omega <- var(auxStats)
  vcov <- solve(t(grad) %*% W %*% grad) %*% 
    t(grad) %*% W %*% omega %*% t(W) %*% grad %*% 
    solve(t(grad) %*% t(W) %*% grad) 
  
  lastTheta <- drop(lastTheta)
  names(lastTheta) <- names(lolik$getModel()$statistics())
  method <- if(is.null(auxFormula)) "Method of Moments" else "Generalized Method Of Moments"
  
  result <- list(method=method,
                 theta=lastTheta,
                 stats=stats,
                 estats=estats, 
                 auxStats=auxStats,
                 obsStats=obsStats,
                 net=samp$network,
                 grad=grad, 
                 vcov=vcov, 
                 likelihoodModel=lolik)
  class(result) <- c("lologGmm","lolog","list")
  result
}
