


#' Creates a probability model for a latent ordered network model
#'
#'
#' @param formula A LOLOG formula. See \code{link{lolog}}
#' @param theta Parameter values.
#'
#'
#' @return
#' An Rcpp object representing the likeilhood model
#'
#'
#' @details
#' # See the methods of the objects returned by this function
#' UndirectedLatentOrderLikelihood
#' DirectedLatentOrderLikelihood
#'
#' # A Barabasi-Albert type graph model with 1000 vertices
#' el <- matrix(0, nrow=0, ncol=2)
#' net <- new(UndirectedNet, el, 1000L)
#' lolik <- createLatentOrderLikelihood(net ~ preferentialAttachment(), theta=1)
#' banet <- lolik$generateNetwork()$network # generate a random network from the model
#' degrees <- banet$degree(1:1000)
#' hist(degrees, breaks=100) # plot the degree distribution
#' banet[["__order__"]] # The vertex inclusion order
createLatentOrderLikelihood <- function(formula, theta = NULL) {
  env <- environment(formula)
  net <- as.BinaryNet(eval(formula[[2]], envir = env))
  model <- createCppModel(formula)
  clss <- class(net)
  networkEngine <- substring(clss, 6, nchar(clss) - 3)
  LikType <-
    eval(parse(
      text = paste("lolog::", networkEngine, "LatentOrderLikelihood", sep = "")
    ))
  lik <- new(LikType, model)
  if (!is.null(theta)) {
    lik$setThetas(theta)
  }
  lik
}


.createLatentOrderLikelihoodFromTerms <-
  function(terms, net, theta = NULL) {
    net <- as.BinaryNet(net)
    model <- .makeCppModelFromTerms(terms, net, theta)
    clss <- class(net)
    networkEngine <- substring(clss, 6, nchar(clss) - 3)
    LikType <-
      eval(parse(
        text = paste("lolog::", networkEngine, "LatentOrderLikelihood", sep = "")
      ))
    lik <- new(LikType, model)
    if (!is.null(theta)) {
      lik$setThetas(theta)
    }
    lik
  }


#' Fits a latent ordered network model using Monte Carlo variational inference
#'
#'
#' @param formula A lolog formula. See \code{link{lolog}}
#' @param nReplicates An integer controling how many dyad ordering to perform.
#' @param dyadInclusionRate Controls what proportion of dyads in each ordering should be dropped.
#' @param targetFrameSize Sets dyadInclusionRate so that the model frame for the logistic regression will have on average this amount of observations.
#'
#'
#' @details
#' This function approximates the maximum liklihood solution via a variational inference on the
#' graph (y) over the latent edge variable inclusion order (s). Specifically, it replaces
#' the conditional probability p(s | y) by p(s). If the LOLOG model contains only dyad independent
#' terms, then these two probabilities are identical, and thus variational inference is
#' exactly maximum likelihood inference. The objective function is
#'
#' \deqn{E_{p(s)}\bigg(\log p(y| S, \theta) \bigg)}
#'
#' This can be approximated by drawing samples from p(s) to approximate the expectation. The
#' number of samples is controled by the nReplaices parameter. The memory required is on the
#' order of nReplicates * (# of dyads). For large networks this can be impractical, so
#' adjusting dyadInclusionRate allows one to downsample the # of dyads in each replicate.
#'
#' If the model is dyad independent, replicates are reducntand, and so nReplicates is set to
#' 1 with a note.
#'
#' The functional form of the objective function is equivalent to logistic regression, and so
#' the \code{\link{glm}} function is used to maximize it. The asymptotic covariance of the parameter
#' estimates is calculated using the methods of Westling (2015).
#'
#'
#' @return An object of class c('lologVariationalFit','lolog','list') consisting of the following
#' items:
#' \item{formula}{ The model formula}
#' \item{method}{"variational"}
#' \item{theta}{The fit parameter values}
#' \item{vcov}{The asymptotic covariance matrix for the parameter values.}
#' \item{nReplicates}{The number of replicates}
#' \item{dyadInclusionRate}{The rate at which dyads are included}
#' \item{allDyadIndependent}{Logical indicating model dyad independence}
#' \item{likelihoodModel}{An object of class *LatentOrderLikelihood at the fit parameters}
#' \item{outcome}{The outcome vector for the logistic regression}
#' \item{predictors}{The change statistic predictor matrix for the logistic rergession}
#'
#'
#' @examples
#' data(ukFaculty)
#' fit <- lologVariational(ukFaculty ~ edges() + nodeMatch("GroupC"),nReplicates=1L, dyadInclusionRate=1)
#' summary(fit)
#'
#'
#' @references
#' Westling, T., & McCormick, T. H. (2015). Beyond prediction: A framework for inference with variational approximations in mixture models. arXiv preprint arXiv:1510.08151.
lologVariational <- function(formula,
           nReplicates = 5L,
           dyadInclusionRate = NULL,
           targetFrameSize = 500000) {
    lolik <- createLatentOrderLikelihood(formula)
    nReplicates <- as.integer(nReplicates)
    
    dyadIndependent <- lolik$getModel()$isIndependent(TRUE, TRUE)
    dyadIndependentOffsets <-
      lolik$getModel()$isIndependent(TRUE, FALSE)
    allDyadIndependent <-
      all(dyadIndependent) & all(dyadIndependentOffsets)
    if (allDyadIndependent & nReplicates != 1L) {
      cat(
        "\n Model is dyad independent. Replications are redundant. Setting nReplicates <- 1L.\n"
      )
      nReplicates <- 1L
    }
    network <- lolik$getModel()$getNetwork()
    n <- network$size()
    ndyads <- n * (n - 1)
    if (!network$isDirected())
      ndyads <- ndyads / 2
    if (is.null(dyadInclusionRate)) {
      dyadInclusionRate <- min(1, targetFrameSize / ndyads)
    }
    samples <-
      lolik$variationalModelFrame(nReplicates, dyadInclusionRate)
    predictors <- lapply(samples, function(x)
      as.data.frame(x[[2]],
                    col.names = 1:length(x[[2]])))
    predictors <- do.call(rbind, predictors)
    outcome <- do.call(c, lapply(samples, function (x)
      x[[1]]))
    
    logFit <-
      glm(outcome ~ as.matrix(predictors) - 1, family = binomial())
    theta <- logFit$coefficients
    names(theta) <- names(lolik$getModel()$statistics())
    result <- list(
      method = "variational",
      formula = formula,
      theta = theta,
      vcov = vcov(logFit) * nReplicates / dyadInclusionRate,
      nReplicates = nReplicates,
      dyadInclusionRate = dyadInclusionRate,
      allDyadIndependent = allDyadIndependent,
      likelihoodModel = lolik,
      outcome = outcome,
      predictors = predictors
    )
    class(result) <- c("lologVariationalFit", "lolog", "list")
    result
  }


#' Print of a lologVariationalFit object
#' @param x the object
#' @param ... additional parameters (unused)
#' @method print lologVariationalFit
print.lologVariationalFit <- function(x, ...) {
  if (x$allDyadIndependent)
    cat("MLE Coefficients:\n")
  else
    cat("Variational Inference Coefficients:\n")
  print(x$theta)
  if (x$dyadInclusionRate != 1) {
    cat("Inclusion rate:", x$dyadInclusionRate, "\n")
  }
  if (!x$allDyadIndependent)
    cat("# of replicates:", x$nReplicates, "\n")
}


#' Print of a lolog object
#' @param x the object
#' @param ... additional parameters (unused)
#' @method print lolog
print.lolog <- function(x, ...) {
  cat(x$method, "Coefficients:\n")
  print(x$theta)
}


#' Summary of a lolog object
#' @param object the object
#' @param ... additional parameters (unused)
#' @method summary lolog
#' @examples
#' data(ukFaculty)
#' fit <- lologVariational(ukFaculty ~ edges() + nodeMatch("GroupC"), nReplicates=1L, dyadInclusionRate=1)
#' summary(fit)
#' @method summary lolog
summary.lolog <- function(object, ...) {
  x <- object
  theta <- x$theta
  se <- sqrt(diag(x$vcov))
  pvalue <- 2 * pnorm(abs(theta / se), lower.tail = FALSE)
  stats <- x$likelihoodModel$getModel()$statistics()
  orderInd <- x$likelihoodModel$getModel()$isIndependent(FALSE, TRUE)
  stats[!orderInd] <- NA
  result <-
    data.frame(
      observed_statistics = stats,
      theta = theta,
      se = se,
      pvalue = round(pvalue, 4)
    )
  rownames(result) <- names(stats)
  result
}


#' Fits a LOLOG model via Mote Carlo Generalized Method of Moments
#'
#'
#' @description
#' \code{lolog} is used to fit Latent Order LOGistic Graph (LOLOG) models. LOLOG models are
#' motivated by the idea of network growth where the network begins empty, and edge variables
#' are sequentially 'added' to the network with an eather unobserved, or partially observed
#' order \eqn{s}. Conditional upon the inclusion order, the probability of an edge has a
#' logistic relationship with the change in network statistics.
#'
#'
#' @param formula A lolog formula for the sufficient statistics (see details).
#' @param auxFormula A lolog formula of statistics to use for moment matching
#' @param theta Initial parameters values. Estimated via \code{\link{lologVariational}} if NULL.
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
#' @param verbose Level of verbosity 0-2.
#'
#'
#' @details
#' LOLOG represents the probability of a tie, given the network grown up to a timepoint as
#' \deqn{
#'   \textrm{logit}\big(p(y_{s_t}=1 | \eta, y^{t-1}, s_{ \leq t})\big) = \theta \cdot c(y_{s_t}=1 | y^{t-1}, s_{ \leq t})
#' }
#' where \eqn{s_{\leq t}} is the growth order of the network up to time \eqn{t}, \eqn{y^{t-1}} is the
#' state of the graph at time \eqn{t-1}. \eqn{c(y_{s_t} | y^{t-1}, s_{ \leq t})} is a vector
#' representing the change in graph statistics from time \eqn{t-1} to \eqn{t} if an edge is present, and
#' \eqn{\theta} is a vector of parameters.
#'
#' The motivating growth order proceeds 'by vertex.' The network begins 'empty' and then vertices are 'added'
#' to the network sequentially. The order of vertex inclusion may be random or fixed. When a vertex 'enters' the
#' network, each of the edge variables connecting it and vertices already in the network are considered for
#' edge creation in a completely random order.
#'
#' LOLOG formulas contain a network, DirectedNet or UndirectedNet object on the left hand side.
#' the right hand side contains the model terms used. for example,
#'
#' \code{net ~ edges}
#'
#' represents and Erdos-Renyi model and
#'
#' \code{net ~ edges + preferentialAttachment()}
#'
#' represents a Barabasi-Albert model. See \code{\link{lolog-terms}} for a list of allowed model statstics.
#'
#' Conditioning on (partial) vertex order can be done by
#' placing an ordering variable on the right hand side of the '|' operator, as in
#'
#' \code{net ~ edges + preferentialAttachment() | order}
#'
#' 'order' should be a numeric vector with as many elements as there are vertices in the network.
#' Ties are allowed. Vertices with higher order values will always be included later. Those with the same
#' values will be included in a random order in each simulated network.
#'
#' offsets and constraints are specified by wraping them with either \code{offset()} or \code{contraint()},
#' for example, the following specifies an Erdos-renyi model with the constraint that degrees must be less
#' that 10
#'
#' \code{net ~ edges + constraint(boundedDegree(0L, 10L))}
#'
#' If the model contains any order dependent statistics, additional moment constraints
#' must be specified in \code{auxFormula}. Ideally these should be chosen to capture
#' the features modeled by the order dependent statistic. For example, \code{preferentialAttachment}
#' models the degree structure, so we might choose two-stars as a moment constraint.
#'
#'  \code{lolog(net ~ edges + preferentialAttachment(), net ~ star(2))}
#'
#' will fit a Barabasi-Albert model with the # of edges and # of two-stars as moment constraints.
#'
#'
#' @return An object of class 'lolog'. If the model is dyad independent, the returned object will
#' also be of class "lologVariational" (see \code{\link{lologVariational}}, otherwise it will
#' also be a "lologGmm" obejct.
#'
#' lologGmm objects contain:
#'
#' \item{method}{"Method of Moments" for order independent models, otherwise "Generalized Method of Moments"}
#' \item{formula}{The model formula}
#' \item{auxFormula}{The formula containing additional moment conditions}
#' \item{theta}{The parameter estimates}
#' \item{stats}{The statistics for each network in the last iteration}
#' \item{estats}{the expected stats (G(y,s)) for each network in the last iteration}
#' \item{obsStats}{the observed network statistics}
#' \item{net}{A network simulated from the fit model}
#' \item{grad}{The gradient of the moment conditions}
#' \item{vcov}{The asymptotic covariance matrix of the parameter estimates}
#' \item{likelihoodModel}{An object of class *LatentOrderLikelihood at the fit parameters}
#'
#'
#' @examples
#' library(network)
#' set.seed(1)
#' data(flo)
#' flomarriage <- network(flo,directed=FALSE)
#' flomarriage %v% "wealth" <- c(10,36,27,146,55,44,20,8,42,103,48,49,10,48,32,3)
#'
#' # A dyad independent model
#' fit <- lolog(flomarriage ~ edges + nodeCov("wealth"))
#' summary(fit)
#'
#' # A dyad dependent model with 2-stars and triangles
#' fit2 <- lolog(flomarriage ~ edges + nodeCov("wealth") + star(2) + triangles, verbose=FALSE)
#' summary(fit2)
#'
#' # An order dependent model
#' fit3 <- lolog(flomarriage ~ edges + nodeCov("wealth") + preferentialAttachment(),
#'               flomarriage ~ star(2), verbose=FALSE)
#' summary(fit3)
#'
#' # Try something a bit more real
#' \dontrun{
#'
#' data(ukFaculty)
#' fituk <- lolog(ukFaculty ~ edges() + nodeMatch("GroupC") + nodeCov("GroupC") + triangles + star(2))
#' summary(fituk)
#' plot(fituk$net, vertex.col= ukFaculty %v% "Group" + 2)
#'
#' }
#'
lolog <- function(formula,
           auxFormula = NULL,
           theta = NULL,
           nsamp = 1000,
           includeOrderIndependent = TRUE,
           weights = "full",
           tol = .1,
           nHalfSteps = 10,
           maxIter = 100,
           minIter = 2,
           startingStepSize = .1,
           maxStepSize = .5,
           cluster = NULL,
           verbose = TRUE) {
    #initialize theta via variational inference
    if (is.null(theta)) {
      if (verbose)
        cat("Initializing using variational fit\n")
      varFit <- lologVariational(formula, dyadInclusionRate = 1)
      if (varFit$allDyadIndependent) {
        if (verbose)
          cat("Model is dyad independent. Returning maximum likelihood estimate.\n")
        return(varFit)
      }
      theta <- varFit$theta
      if (verbose)
        cat("theta:\n")
      if (verbose)
        print(theta)
    }
    
    lolik <- createLatentOrderLikelihood(formula, theta = theta)
    
    orderIndependent <- lolik$getModel()$isIndependent(FALSE, TRUE)
    dyadIndependent <- lolik$getModel()$isIndependent(TRUE, TRUE)
    dyadIndependentOffsets <-
      lolik$getModel()$isIndependent(TRUE, FALSE)
    if (all(dyadIndependent) & all(dyadIndependentOffsets)) {
      if (verbose)
        cat("Model is dyad independent. Returning maximum likelihood estimate.\n")
      varFit <- lologVariational(formula, dyadInclusionRate = 1)
      return(varFit)
    }
    
    terms <- .prepModelTerms(formula)
    auxTerms <- .prepModelTerms(auxFormula)
    samp <- NULL
    obsStats <- NULL
    if (!is.null(auxFormula)) {
      auxModel <- createCppModel(auxFormula)
      auxModel$setNetwork(lolik$getModel()$getNetwork())
      auxModel$calculate()
      obsStats <- auxModel$statistics()
    }
    if (includeOrderIndependent) {
      obsStats <-
        c(lolik$getModel()$statistics()[orderIndependent], obsStats)
    }
    stepSize <- startingStepSize
    lastTheta <- NULL
    lastObjective <- Inf
    hsCount <- 0
    iter <- 0
    if (!is.null(cluster)) {
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
    while (iter < maxIter) {
      iter <- iter + 1
      
      #generate networks
      lolik$setThetas(theta)
      stats <- matrix(0, ncol = length(theta), nrow = nsamp)
      estats <- matrix(0, ncol = length(theta), nrow = nsamp)
      if (includeOrderIndependent)
        auxStats <-
        matrix(0,
               ncol = length(obsStats) - sum(orderIndependent),
               nrow = nsamp)
      else
        auxStats <- matrix(0, ncol = length(obsStats), nrow = nsamp)
      if (is.null(cluster)) {
        for (i in 1:nsamp) {
          if (verbose)
            cat(".")
          samp <- lolik$generateNetwork()
          if (!is.null(auxFormula)) {
            auxModel$setNetwork(samp$network)
            auxModel$calculate()
            auxStats[i, ] <- auxModel$statistics()
          }
          stats[i, ] <- samp$stats + samp$emptyNetworkStats
          estats[i, ] <- samp$expectedStats + samp$emptyNetworkStats
        }
        if (includeOrderIndependent)
          auxStats <- cbind(stats[, orderIndependent], auxStats)
        
        if (verbose)
          cat("\n")
      } else{
        workingNetwork <- as.network(lolik$getModel()$getNetwork())
        worker <- function(i, theta) {
          cat(i, " ")
          network <- as.BinaryNet(network)
          lolik <-
            .createLatentOrderLikelihoodFromTerms(terms, network, theta)
          samp <- lolik$generateNetwork()
          if (!is.null(auxTerms)) {
            auxModel <- .makeCppModelFromTerms(auxTerms, network)
            auxModel$setNetwork(samp$network)
            auxModel$calculate()
            as <- auxModel$statistics()
          } else{
            as <- numeric()
          }
          list(
            stats = samp$stats + samp$emptyNetworkStats,
            estats = samp$expectedStats + samp$emptyNetworkStats,
            auxStats = as
          )
        }
        results <-
          parallel::parLapply(cluster, 1:nsamp, worker, theta = theta)
        stats <- t(sapply(results, function(x)
          x$stats))
        estats <- t(sapply(results, function(x)
          x$estats))
        if (!is.null(auxFormula))
          auxStats <- t(sapply(results, function(x)
            x$auxStats))
        else
          auxStats <- NULL
        if (includeOrderIndependent)
          auxStats <- cbind(stats[, orderIndependent], drop(auxStats))
      }
      
      #calculate gradient of moment conditions
      grad <- matrix(0, ncol = length(theta), nrow = length(obsStats))
      for (i in 1:length(obsStats)) {
        for (j in 1:length(theta)) {
          grad[i, j] <-
            -(cov(auxStats[, i], stats[, j]) - cov(auxStats[, i], estats[, j]))
        }
      }
      
      if (weights == "diagonal")
        W <- diag(1 / (diag(var(auxStats))))
      else
        W <- solve(var(auxStats))
      
      #cacluate moment conditions and stat/observed stat differences transformed by W.
      mh <- colMeans(auxStats)
      diffs <- -sweep(auxStats, 2, obsStats)
      transformedDiffs <- t(t(grad) %*% W %*% t(diffs))
      momentCondition <- colMeans(transformedDiffs)
      
      objective <- colMeans(diffs) %*% W %*% colMeans(diffs)
      if (verbose)
        cat("Objective:\n")
      if (verbose)
        print(objective)
      objCrit <-
        max(-1000000, objective - lastObjective) / (lastObjective + 1)
      
      if (verbose)
        cat("Moment Conditions:\n")
      if (verbose)
        print(momentCondition)
      
      
      # Calculate inverse
      invFailed <-
        inherits(try(gradInv <-
                       solve(t(grad) %*% W %*% grad), silent = TRUE)
                 ,"try-error")
      
      if (verbose > 1)
        pairs(stats)
      
      # If inverse failed, or the objective has increased singificantly, initiate half stepping
      if (hsCount < nHalfSteps &&
          !is.null(lastTheta) && (invFailed || objCrit > .3)) {
        if (verbose)
          cat("Half step back\n")
        theta <- (lastTheta + theta) / 2
        hsCount <- hsCount + 1
        stepSize <- stepSize / 2
        if (verbose)
          cat("Theta:\n")
        if (verbose)
          print(theta)
        next
      } else{
        stepSize <- min(maxStepSize, stepSize * 1.25)
        hsCount <- 0
      }
      if (verbose)
        print(stepSize)
      
      #Update theta
      lastTheta <- theta
      theta <- theta - stepSize * gradInv %*% momentCondition
      lastObjective <- objective
      
      if (verbose)
        print("auxStat Diffs:")
      if (verbose)
        print(colMeans(diffs) / sqrt(diag(var(diffs))))
      
      #Hotelling's T^2 test
      hotT <-
        momentCondition %*% solve(var(transformedDiffs) / nrow(transformedDiffs)) %*% momentCondition
      pvalue <- pchisq(hotT, df = length(theta), lower.tail = FALSE)
      if (verbose)
        cat("Hotelling's T2 p-value: ", pvalue, "\n")
      if (verbose)
        cat("Theta:\n")
      if (verbose)
        print(theta)
      if (pvalue > tol && iter >= minIter) {
        break
      } else if (iter < maxIter) {
        
      }
    }
    
    if (is.null(samp)) {
      samp <- lolik$generateNetwork()
    }
    
    omega <- var(auxStats)
    vcov <- solve(t(grad) %*% W %*% grad) %*%
      t(grad) %*% W %*% omega %*% t(W) %*% grad %*%
      solve(t(grad) %*% t(W) %*% grad)
    
    lastTheta <- drop(lastTheta)
    names(lastTheta) <- names(lolik$getModel()$statistics())
    method <-
      if (is.null(auxFormula))
        "Method of Moments"
    else
      "Generalized Method Of Moments"
    
    result <- list(
      method = method,
      formula = formula,
      auxFromula = auxFormula,
      theta = lastTheta,
      stats = stats,
      estats = estats,
      auxStats = auxStats,
      obsStats = obsStats,
      net = samp$network,
      grad = grad,
      vcov = vcov,
      likelihoodModel = lolik
    )
    class(result) <- c("lologGmm", "lolog", "list")
    result
  }


#' Generates BinaryNetworks from a fit lolog object
#'
#'
#' @param object The lolog object
#' @param nsim The number of simulated networks
#' @param seed Either NULL or an integer that will be used in a call to set.seed before simulating
#' @param convert convert to a network object#'
#' @param ... unused
#'
#'
#' @examples
#' library(network)
#' data(flo)
#' flomarriage <- network(flo,directed=FALSE)
#' flomarriage %v% "wealth" <- c(10,36,27,146,55,44,20,8,42,103,48,49,10,48,32,3)
#' fit <- lolog(flomarriage ~ edges + nodeCov("wealth"))
#' net <- simulate(fit)[[1]]
#' plot(net)
#'
#' @method simulate lolog
simulate.lolog <- function(object, nsim = 1, seed = NULL, convert = FALSE, ...) {
    if (!is.null(seed))
      set.seed(seed)
    l <- list()
    for (i in 1:nsim) {
      l[[i]] <- object$likelihoodModel$generateNetwork()$network
      if (convert)
        l[[i]] <- as.network(l[[i]])
    }
    l
  }


#' Conduct goodness of fit diagnostics
#'
#'
#' @param object the object to evaulate
#' @param ... additional parameters
#'
#'
#' @details
#' see \code{\link{gofit.lolog}}
gofit <- function(object, ...) {
  UseMethod("gofit")
}


#' Goodness of Fit Diagnostics for a LOLOG fit
#'
#'
#' @param object the object to evaulate
#' @param formula A formula specifying the statistics on which to evaluate the fit
#' @param nsim The number of simulated statistics
#' @param ... additional parameters
#'
#' @examples
#'
#' data(ukFaculty)
#' # A dyad independent model
#' fitind <- lolog(ukFaculty ~ edges() + nodeMatch("GroupC") + nodeCov("GroupC"))
#' summary(fitind)
#'
#' # Check gof on degree distribution (bad!)
#' gind <- gofit(fitind, ukFaculty ~ degree(0:50))
#' gind
#' plot(gind)
#'
#' #check gof on esp distribution (bad!)
#' gind <- gofit(fitind, ukFaculty ~ esp(0:25))
#' gind
#' plot(gind)
#'
#' \dontrun{
#'
#' #include triangles and 2-stars
#' fitdep <- lolog(ukFaculty ~ edges() + nodeMatch("GroupC") + nodeCov("GroupC") + triangles + star(2))
#' summary(fitdep)
#'
#' # Check gof on degree distribution (good!)
#' gdep <- gofit(fitdep, ukFaculty ~ degree(0:50))
#' gdep
#' plot(gdep)
#'
#' #check gof on esp dsitribution (good!)
#' gdep <- gofit(fitdep, ukFaculty ~ esp(0:25))
#' gdep
#' plot(gdep)
#'
#' }
#'
#'
#' @method gofit lolog
gofit.lolog <- function(object, formula, nsim = 100, ...) {
  model <- createCppModel(formula)
  observedNetwork <- object$likelihoodModel$getModel()$getNetwork()
  model$setNetwork(observedNetwork)
  model$calculate()
  ostats <- model$statistics()
  ns <- length(ostats)
  stats <- matrix(0, nrow = nsim, ncol = ns)
  model$calculate
  for (i in 1:nsim) {
    net <- object$likelihoodModel$generateNetwork()$network
    model$setNetwork(net)
    model$calculate()
    stats[i, ] <- model$statistics()
  }
  
  mn <- apply(stats, 2, min)
  mx <- apply(stats, 2, max)
  pv <- apply(sweep(stats, 2, ostats), 2, function(a) {
    if (sd(a) < .Machine$double.eps)
      return(NA)
    2 * pnorm(-abs(mean(a) / sd(a)))
  })
  d <- data.frame(
    obs = ostats,
    min = mn,
    mean = colMeans(stats),
    max = mx,
    pvalue = pv
  )
  
  result <- list(ostats = ostats,
                 stats = stats,
                 summary = d)
  class(result) <- "gofit"
  result
}

#' prints a gofit object
#'
#'
#' @param x The object
#' @param ... passed to print.data.frame
#'
#'
#' @method print gofit
print.gofit <- function(x, ...) {
  print(x$summary, ...)
}

#' Plots a gofit object
#'
#'
#' @param x the gofit object
#' @param y unused
#' @param type type of plot, boxplot or lineplot
#' @param normalize If true, netwrok statistics are normalized by subtracting off the observed statistics and scaling by the standard deviation.
#' @param lineAlpha The transparancy of the simulated statistics lines
#' @param lineSize The width of the lines
#' @param ... passed to either boxplot or geom_line
#'
#'
#' @examples
#' data(ukFaculty)
#' # A dyad independent model
#' fitind <- lolog(ukFaculty ~ edges() + nodeMatch("GroupC") + nodeCov("GroupC"))
#' summary(fitind)
#'
#' # Check gof on degree distribution (bad!)
#' gind <- gofit(fitind, ukFaculty ~ degree(0:50))
#' plot(gind)
#' plot(gind, type="box")
#' @method plot gofit
plot.gofit <-
  function(x,
           y,
           type = c("line", "box"),
           normalize = FALSE,
           lineAlpha = .06,
           lineSize = 1,
           ...) {
    type <- match.arg(type)
    stats <- x$stats
    ostats <- x$ostats
    nms <- names(ostats)
    colnames(stats) <- nms
    ylab <- "Statistic"
    if (normalize) {
      stats <- apply(sweep(stats, 2, ostats), 2, function(a) {
        if (sd(a) < .Machine$double.eps)
          return(rep(NA, length(a)))
        a / sd(a)
      })
      ostats <- rep(0, length(ostats))
      ylab <- "Normalized Statistic"
    }
    
    if (type == "box") {
      boxplot(stats, ylab = ylab, ...)
      points(ostats, col = "red", pch = 16)
      return(NULL)
    } else{
      Var2 <- value <- Var1 <- xx <- yy <- gg <- NULL #For R CMD check
      mstats <- reshape2::melt(stats)
      o <- data.frame(xx = nms, yy = ostats, gg = "observed")
      gg <- ggplot2::ggplot(data = mstats) +
        ggplot2::geom_line(
          ggplot2::aes(x = Var2, y = value, group = Var1),
          alpha = lineAlpha,
          size = lineSize,
          ...
        ) +
        ggplot2::geom_line(
          ggplot2::aes(x = xx, y = yy, group = gg),
          data = o,
          color = "red",
          size = lineSize,
          ...
        ) +
        ggplot2::theme_bw() + ggplot2::ylab(ylab) + ggplot2::xlab("")  +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank()
        )
      return(gg)
    }
  }
