
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
#' #include triangles and 2-stars (in and out)
#' fitdep <- lolog(ukFaculty ~ edges() + nodeMatch("GroupC") + nodeCov("GroupC") + 
#'                 triangles + star(2, direction=1L) + star(2, direction=2L), nsamp=1500)
#' summary(fitdep)
#'
#' # Check gof on (in + out) degree distribution (good!)
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
plot.gofit <- function(x,
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