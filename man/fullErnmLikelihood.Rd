\name{fullErnmLikelihood}
\alias{fullErnmLikelihood}
\title{likelihood for a fully observed lolog}
\usage{
  fullErnmLikelihood(theta, sample, theta0, stats,
    minEss = 5, damping = 0.05,
    method = c("logtaylor", "lognormal", "kernel"))
}
\arguments{
  \item{theta}{parameters}

  \item{sample}{mcmc sample}

  \item{theta0}{parameter values which generated sample}

  \item{stats}{observed statistics}

  \item{minEss}{minimum effective sample size}

  \item{damping}{a damping parameter}

  \item{method}{cumulant generating function approximation}
}
\description{
  likelihood for a fully observed lolog
}

