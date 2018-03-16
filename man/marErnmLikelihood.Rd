\name{marErnmLikelihood}
\alias{marErnmLikelihood}
\title{likelihood for an lolog with missing data}
\usage{
  marErnmLikelihood(theta, sample, theta0, stats,
    minEss = 5, damping = 0.1)
}
\arguments{
  \item{theta}{parameters}

  \item{sample}{mcmc sample}

  \item{theta0}{parameter values which generated sample}

  \item{stats}{observed statistics}

  \item{minEss}{minimum effective sample size}

  \item{damping}{a damping parameter}
}
\description{
  likelihood for an lolog with missing data
}

