\name{simulateStatistics}
\alias{simulateStatistics}
\title{simulate statistics}
\usage{
  simulateStatistics(formula, theta,
    sampler = "NTDNBRMetropolis",
    nodeSamplingPercentage = 0.2, mcmcBurnIn = 10000,
    mcmcInterval = 100, mcmcSampleSize = 100,
    ignoreMnar = TRUE)
}
\arguments{
  \item{formula}{the model formula}

  \item{theta}{model parameters}

  \item{sampler}{the method of sampling to use}

  \item{nodeSamplingPercentage}{how often the nodes should
  be toggled}

  \item{mcmcBurnIn}{burn in}

  \item{mcmcInterval}{interval}

  \item{mcmcSampleSize}{sample size}

  \item{ignoreMnar}{ignore missing not at random offsets}
}
\description{
  simulate statistics
}

