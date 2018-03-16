\name{lologFit}
\alias{lologFit}
\title{fit an lolog model}
\usage{
  lologFit(sampler, theta0, mcmcBurnIn = 10000,
    mcmcInterval = 100, mcmcSampleSize = 10000,
    minIter = 3, maxIter = 40, objectiveTolerance = 0.5,
    gradTolerance = 0.25, meanStats, verbose = 1)
}
\arguments{
  \item{sampler}{the ErnmModel}

  \item{theta0}{initial starting values}

  \item{mcmcBurnIn}{burn in}

  \item{mcmcInterval}{interval}

  \item{mcmcSampleSize}{sample size}

  \item{minIter}{minimum number of iterations}

  \item{maxIter}{maximum number of iterations}

  \item{objectiveTolerance}{convergance criteria on change
  in log likelihood ratio}

  \item{gradTolerance}{convergance criteria on scaled
  gradient}

  \item{meanStats}{if non-missing, these are the target
  statistics}

  \item{verbose}{level of verbosity 0, 1, or 2}
}
\description{
  fit an lolog model
}

