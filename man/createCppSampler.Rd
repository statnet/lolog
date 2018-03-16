\name{createCppSampler}
\alias{createCppSampler}
\title{create a sampler}
\usage{
  createCppSampler(formula, sampler = "NTDNBRMetropolis",
    nodeSamplingPercentage = 0.2, ignoreMnar = TRUE)
}
\arguments{
  \item{formula}{the model formula}

  \item{sampler}{the method of sampling to use}

  \item{nodeSamplingPercentage}{how often the nodes should
  be toggled}

  \item{ignoreMnar}{ignore missing not at random offsets}
}
\description{
  create a sampler
}

