## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width="90%", 
  fig.height=4,
  dpi=120
)

## ---- fig.height=7-------------------------------------------------------
suppressPackageStartupMessages(library(network))
library(lolog)
data(ukFaculty)
#?ukFaculty
ukFaculty %v% "Group"  # The school affiliation of the faculty
ukFaculty %v% "GroupC" # affiliation coded as categorical
plot(ukFaculty, vertex.col = (ukFaculty %v% "Group" ) + 1)

## ------------------------------------------------------------------------
fitukInd <- lolog(ukFaculty ~ edges() + nodeMatch("GroupC"))
summary(fitukInd)

## ------------------------------------------------------------------------
suppressPackageStartupMessages(library(ergm))
ergm(ukFaculty ~ edges() + nodematch("GroupC"))

## ------------------------------------------------------------------------
g <- gofit(fitukInd, ukFaculty ~ degree(0:50,"out"))
plot(g)
g <- gofit(fitukInd, ukFaculty ~ degree(0:50,"in"))
plot(g)
g <- gofit(fitukInd, ukFaculty ~ esp(0:50))
plot(g)

## ------------------------------------------------------------------------
g <- gofit(fitukInd, ukFaculty ~ edges +mutual)
plot(g,type="box")

## ------------------------------------------------------------------------
fituk <- lolog(ukFaculty ~ edges() + nodeMatch("GroupC") + 
            mutual + triangles ,verbose=FALSE)
summary(fituk)

## ------------------------------------------------------------------------
g <- gofit(fituk, ukFaculty ~ degree(0:50,"out"))
plot(g)
g <- gofit(fituk, ukFaculty ~ degree(0:50,"in"))
plot(g)
g <- gofit(fituk, ukFaculty ~ esp(0:50))
plot(g)

## ------------------------------------------------------------------------
plot(fituk)

## ------------------------------------------------------------------------
fitukErgm <- ergm(ukFaculty ~ edges() + nodematch("GroupC") + mutual +
            gwesp(decay=.25, fixed=TRUE), verbose=FALSE)
summary(fitukErgm)
g <- gof(fitukErgm)
par(mfrow=c(2,3))
plot(g)

