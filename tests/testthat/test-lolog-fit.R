library(lolog)
library(testthat)
library(ergm)

context("LOLOG Fit")

test_that("lolog_fit",{
  data(sampson)
  fit <- lologGmm(samplike ~ edges() + triangles(), 
                  samplike ~ edges() + triangles(),
                  theta=c(-1.2304996,0),
                  nsamp=200,
                  includeOrderIndependent = FALSE,
                  verbose=FALSE)
  expect_true(all(fit$theta > c(-1.26,.19)) & all(fit$theta < c(-1.23,.21))) 
  
  # Test Dyad Independent
  efit <- ergm(samplike ~ edges + nodematch("group"))
  vfit <- lologVariational(samplike ~ edges() + nodeMatch("group"))
  expect_true(all(round(efit$coef - vfit$theta,6) == 0))
  
  gmmfit <- lologGmm(samplike ~ edges() + nodeMatch("group"),
                     theta=vfit$theta,nsamp=500,
                     verbose=FALSE)
  svar <- summary(vfit)
  sgmm <- summary(gmmfit)
  expect_true(all(abs(svar$se - sgmm$se) < .04))
  expect_true(all(abs(svar$theta - sgmm$theta) < .02))
  
  
  # order dependent
  data(flo)
  flomarriage <- network(flo,directed=FALSE)
  fit <- lologGmm(flomarriage ~ edges() + preferentialAttachment(), 
                  flomarriage ~ star(2L),
                  theta=c(-1.54998018,0),
                  nsamp=400,
                  verbose=FALSE)
  expect_true(all(fit$theta > c(-1.9,0)) & all(fit$theta < c(-1.1,.2))) 
  
})



test_that("lolog_fit_parallel",{
  library(parallel)
  cluster <- parallel::makeCluster(2)
  # order dependent
  data(flo)
  flomarriage <- network(flo,directed=FALSE)
  fit <- lologGmm(flomarriage ~ edges() + preferentialAttachment(), 
                  flomarriage ~ star(2L),
                  theta=c(-1.54998018,0),
                  nsamp=400,
                  cluster=cluster,
                  verbose=FALSE)
  expect_true(all(fit$theta > c(-1.9,0)) & all(fit$theta < c(-1.1,.2))) 
  
  
  fit <- lologGmm(flomarriage ~ edges() + star(2), 
                  theta=c(-1.54998018,0),
                  nsamp=200,
                  cluster=cluster,
                  verbose=FALSE)
  expect_true(all(fit$theta > c(-1.65,0)) & all(fit$theta < c(-1.55,.2))) 
  parallel::stopCluster(cluster)
})