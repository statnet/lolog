library(lolog)
library(testthat)
library(ergm)

context("LOLOG Fit")

test_that("lolog_fit",{
  data(sampson)
  fit <- lologGmm(samplike ~ edges() + triangles(), 
                  samplike ~ edges() + triangles(),
                  theta=c(-1.2304996,0),
                  nsamp=200)
  expect_true(all(fit$theta > c(-1.26,.19)) & all(fit$theta < c(-1.23,.21))) 
  
  # Test Dyad Independent
  efit <- ergm(samplike ~ edges + nodematch("group"))
  vfit <- lologVariational(samplike ~ edges() + nodeMatch("group"))
  expect_true(all(round(efit$coef - vfit$theta,6) == 0))
  
  gmmfit <- lologGmm(samplike ~ edges() + nodeMatch("group"),
                     samplike ~ edges() + nodeMatch("group"),
                     theta=vfit$theta,nsamp=500)
  svar <- summary(vfit)
  sgmm <- summary(gmmfit)
  expect_true(all(abs(svar$se - sgmm$se) < .04))
  expect_true(all(abs(svar$theta - sgmm$theta) < .02))
  
  
})
