context("pcnb")

test_that("pcnb stops when it should", {

  expect_error( pcnb(2000, 120, 90, 0.994) ) # missing an argument
  expect_error( pcnb(980, -2000, 120, 90, 0.994) ) # D < 0
  expect_error( pcnb(980, 2000, -120, 90, 0.994) ) # r1 < 0
  expect_error( pcnb(980, 2000, 120, -90, 0.994) ) # r2 < 0
  expect_error( pcnb(980, 2000, 120, 90, -0.994) ) # lambda < 0

})

test_that("pcnb computes correctly", {

  # lamba == 1
  D <- 32
  r1 <- 600
  r2 <- 499
  lambda <- 1
  expect_equal(pcnb(0:D, D, r1, r2, lambda),  rmutil::pbetabinom(0:D, D, r1/(r1+r2), r1+r2))

  # lambda < 1
  D <- 64
  r1 <- 563
  r2 <- 49
  lambda <- 0.72
  expect_equal(pcnb(0:D, D, r1, r2, lambda), cumsum(dcnb(0:D, D, r1, r2, lambda)))

  # lambda > 1
  D <- 99
  r1 <- 60
  r2 <- 588
  lambda <- 1.32
  expect_equal(pcnb(0:D, D, r1, r2, lambda), 1 - pcnb((D-1):-1, D, r2, r1, 1/lambda))

})
