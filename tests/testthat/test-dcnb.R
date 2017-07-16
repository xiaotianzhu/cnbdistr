context("dcnb")

test_that("dcnb stops when it should", {

  expect_error( dcnb(2000, 120, 90, 0.994) ) # missing an argument
  expect_error( dcnb(-980, 2000, 120, 90, 0.994) ) # x < 0
  expect_error( dcnb(2980, 2000, 120, 90, 0.994) ) # x > D
  expect_error( dcnb(980, -2000, 120, 90, 0.994) ) # D < 0
  expect_error( dcnb(980, 2000, -120, 90, 0.994) ) # r1 < 0
  expect_error( dcnb(980, 2000, 120, -90, 0.994) ) # r2 < 0
  expect_error( dcnb(980, 2000, 120, 90, -0.994) ) # lambda < 0

})

test_that("dcnb computes correctly", {

  # lamba == 1
  D <- 32
  r1 <- 600
  r2 <- 499
  lambda <- 1
  expect_equal(dcnb(0:D, D, r1, r2, lambda),  rmutil::dbetabinom(0:D, D, r1/(r1+r2), r1+r2))

  # lambda < 1
  D <- 64
  r1 <- 563
  r2 <- 49
  lambda <- 0.72
  tmp <- log(dcnb(0:D, D, r1, r2, lambda)) - log(rmutil::dbetabinom(0:D, D, r1/(r1+r2), r1+r2)) - c(0:D)*log(lambda)
  expect_true(diff(range(tmp)) < 1e-11)

  # lambda > 1
  D <- 99
  r1 <- 60
  r2 <- 588
  lambda <- 1.32
  expect_equal(dcnb(0:D, D, r1, r2, lambda), dcnb(D:0, D, r2, r1, 1/lambda))

})
