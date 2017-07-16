context("qcnb")

test_that("qcnb stops when it should", {

  expect_error( qcnb(2000, 120, 90, 0.994) ) # missing an argument
  expect_error( qcnb(-980, 2000, 120, 90, 0.994) ) # q < 0
  expect_error( qcnb(1.9, 2000, 120, 90, 0.994) ) # q > 1
  expect_error( qcnb(0.3, -2000, 120, 90, 0.994) ) # D < 0
  expect_error( qcnb(0.4, 2000, -120, 90, 0.994) ) # r1 < 0
  expect_error( qcnb(0.5, 2000, 120, -90, 0.994) ) # r2 < 0
  expect_error( qcnb(0.6, 2000, 120, 90, -0.994) ) # lambda < 0

})

test_that("qcnb computes correctly", {

  # lamba == 1
  D <- 32
  r1 <- 600
  r2 <- 499
  lambda <- 1
  expect_equal(qcnb(seq(0, 0.999999999, 0.01), D, r1, r2, lambda),  rmutil::qbetabinom(seq(0, 0.999999999, 0.01), D, r1/(r1+r2), r1+r2))

  # lambda < 1
  D <- 64
  r1 <- 163
  r2 <- 49
  lambda <- 0.72
  tmp <- qcnb(seq(0.1, 1, 0.01), D, r1, r2, lambda)
  expect_false(any(pcnb(tmp, D, r1, r2, lambda) < seq(0.1, 1, 0.01)))
  expect_false(any(pcnb(tmp - 1, D, r1, r2, lambda) >= seq(0.1, 1, 0.01)))

  # lambda > 1
  D <- 99
  r1 <- 60
  r2 <- 128
  lambda <- 1.32
  tmp <- qcnb(seq(0.01, 0.9, 0.01), D, r1, r2, lambda)
  expect_false(any(pcnb(tmp, D, r1, r2, lambda) < seq(0.01, 0.9, 0.01)))
  expect_false(any(pcnb(tmp - 1, D, r1, r2, lambda) >= seq(0.01, 0.9, 0.01)))

})
