context("rcnb")

test_that("rcnb stops when it should", {

  expect_error( rcnb(2000, 120, 90, 0.994) ) # missing an argument
  expect_error( rcnb(-2, 2000, 120, 90, 0.994) ) # n < 0
  expect_error( rcnb(3, -2000, 120, 90, 0.994) ) # D < 0
  expect_error( rcnb(4, 2000, -120, 90, 0.994) ) # r1 < 0
  expect_error( rcnb(5, 2000, 120, -90, 0.994) ) # r2 < 0
  expect_error( rcnb(6, 2000, 120, 90, -0.994) ) # lambda < 0

})

test_that("rcnb computes correctly", {

  set.seed(2018)

  # lambda < 1
  m = 720
  D = 7
  r1 = 2
  r2 = 0.4
  lambda = 0.6
  observed <- table(rcnb(m, D, r1, r2, lambda))
  expected <- dcnb(0:D, D, r1, r2, lambda) * m
  tested <- sum((observed - expected)^2 / expected)
  expect_true(tested < stats::qchisq(0.4, D, lower.tail = F))

  rm(.Random.seed, envir=globalenv())

})
