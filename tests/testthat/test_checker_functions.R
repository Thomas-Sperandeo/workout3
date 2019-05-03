context("Test checker arguments")

#Tests for check_prob
test_that("check_prob works for numbers between 0 and 1", {
  expect_true(check_prob(0.5))
  expect_true(check_prob(0.05))
})

test_that("check_prob fails with numbers > 1 and < 0", {
  expect_error(check_prob(-0.3))
  expect_error(check_prob(15))
})

test_that("check_prob fails when length(prob) > 1", {
  expect_error(check_prob(c(1, 2)))
  expect_error(check_prob(c(0.1, 0.8)))
})

#Tests for check_trials
test_that("check trials works for a positive integer", {
  expect_true(check_trials(10))
  expect_true(check_trials(15))
})

test_that("check trials fails when not a positive integer", {
  expect_error(check_trials(-5))
  expect_error(check_trials(2.3))
  expect_error(check_trials(0))
  expect_error(check_trials("hi"))
})

test_that("check trials fails when length(trials) > 1", {
  expect_error(check_trials(c(1, 2)))
  expect_error(check_trials(1:5))
})

#Tests for check_success
test_that("check_success works for any positive integer trials and vector success, where max(success) > trials", {
  expect_true(check_success(10, 1:5))
  expect_true(check_success(3, 3))
})

test_that("check_success fails when max(success) > trials", {
  expect_error(check_success(5, 10))
  expect_error(check_success(-1, 50))
})

test_that("check success fails when min(success) < 0", {
  expect_error(check_success(10, -5))
  expect_error(check_success(1, -100))
})


