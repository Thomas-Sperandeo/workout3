context("Test auxiliary functions")

#Tests aux_mean
test_that("checks aux_mean returns an object of length 1", {
  expect_length(aux_mean(10, 0.5), 1)
  expect_length(aux_mean(25, 0.99), 1)
})

test_that("checks aux_mean return a numeric object", {
  expect_type(aux_mean(15, 0.8), "double")
  expect_type(aux_mean(22, 0.2), "double")
})

#Tests aux_variance
test_that("checks aux_variance returns an object of length 1", {
  expect_length(aux_variance(10, 0.5), 1)
  expect_length(aux_variance(25, 0.99), 1)
})

test_that("checks aux_variance return a numeric object", {
  expect_type(aux_variance(15, 0.8), "double")
  expect_type(aux_variance(22, 0.2), "double")
})

#Tests aux_mode
test_that("checks aux_mode returns an object of length 1 if int(np + p) is not an integer", {
  expect_length(aux_mode(100, 0.5), 1)
  expect_length(aux_mode(7, 0.3), 1)
})

test_that("checks aux_mode returns an object of length 2 if int(np + p) is an integer", {
  expect_length(aux_mode(5, 0.5), 2)
  expect_length(aux_mode(1, 0.5), 2)
  expect_length(aux_mode(9, 0.2), 2)
})

test_that("checks aux_mode return a numeric object", {
  expect_type(aux_mode(15, 0.8), "double")
  expect_type(aux_mode(1, 0.5), "double")
})


#Tests aux_skewness
test_that("checks aux_skewness returns an object of length 1", {
  expect_length(aux_skewness(10, 0.5), 1)
  expect_length(aux_skewness(25, 0.99), 1)
})

test_that("checks aux_skewness return a numeric object", {
  expect_type(aux_skewness(15, 0.8), "double")
  expect_type(aux_skewness(22, 0.2), "double")
})


#Tests aux_kurtosis
test_that("checks aux_kurtosis returns an object of length 1", {
  expect_length(aux_kurtosis(10, 0.5), 1)
  expect_length(aux_kurtosis(25, 0.99), 1)
})

test_that("checks aux_kurtosis return a numeric object", {
  expect_type(aux_kurtosis(15, 0.8), "double")
  expect_type(aux_kurtosis(22, 0.2), "double")
})
