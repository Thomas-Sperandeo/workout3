context("Tests main binomial probability functions")

#Tests bin_choose
test_that("checks bin_choose returns an object of length(success) for valid inputs trials and success", {
  expect_length(bin_choose(10, 0:5), 6)
  expect_length(bin_choose(10, 2:9), 8)
  expect_length(bin_choose(10, 1), 1)
})

test_that("checks bin_choose returns a numeric object", {
  expect_type(bin_choose(10, 0:5), "double")
  expect_type(bin_choose(10, 2:9), "double")
  expect_type(bin_choose(10, 1), "double")
})

test_that("checks bin_choose fails when max(success) > trials", {
    expect_error(bin_choose(1, 5))
    expect_error(bin_choose(5, 4:6))
    expect_error(bin_choose(10, 25))
})

#Tests bin_probability
test_that("checks bin_probability returns an object of length(success) for valid inputs trials, success and prob", {
  expect_length(bin_probability(10, 0:5, 0.2), 6)
  expect_length(bin_probability(10, 2:9, 0.33), 8)
  expect_length(bin_probability(10, 1, 1), 1)
})


#Tests bin_distribution
test_that("checks bin_distribution returns an object of class c('bindis', 'data.frame')", {
  expect_equal(class(bin_distribution(10, 0.5)), c("bindis", "data.frame"))
  expect_equal(class(bin_distribution(27, 0.1)), c("bindis", "data.frame"))
})


#Tests bin_cumulative
test_that("checks sum of cumulative prob = 1", {
  expect_equal(sum(bin_cumulative(10, 0.2)$prob), 1)
  expect_equal(sum(bin_cumulative(21, 0.85)$prob), 1)
})

test_that("checks # of rows of bin_cumulative = length(trials)", {
  expect_equal(length(bin_cumulative(10, 0.2)$successes), 11)
  expect_equal(length(bin_cumulative(20, 0.4)$successes), 21)
})

test_that("checks bin_culmative returns an object of type c('bincum', 'data.frame')", {
  expect_equal(class(bin_cumulative(8, 0.25)), c("bincum", "data.frame"))
  expect_equal(class(bin_cumulative(23, 0.71)), c("bincum", "data.frame"))
})
