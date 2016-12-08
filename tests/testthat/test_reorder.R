context("Reordering factors")

test_that("Case 1: The factor variable x is not a factor",{
  expect_error(reorder(c(1,2), c(1,2), median),"y should be a factor", fixed = TRUE)
})

test_that("Case 2: The factor variables x and y have different length",{
  expect_error(reorder(factor('a'), c(1,2), median),"x and y should be of the same length", fixed = TRUE)
})

test_that("Case 3: The factor variable Fun is not a function",{
  expect_error(reorder(factor(c('a','b')), c(1,2), 3),"FUN should be a function", fixed = TRUE)
})

test_that("Case 4: The argument order is not logical",{
  expect_error(reorder(factor(c('a','b')), c(1,2), mean, order=1),"This argument should be logical", fixed = TRUE)
})

test_that("Case 5: Checking for output with data without order",{
  test_factors <- as.factor(c("a","a","a","b","b","b","c","c","c"))
  test_vals <- seq(1,9)

  expected_output <- base::factor(c("a","a","a","b","b","b","c","c","c"), levels = c("c","b","a"))
  expected_scores <- c(2, 5, 8)
  dim(expected_scores) <- 3
  dimnames(expected_scores) <- list(c("a", "b", "c"))
  attr(expected_output, "scores") <- expected_scores
  check_output <- reorder(test_factors, test_vals, mean)
  expect_equal(expected_output, check_output)
})
