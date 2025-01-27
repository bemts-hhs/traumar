test_that("`%not_in%` works with character vectors", {
  x <- c("apple", "banana", "cherry")
  y <- c("banana", "grape")

  result <- x %not_in% y
  expected <- c(TRUE, FALSE, TRUE)

  expect_equal(result, expected)
})

test_that("`%not_in%` works with numeric vectors", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4, 6)

  result <- x %not_in% y
  expected <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

  expect_equal(result, expected)
})

test_that("`%not_in%` works with logical vectors", {
  x <- c(TRUE, FALSE, TRUE)
  y <- c(FALSE)

  result <- x %not_in% y
  expected <- c(TRUE, FALSE, TRUE)

  expect_equal(result, expected)
})

test_that("`%not_in%` works with mixed types", {
  x <- c(1, "banana", TRUE)
  y <- c("banana", 2, FALSE)

  result <- x %not_in% y
  expected <- c(TRUE, FALSE, TRUE)

  expect_equal(result, expected)
})

test_that("`%not_in%` works with empty vectors", {
  x <- character(0)
  y <- c("apple", "banana")

  result <- x %not_in% y
  expected <- logical(0)

  expect_equal(result, expected)

  x <- c("apple", "banana")
  y <- character(0)

  result <- x %not_in% y
  expected <- c(TRUE, TRUE)

  expect_equal(result, expected)
})

test_that("`%not_in%` works with NA values in `x` and `y`", {
  x <- c(NA, "apple", NA)
  y <- c("banana", NA)

  result <- x %not_in% y
  expected <- c(FALSE, TRUE, FALSE)

  expect_equal(result, expected)
})

test_that("`%not_in%` works with identical `x` and `y`", {
  x <- c("apple", "banana", "cherry")
  y <- c("apple", "banana", "cherry")

  result <- x %not_in% y
  expected <- c(FALSE, FALSE, FALSE)

  expect_equal(result, expected)
})

test_that("`%not_in%` handles duplicates in `x` and `y`", {
  x <- c("apple", "banana", "banana", "cherry")
  y <- c("banana", "banana", "grape")

  result <- x %not_in% y
  expected <- c(TRUE, FALSE, FALSE, TRUE)

  expect_equal(result, expected)
})

test_that("`%not_in%` works with NULL values in `x` or `y`", {
  x <- c("apple", "banana", "cherry")
  y <- NULL

  result <- x %not_in% y
  expected <- c(TRUE, TRUE, TRUE)

  expect_equal(result, expected)

  x <- NULL
  y <- c("apple", "banana")

  result <- x %not_in% y
  expected <- logical(0)

  expect_equal(result, expected)
})

test_that("`%not_in%` works with numeric edge cases", {
  x <- c(-1, 0, 1, Inf, -Inf, NA)
  y <- c(0, NA, Inf)

  result <- x %not_in% y
  expected <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)

  expect_equal(result, expected)
})
