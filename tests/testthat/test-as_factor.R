# These methods are related to the defined() class but
# tested in a separate test suite.

test_that("as_factor() converts defined vector with labels to factor", {
  x <- defined(
    c(0, 1, 1, 0),
    label = "Sex",
    labels = c("Female" = 0, "Male" = 1)
  )
  f <- as_factor(x)

  expect_s3_class(f, "factor")
  expect_equal(levels(f), c("Female", "Male"))
  expect_equal(as.character(f), c("Female", "Male", "Male", "Female"))
})

test_that("as_factor() preserves order of labels", {
  x <- defined(
    c(2, 3, 1),
    label = "Priority",
    labels = c("Low" = 1, "Medium" = 2, "High" = 3)
  )
  f <- as_factor(x)
  expect_equal(levels(f), c("Low", "Medium", "High"))
  expect_equal(as.character(f), c("Medium", "High", "Low"))
})

test_that("as_factor() without labels returns numeric-like factor", {
  x <- defined(c(1, 2, 1, 3), label = "Unlabelled")
  f <- as_factor(x)
  expect_s3_class(f, "factor")
  expect_equal(as.character(f), c("1", "2", "1", "3"))
})
