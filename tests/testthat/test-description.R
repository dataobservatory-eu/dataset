test_that("description() <- assignment works with and without overwrite", {
  x <- orange_df
  description(x, overwrite = TRUE) <- "Description 1"
  expect_equal(description(x), "Description 1")

  # Attempt to overwrite without permission triggers warning and no change
  expect_warning({
    description(x, overwrite = FALSE) <- "Should Not Overwrite"
  }, regexp = "^The dataset has already a description")
  expect_equal(description(x), "Description 1")

  # Explicit overwrite works
  description(x, overwrite = TRUE) <- "Description 2"
  expect_equal(description(x), "Description 2")
})

test_that("description() <- NULL sets to ':unas'", {
  x <- orange_df
  description(x, overwrite = TRUE) <- NULL
  expect_equal(description(x), ":unas")
})

test_that("description() <- works when no initial description exists", {
  # Strip description from copy of orange_df
  x <- orange_df
  attr(x, "dataset_bibentry")$description <- NULL
  expect_equal(length(description(x)), 0)

  # Assign a new one without error
  description(x, overwrite = TRUE) <- "Fresh Description"
  expect_equal(description(x), "Fresh Description")
})

test_that("description() fails on non-dataset_df", {
  expect_error(description(mtcars), "x must be a dataset object")
  expect_error(description(mtcars, overwrite = TRUE) <- "Bad", "x must be a dataset object")
})
