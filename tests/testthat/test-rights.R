test_that("rights() retrieves rights from dataset_df", {
  df <- dataset_df(x = 1:3)
  rights(df, overwrite=TRUE) <- "CC-BY-4.0"
  expect_equal(rights(df), "CC-BY-4.0")
})

test_that("rights() errors for non-dataset_df object", {
  df <- data.frame(x = 1:3)
  expect_error(
    rights(df),
    "rights\\(x\\): x must be a dataset object"
  )
})

test_that("rights<- assigns rights when none exists", {
  df <- dataset_df(x = 1:3)
  expect_silent(rights(df, overwrite=TRUE) <- "GPL-3")
  expect_equal(rights(df), "GPL-3")
})

test_that("rights<- assigns rights if current is ':unas'", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry")$rights <- ":unas"
  rights(df) <- "MIT"
  expect_equal(rights(df), "MIT")
})

test_that("rights<- emits message if overwrite = FALSE and field exists", {
  df <- dataset_df(x = 1:3)
  rights(df, overwrite=TRUE) <- "Original"
  expect_message(
    rights(df, overwrite = FALSE) <- "Attempted overwrite",
    "already a rights field"
  )
  expect_equal(rights(df), "Original")
})

test_that("rights<- overwrites if overwrite = TRUE", {
  df <- dataset_df(x = 1:3)
  rights(df) <- "Old License"
  rights(df, overwrite = TRUE) <- "New License"
  expect_equal(rights(df), "New License")
})

test_that("rights<- errors if value is not length 1", {
  df <- dataset_df(x = 1:3)
  expect_error(
    rights(df) <- c("A", "B"),
    regexp = "must have length=1"
  )
})


test_that("rights<- sets :unas when value is NULL", {
  df <- dataset_df(x = 1:3)
  rights(df) <- NULL
  expect_equal(rights(df), ":unas")
})
