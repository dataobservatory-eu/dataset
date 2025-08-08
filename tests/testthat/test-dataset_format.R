test_that("dataset_format() returns default when unset", {
  df <- dataset_df(data.frame(x = 1:3))
  # Remove format to simulate unset
  be <- get_bibentry(df)
  be$format <- NULL
  attr(df, "dataset_bibentry") <- be

  expect_equal(dataset_format(df), "application/r-rds")
})

test_that("dataset_format() returns current value when set", {
  df <- dataset_df(data.frame(x = 1:3))
  be <- get_bibentry(df)
  be$format <- "text/csv"
  attr(df, "dataset_bibentry") <- be

  expect_equal(dataset_format(df), "text/csv")
})

test_that("dataset_format<- sets value when default or unset", {
  df <- dataset_df(data.frame(x = 1:3))
  dataset_format(df) <- "text/csv"
  expect_equal(dataset_format(df), "text/csv")
})

test_that("dataset_format<- resets to default on NULL", {
  df <- dataset_df(data.frame(x = 1:3))
  dataset_format(df) <- "text/csv"
  dataset_format(df) <- NULL
  expect_equal(dataset_format(df), "application/r-rds")
})

test_that("dataset_format<- respects overwrite = FALSE", {
  df <- dataset_df(data.frame(x = 1:3))
  dataset_format(df) <- "text/csv"

  expect_message(
    dataset_format(df) <- "application/parquet",
    "already has a format"
  )
  expect_equal(dataset_format(df), "text/csv")
})

test_that("dataset_format<- overwrites when overwrite = TRUE", {
  df <- dataset_df(data.frame(x = 1:3))
  dataset_format(df) <- "text/csv"
  dataset_format(df, overwrite = TRUE) <- "application/parquet"
  expect_equal(dataset_format(df), "application/parquet")
})

test_that("dataset_format() errors for non-dataset_df", {
  expect_error(
    dataset_format(mtcars),
    "must be a dataset object created with dataset_df"
  )
})

test_that("dataset_format<- errors for invalid value length", {
  df <- dataset_df(data.frame(x = 1:3))
  expect_error(
    dataset_format(df) <- c("a", "b"),
    "must be length 1 or NULL"
  )
})
