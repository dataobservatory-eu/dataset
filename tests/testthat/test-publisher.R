test_that("publisher() retrieves publisher from dataset_df", {
  df <- dataset_df(x = 1:3)
  publisher(df) <- "Open Science Repository"
  expect_equal(publisher(df), "Open Science Repository")
})

test_that("publisher() errors if object is not a dataset_df", {
  regular_df <- data.frame(x = 1:3)
  expect_error(
    publisher(regular_df),
    "publisher\\(x\\): x must be a dataset object"
  )
})

test_that("publisher<- assigns publisher if none exists", {
  df <- dataset_df(x = 1:3)
  expect_silent(publisher(df) <- "Data Publisher Inc.")
  expect_equal(publisher(df), "Data Publisher Inc.")
})

test_that("publisher<- assigns publisher if current is :tba", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry")$publisher <- ":tba"
  publisher(df) <- "Example Press"
  expect_equal(publisher(df), "Example Press")
})

test_that("publisher<- does not overwrite by default if set", {
  df <- dataset_df(x = 1:3)
  publisher(df) <- "Original Publisher"
  expect_message(publisher(df, overwrite = FALSE) <- "New Publisher",
    regexp = "dataset has already an Publisher"
  )
  expect_equal(publisher(df), "Original Publisher")
})

test_that("publisher<- does overwrite if overwrite = TRUE", {
  df <- dataset_df(x = 1:3)
  publisher(df) <- "Old Publisher"
  publisher(df, overwrite = TRUE) <- "New Publisher"
  expect_equal(publisher(df), "New Publisher")
})

test_that("publisher<- errors with invalid value length", {
  df <- dataset_df(x = 1:3)
  expect_error(
    publisher(df) <- c("a", "b"),
    "value must be of length 1"
  )
})

test_that("publisher<- sets to :tba when value is NULL", {
  df <- dataset_df(x = 1:3)
  publisher(df) <- NULL
  expect_equal(publisher(df), ":tba")
})
