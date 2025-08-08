test_that("creator() retrieves the current creator from dataset_df", {
  df <- dataset_df(data.frame(x = 1:3))
  creator(df) <- person("Alice", "Smith", role = "aut")
  result <- creator(df)
  expect_s3_class(result, "person")
  expect_equal(as.character(result), "Alice Smith [aut]")
})

test_that("creator<-() sets a new creator", {
  df <- dataset_df(data.frame(x = 1:3))
  creator(df) <- person("Jane", "Doe", role = "aut")
  expect_equal(as.character(creator(df)), "Jane Doe [aut]")
})

test_that("creator<-() appends new creator when overwrite = FALSE", {
  df <- dataset_df(data.frame(x = 1:3))
  creator(df) <- person("Alice", "Smith", role = "aut")
  creator(df, overwrite = FALSE) <- person("Bob", "Jones", role = "ctb")
  names <- vapply(creator(df), as.character, character(1))
  expect_true("Alice Smith [aut]" %in% names)
  expect_true("Bob Jones [ctb]" %in% names)
  creator(df, overwrite = FALSE) <- person("Bob", "Jones", role = "aut")
  names2 <- vapply(creator(df), as.character, character(1))
  expect_true("Bob Jones [aut]" %in% names2)
  creator(df, overwrite = TRUE) <- person("Joe", "Doe", role = "aut")
  names3 <- vapply(creator(df), as.character, character(1))
  expect_false("Bob Jones [aut]" %in% names3)
  expect_true("Joe Doe [aut]" %in% names3)
})

test_that("creator<-() does not change anything when value is NULL", {
  df <- dataset_df(data.frame(x = 1:3))
  creator(df) <- person("Test", "User", role = "aut")

  creator(df) <- NULL # This should return `df` unchanged
  expect_equal(as.character(creator(df)), "Test User [aut]")
})

test_that("creator<-() errors when value is not a person object", {
  df <- dataset_df(data.frame(x = 1:3))
  expect_error(
    creator(df) <- "Jane Doe",
    regexp = "value` must be a utils::person object"
  )
})

test_that("creator() errors on non-dataset_df input", {
  x <- data.frame(x = 1:3)
  expect_error(
    creator(x),
    regexp = "creator\\(x\\): x must be a dataset object"
  )
})

test_that("creator<-() errors on non-dataset_df input", {
  x <- data.frame(x = 1:3)
  expect_error(
    creator(x) <- person("Fake", "User", role = "aut"),
    regexp = "creator\\(x\\) <- value: x must be a dataset object"
  )
})
