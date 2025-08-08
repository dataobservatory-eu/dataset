test_that("publication_year() gets the publication year correctly", {
  df <- dataset_df(data.frame(a = 1:3))
  publication_year(df) <- "2020"
  expect_equal(publication_year(df), "2020")
})

test_that("publication_year() returns character", {
  df <- dataset_df(data.frame(b = 1:3))
  publication_year(df) <- "1999"
  expect_type(publication_year(df), "character")
})

test_that("publication_year()<- sets the year when no existing value", {
  df <- dataset_df(data.frame(c = 1:3))
  publication_year(df) <- "2015"
  expect_equal(publication_year(df), "2015")
})

test_that("publication_year()<- overwrites when overwrite = TRUE", {
  df <- dataset_df(data.frame(d = 1:3))
  publication_year(df) <- "2010"
  publication_year(df, overwrite = TRUE) <- "2022"
  expect_equal(publication_year(df), "2022")
})

test_that("publication_year()<- does not overwrite when overwrite = FALSE", {
  df <- dataset_df(data.frame(e = 1:3))
  publication_year(df) <- "2001"
  expect_warning(
    publication_year(df, overwrite = FALSE) <- "2005",
    regexp = "The dataset has already an publication_year"
  )
  expect_equal(publication_year(df), "2001") # Should not change
})

test_that("publication_year()<- handles NULL value by setting ':unas'", {
  df <- dataset_df(data.frame(f = 1:3))
  publication_year(df) <- NULL
  expect_equal(publication_year(df), ":unas")
})

test_that("publication_year() fails on non-dataset_df input", {
  df <- data.frame(x = 1:3)
  expect_error(publication_year(df),
    regexp = "publication_year\\(x\\): x must be a dataset object"
  )
  expect_error(publication_year(df) <- "2020",
    regexp = "publication_year\\(x\\) <- value: x must be a dataset object"
  )
})
