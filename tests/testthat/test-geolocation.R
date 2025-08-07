test_that("geolocation() returns NULL if not set", {
  df <- data.frame(a = 1:3)
  expect_null(geolocation(df))
})

test_that("geolocation() returns the value after being set", {
  df <- data.frame(b = 1:3)
  geolocation(df) <- "US"
  expect_equal(geolocation(df), "US")
})

test_that("geolocation()<- sets NA when value is NULL and no existing attribute", {
  df <- data.frame(c = 1:3)
  geolocation(df) <- NULL
  expect_true(is.na(geolocation(df)))
})

test_that("geolocation()<- overwrites existing attribute when overwrite = TRUE", {
  df <- data.frame(d = 1:3)
  geolocation(df) <- "US"
  geolocation(df, overwrite = TRUE) <- "GB"
  expect_equal(geolocation(df), "GB")
})

test_that("geolocation()<- does not overwrite when overwrite = FALSE", {
  df <- data.frame(e = 1:3)
  geolocation(df) <- "US"
  expect_message(
    geolocation(df, overwrite = FALSE) <- "FR",
    regexp = "The dataset has already an Geolocation"
  )
  expect_equal(geolocation(df), "US")
})

test_that("geolocation()<- sets value when attribute exists but is NULL", {
  df <- data.frame(f = 1:3)
  attr(df, "Geolocation") <- NULL
  geolocation(df) <- "IT"
  expect_equal(geolocation(df), "IT")
})

test_that("geolocation() works on dataset_df object", {
  skip_if_not(exists("dataset_df"), message = "dataset_df() not defined")
  df <- dataset_df(data.frame(g = 1:3))
  geolocation(df) <- "CA"
  expect_equal(geolocation(df), "CA")
})


