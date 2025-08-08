test_that("language() sets and retrieves ISO 639-3 language code", {
  df <- dataset_df(data.frame(x = 1:3))
  language(df) <- "en"
  expect_equal(language(df), "eng")
})

test_that("language() sets and retrieves ISO 639-1 code", {
  df <- dataset_df(data.frame(x = 1:3))
  language(df, iso_639_code = "639-1") <- "eng"
  expect_equal(language(df), "en")
})

test_that("language() sets and retrieves ISO 639-1
          code with multiple hits", {
  df <- dataset_df(data.frame(x = 1:3))
  language(df, iso_639_code = "639-3") <- "English, Old"
  expect_equal(language(df), "ang")
})

test_that("language() accepts full language names", {
  df <- dataset_df(data.frame(x = 1:3))
  language(df) <- "French"
  expect_equal(language(df), "fra") # 639-3 default
})

test_that("language() is case-insensitive for names and codes", {
  df <- dataset_df(data.frame(x = 1:3))
  language(df) <- "SpaniSH"
  expect_equal(language(df), "spa")
})

test_that("language() throws error on unknown language code", {
  df <- dataset_df(data.frame(x = 1:3))
  expect_error(
    language(df) <- "xx",
    regexp = "is not a valid ISO 639 language code"
  )
})

test_that("language() throws error on unknown full name", {
  df <- dataset_df(data.frame(x = 1:3))
  expect_error(
    language(df) <- "Elvish",
    regexp = "is not a valid ISO 639 language code"
  )
})

test_that("language() rejects non-dataset_df input", {
  df <- data.frame(x = 1:3)
  expect_error(
    language(df),
    regexp = "language\\(x\\): x must be a dataset object"
  )
  expect_error(
    language(df) <- "en",
    regexp = "language\\(x\\)<- value: x must be a dataset object"
  )
})
