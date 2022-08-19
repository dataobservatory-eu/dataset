iris_dataset <- iris
language(iris_dataset) <- "English"

test_that("language() works", {
  expect_equal(language(iris_dataset), "eng")
})

language(x=iris_dataset, iso_639_code  = "639-2") <- "fr"

test_that("language() with two letter code works", {
  expect_equal(language(iris_dataset),  "fra")
})

language(iris_dataset) <- NULL

test_that("language() can return NULL value", {
  expect_true(is.null(language(iris_dataset)))
})

test_that("language() throws error", {
  expect_error(language(iris_dataset) <- "xxx")
})

