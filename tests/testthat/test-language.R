test_that("language() works", {
  expect_equal(language(iris_dataset), "en")
})


iris_dataset_2 <- iris_dataset
value <- "fr"

language(x=iris_dataset_2, iso_639_code  = "639-2") <- "fr"
language(iris_dataset_2)

test_that("language() with two letter code works", {
  expect_equal(language(iris_dataset_2),  "fra")
})

language(iris_dataset_2) <- NULL

test_that("language() can returns :unas value", {
  expect_equal(language(iris_dataset_2), ":unas")
})

test_that("language() throws error", {
  expect_error(language(iris_dataset) <- "xxx")
})

