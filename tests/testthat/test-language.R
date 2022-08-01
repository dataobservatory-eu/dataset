iris_dataset <- iris
language(iris_dataset) <- "English"

test_that("language() works", {
  expect_equal(language(iris_dataset), "eng")
})

language(iris_dataset) <- NULL

test_that("language() can return NULL value", {
  expect_true(is.null(language(iris_dataset)))
})
