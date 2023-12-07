test_that("description() <- assignment works", {
  expect_equal(description(iris_dataset), "The famous (Fisher's or Anderson's) iris data set.")
  expect_warning(description(iris_dataset, overwrite=F) <- "Overwritten.")
})

description(iris_dataset, TRUE) <- "Overwritten."

test_that("description() works", {
  expect_equal(description(iris_dataset), "Overwritten.")
})

description(iris_dataset, TRUE) <- NULL

test_that("description() works", {
  expect_equal(description(iris_dataset), ":unas")
})
