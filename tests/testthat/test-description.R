description(x=iris_dataset) <- "The famous iris dataset used in R language examples."

test_that("description() <- assignment works", {
  expect_equal(description(iris_dataset), "The famous iris dataset used in R language examples.")
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
