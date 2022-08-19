iris_dataset <- iris
description(iris_dataset) <- "The famous iris dataset used in R language examples."

test_that("description() works", {
  expect_equal(description(iris_dataset), "The famous iris dataset used in R language examples.")
  expect_message(description(iris_dataset, overwrite=F) <- "Overwritten.")
})

description(iris_dataset) <- "Overwritten."

test_that("description() overwrites by default", {
  expect_equal(description(iris_dataset), "Overwritten.")
})

