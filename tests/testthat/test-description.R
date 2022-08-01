iris_dataset <- iris
description(iris_dataset) <- "The famous iris dataset used in R language examples."

test_that("description() works", {
  expect_equal(description(iris_dataset), "The famous iris dataset used in R language examples.")
})
