iris_dataset <- iris
version(iris_dataset) <- "1.0"

test_that("multiplication works", {
  expect_equal(version(iris_dataset), "1.0")
})
