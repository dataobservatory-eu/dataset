iris_dataset <- iris
version(iris_dataset) <- "1.0"

test_that("version() works", {
  expect_equal(version(iris_dataset), "1.0")
  expect_message(version(iris_dataset, overwrite=F) <- "2.0")
})
